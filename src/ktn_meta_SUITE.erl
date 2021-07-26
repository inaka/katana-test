%%% @doc Meta Testing SUITE
%%% Use with mixer or by yourself. Just include a call to each of its functions
%%% in your common test suites.
%%% Make sure to add an application property to your common test configuration.
-module(ktn_meta_SUITE).
-author('elbrujohalcon@inaka.net').

-export([all/0]).
-export([xref/1, dialyzer/1, elvis/1]).
-ignore_xref([all/0]).
-ignore_xref([xref/1, dialyzer/1, elvis/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [dialyzer | elvis | xref, ...].
all() -> [dialyzer, elvis, xref].

%% @doc xref's your code using xref_runner.
%%      Available Options:
%%      - xref_config: Configuration for xref_runner
%%      - xref_checks: List of xref checks to perform
-spec xref(config()) -> {comment, []}.
xref(Config) ->
  XrefConfig =
    case test_server:lookup_config(xref_config, Config) of
      undefined ->
        #{ dirs => dirs(Config)
         , xref_defaults => [ {verbose, true}
                            , {recurse, true}
                            , {builtins, true}
                            ]
         };
      XC -> XC
    end,
  Checks =
    case test_server:lookup_config(xref_checks, Config) of
      undefined ->
        [ undefined_function_calls
        , locals_not_used
        , deprecated_function_calls
        ];
      Cs -> Cs
    end,

  ct:comment("There are no Warnings"),
  [] =
    [ Warning
    || Check <- Checks, Warning <- xref_runner:check(Check, XrefConfig)],

  {comment, ""}.

%% @doc dialyzes your code.
%%      By default it uses all the plts in the project root folder.
%%      You can change that by providing a 'plts' parameter in Config.
%%      You can also change the warnings using the 'dialyzer_warnings' parameter
-spec dialyzer(config()) -> {comment, []}.
dialyzer(Config) ->
  Plts = plts(Config),
  Dirs = dirs(Config),
  Warnings =
    case test_server:lookup_config(dialyzer_warnings, Config) of
      undefined -> [ error_handling
                   , race_conditions
                   , unmatched_returns
                   ];
      Ws -> Ws
    end,

  ct:comment("Dialyzer must emit no warnings"),
  Opts =
    [ {analysis_type, succ_typings}
    , {plts,          Plts}
    , {files_rec,     Dirs}
    , {check_plt,     true}
    , {warnings,      Warnings}
    , {get_warnings,  true}
    ],
  [] = [dialyzer:format_warning(W, basename) || W <- dialyzer:run(Opts)],
  {comment, ""}.

%% @doc Checks your code with elvis
%%      Available Options:
%%      - elvis_config: Location of elvis.config
-spec elvis(config()) -> {comment, []}.
elvis(Config) ->
  ElvisConfig =
    case test_server:lookup_config(elvis_config, Config) of
      undefined ->
        BaseDir = base_dir(Config),
        ConfigFile =
          case is_rebar3_project(Config) of
            true  -> filename:join(BaseDir, "../../../../elvis.config");
            false -> filename:join(BaseDir, "elvis.config")
          end,
        [ fix_dirs(Group, Config)
        || Group <- elvis_config:from_file(ConfigFile)];
      ConfigFile -> elvis_config:from_file(ConfigFile)
    end,

  ct:comment("Elvis rocks!"),
  case elvis_core:rock(ElvisConfig) of
    ok -> ok;
    {fail, Results} ->
      Errors = [format_elvis_error(R)
                    || #{rules := Rules} = R <- Results, Rules /= []],
      ct:fail({elvis_failed, Errors})
  end,
  {comment, ""}.

format_elvis_error(Error) ->
  #{file := Path, rules := Rules} = Error,
  #{file => Path, problems => lists:flatten([format_elvis_message(R)
                                                 || R <- Rules])}.

format_elvis_message(Rule) ->
  #{name := Name, items := Items} = Rule,
  [#{message => lists:flatten(io_lib:format(Message, Info)),
     line => LineNum,
     rule => Name}
    || #{info := Info, message := Message, line_num := LineNum} <- Items].

base_dir(Config) ->
  case test_server:lookup_config(base_dir, Config) of
    undefined ->
      case test_server:lookup_config(application, Config) of
        undefined ->
          ct:fail("Missing base_dir and application in Config: ~p", [Config]);
        App -> code:lib_dir(App)
      end;
    BaseDir -> BaseDir
  end.

plts(Config) ->
  case test_server:lookup_config(plts, Config) of
    undefined ->
      BaseDir = base_dir(Config),
      case plts(BaseDir, is_rebar3_project(Config)) of
        {[], Dirs} ->
          ExpandedDirs = [normalize_path(Dir) || Dir <- Dirs],
          DirsStr = string:join(ExpandedDirs, " "),
          ct:fail("No plts found in: ~s - you need to at least have one",
                  [DirsStr]);
        {Plts, _} -> Plts
      end;
    Plts -> Plts
  end.

plts(BaseDir, false) -> % not rebar3
  {filelib:wildcard(filename:join(BaseDir, "*.plt")), BaseDir};
plts(BaseDir, true) ->
  TestProfileDir = filename:join(BaseDir, "../../../test"),
  case filelib:wildcard(filename:join(TestProfileDir, "*_plt")) of
    [] ->
      DefaultProfileDir = filename:join(BaseDir, "../../../default"),
      case filelib:wildcard(filename:join(DefaultProfileDir, "*_plt")) of
        [] ->
          % Last resort: if there are no plts in test or default, maybe
          % our user has some other profile where the plt is generated
          AllProfileDirs = filename:join(BaseDir, "../../../*"),
          {filelib:wildcard(filename:join(AllProfileDirs, "*_plt")),
           [TestProfileDir, DefaultProfileDir, AllProfileDirs]};
        DefaultPlts -> {DefaultPlts, []}
      end;
    TestPlts -> {TestPlts, []}
  end.

fix_dirs(#{dirs := Dirs} = Group, Config) ->
  NewDirs =
    [filename:join(base_dir(Config), Dir) || Dir <- Dirs],
  Group#{dirs := NewDirs}.

dirs(Config) ->
  BaseDir = base_dir(Config),
  Dirs =
    case test_server:lookup_config(dirs, Config) of
      undefined -> ["ebin", "test"];
      Directories -> Directories
    end,
  [filename:join(BaseDir, Dir) || Dir <- Dirs].

is_rebar3_project(Config) ->
  BaseDir = base_dir(Config),
  BaseDirBin = list_to_binary(BaseDir),
  % rebar3 projects has a `_build' folder for its profiles.
  case re:split(BaseDir, "_build") of
    % If there is just one result, it means `build' wasn't found in the path
    [BaseDirBin] -> false;
    % If there is at least two results, it means `build' was found in the
    % path, so we are dealing with a rebar3 project.
    [_Build, _Found | _] -> true
  end.

normalize_path(Path) ->
  filename:join(normalize_path1(filename:split(Path), [])).

normalize_path1([], Acc) ->
  lists:reverse(Acc);

normalize_path1([".."|Rest], Acc) ->
  normalize_path1(Rest, tl(Acc));

normalize_path1([D1|Rest], Acc) ->
  normalize_path1(Rest, [D1|Acc]).
