# katana-test

# ⚠ THIS REPO IS ARCHIVED ⚠️

Instead of using this tool, consider adding a `test` alias to your `rebar.config`, like this:

```erlang
{alias,
 [{test, [compile, format, lint, hank, dialyzer, {ct, "--verbose"}, cover, edoc]}]}.
```

That way, you can run `rebar3 test` on your CI pipelines and get the code checked with all the desirable tools/plugins at once.

<details><summary>Old Readme</summary>
Katana Test is an Erlang library application containing modules useful for testing Erlang systems.
It currently contains the module `ktn_meta_SUITE`, which is a Common Test suite to be included among your tests. Read below for more information.

# Contact Us

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/katana-test/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at
[inaka.github.io](http://inaka.github.io)

### `ktn_meta_SUITE`

#### Goals
The **meta** suite lets you check your code with `dialyzer`, `xref` and `elvis`.

#### Usage
To include the suite in your project, you only need to invoke its functions from a common_test suite. If you use [mixer](https://github.com/inaka/mixer) you can do…

```erlang
-module(your_meta_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([ktn_meta_SUITE]).

-export([init_per_suite/1, end_per_suite/1]).

init_per_suite(Config) -> [{application, your_app} | Config].
end_per_suite(_) -> ok.
```

Of course, you can choose what functions to include, for example if you want `dialyzer` but not `elvis` nor `xref` you can do…

```erlang
-mixin([{ ktn_meta_SUITE
        , [ dialyzer/1
          ]
        }]).

-export([all/0]).

all() -> [dialyzer].
```

#### Configuration
`ktn_meta_SUITE` uses the following configuration parameters that you can add to the common_test config for your test suite:

| Parameter | Description | Default |
|-----------|-------------|---------|
| `base_dir` | The base_dir for your app | `code:lib_dir(App)` where `App` is what you define in `application` below |
| `application` | The name of your app | **no default** |
| `dialyzer_warnings` | The active warnings for _diaylzer_ | `[error_handling, race_conditions, unmatched_returns, unknown]` |
| `plts` | The list of plt files for _dialyzer_ | `filelib:wildcard("your_app/*.plt")` |
| `elvis_config` | Config file for _elvis_ | `"your_app/elvis.config"` |
| `xref_config` | Config options for _xref_ | `#{dirs => [filename:join(BaseDir, "ebin"), filename:join(BaseDir, "test")], xref_defaults => [{verbose, true}, {recurse, true}, {builtins, true}]}` |
| `xref_checks` | List of checks for _xref_ | `[ undefined_function_calls, locals_not_used, deprecated_function_calls]` |
| `dirs` | List of folders to check with _xref_ and _dialyzer_ | `["ebin", "test"]` |
</details>
