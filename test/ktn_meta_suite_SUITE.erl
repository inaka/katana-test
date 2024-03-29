-module(ktn_meta_suite_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ ktn_meta_SUITE
        , [ all/0
          , xref/1
          , dialyzer/1
          , elvis/1
          ]
        }]).

-export([init_per_suite/1, end_per_suite/1]).

-type config() :: [{atom(), term()}].

-if(?OTP_RELEASE >= 23).
-behaviour(ct_suite).
-endif.

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  [{application, katana_test}| Config].

-spec end_per_suite(config()) -> term() | {save_config, config()}.
end_per_suite(_) ->
  ok.
