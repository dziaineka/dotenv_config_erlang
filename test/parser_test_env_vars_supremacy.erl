-module(parser_test_env_vars_supremacy).
-behaviour(dotenv_config_parser).

-export([get_parser/0]).

-spec get_parser() -> dotenv_config:parser().
get_parser() ->
    [
        {<<"VALUE_TO_OVERRIDE">>, str},
        {<<"PORT">>, int}
    ].
