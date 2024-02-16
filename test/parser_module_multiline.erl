-module(parser_module_multiline).
-behaviour(dotenv_config_parser).

-export([get_parser/0]).

-spec get_parser() -> dotenv_config:parser().
get_parser() ->
    [
        {<<"SINGLELINE">>, str},
        {<<"MULTILINE">>, str}
    ].
