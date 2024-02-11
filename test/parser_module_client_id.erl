-module(parser_module_client_id).
-behaviour(dotenv_config_parser).

-export([get_parser/0]).

-spec get_parser() -> dotenv_config:parser().
get_parser() ->
    [
        {<<"CLIENT_ID">>, str}
    ].
