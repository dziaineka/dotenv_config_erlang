-module(parser_module_sample).
-behaviour(dotenv_config_parser).

-export([get_parser/0]).

-spec get_parser() -> dotenv_config:parser().
get_parser() ->
    [
        {<<"CLIENT_ID">>, str},
        {<<"PORT">>, int},
        {<<"DEBUG">>, bool},
        {<<"NAMES">>, json},
        {<<"LOG_LEVEL">>, atom},
        {<<"CALLBACK_MODULE">>, module},
        {<<"CHARLIST">>, charlist},
        {<<"SOME_LIST_MODE">>, [{exact, <<"allowlist">>}, {exact, <<"blocklist">>}]},
        {<<"SOME_CALL_TIMEOUT">>, [{exact, <<"infinity">>}, int]},
        {<<"ANOTHER_CALL_TIMEOUT">>, [{exact, <<"infinity">>}, int]},
        {<<"SOME_COMPLEX_TYPE">>, fun (RawValue) -> <<RawValue/binary, "_42">> end},
        {<<"JSON_OBJECT">>, json}
    ].
