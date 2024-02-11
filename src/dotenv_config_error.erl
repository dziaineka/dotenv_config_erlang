-module(dotenv_config_error).
-export([to_binary/1]).

-spec to_binary(any()) -> binary().
to_binary(Error) ->
    CharError = io_lib:format("~p", [Error]),
    unicode:characters_to_binary(CharError).
