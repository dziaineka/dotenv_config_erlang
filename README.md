# Dotenv Config

Erlang apps config based on environment variables and .env files

## Usage

At the start of your application, load the .env files (later files will overwrite the previous ones values).

```erlang
dotenv_config:init(parser_module, ["path/to/.env"]).
```

`parser_module` is a module that implements the `dotenv_config_parser` behaviour. It should have a `get_parser/0` function that returns a list of
`{ConfigItemName, ConfigItemType}` tuples. See the `parser_module` example below.

Get the config value:

```erlang
dotenv_config:get(<<"CLIENT_ID">>).
```

Set the config value:

```erlang
dotenv_config:set(<<"CLIENT_ID">>, <<"new_client_id">>).
```

Get all the config values:

```erlang
dotenv_config:get_all().
```

## `parser_module` example

```erlang
-module(client_config_example).
-behaviour(dotenv_config_parser).

-export([get_parser/0]).

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
        {<<"SOME_COMPLEX_TYPE">>, fun (RawValue) -> <<RawValue/binary, "_42">> end}
    ].
```

`json` type would be parsed to list or map using `jiffy:decode(RawValue, [return_maps]).`

`module` type would be checked for existence.
