-module(dotenv_config_parser).

-include("dotenv_config.hrl").

-callback get_parser() -> parser().

-export([parse_file/1, parse_config/2]).

-spec parse_file(file:name_all()) -> {ok, parsed_config_file_raw()} | {error, binary()}.
parse_file(FileName) ->
    case file:read_file(FileName) of
        {ok, FileContent} ->
            parse_file_content(FileContent);
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_file_content(binary()) -> {ok, parsed_config_file_raw()} | {error, binary()}.
parse_file_content(FileContent) ->
    Lines = binary:split(FileContent, <<"\n">>, [global, trim_all]),
    ParsedConfigFileRaw = lists:foldl(
        fun(Line, ParsedLines) ->
            case parse_line(Line) of
                {ok, {ConfigItemName, ConfigItemRawValue}} ->
                    maps:put(ConfigItemName, ConfigItemRawValue, ParsedLines);
                error ->
                    exit(<<"Can't parse this line from doteenv config: ", Line/binary>>)
            end
        end,
        #{},
        Lines
    ),
    {ok, ParsedConfigFileRaw}.

-spec parse_line(binary()) -> {ok, {config_item_name(), config_item_raw_value()}} | error.
parse_line(Line) ->
    case binary:split(Line, <<"=">>, [trim_all]) of
        [Key, Value] ->
            {ok, {string:trim(Key), string:trim(Value)}};
        _ ->
            error
    end.

-spec parse_config(parsed_config_file_raw(), module()) ->
    {ok, parsed_config_file()} | {error, binary()}.
parse_config(Config, Module) ->
    ConfigItemsParsers = Module:get_parser(),

    ParsedConfig = lists:foldl(
        fun({ConfigItemName, Parser}, ParsedConfigAcc) ->
            DefaultValue = get_default_value(ConfigItemName),
            case maps:get(ConfigItemName, Config, DefaultValue) of
                not_found ->
                    exit(<<"Can't find config item: ", ConfigItemName/binary>>);
                already_stored ->
                    ParsedConfigAcc;
                ConfigItemRawValue ->
                    case parse_config_item(ConfigItemRawValue, Parser) of
                        {ok, ConfigItemValue} ->
                            [{ConfigItemName, ConfigItemValue} | ParsedConfigAcc];
                        error ->
                            BinParser = io_lib:format("~p", [Parser]),
                            exit(
                                <<"Can't parse config item: ", ConfigItemName/binary, " of value: ",
                                    ConfigItemRawValue/binary, " to type: ", BinParser/binary>>
                            )
                    end
            end
        end,
        [],
        ConfigItemsParsers
    ),
    {ok, ParsedConfig}.

-spec get_default_value(config_item_name()) -> config_item_raw_value().
get_default_value(ConfigItemName) ->
    MaybeEnvironmentValue = get_environment_variable(ConfigItemName),
    IsAlreadyStored = is_value_already_stored(ConfigItemName),
    case {MaybeEnvironmentValue, IsAlreadyStored} of
        {not_found, false} ->
            not_found;
        {EnvironmentValue, false} ->
            EnvironmentValue;
        {_, true} ->
            already_stored
    end.

-spec get_environment_variable(config_item_name()) -> config_item_raw_value() | not_found.
get_environment_variable(ConfigItemName) ->
    case os:getenv(binary_to_list(ConfigItemName)) of
        false ->
            not_found;
        EnvironmentValue ->
            list_to_binary(EnvironmentValue)
    end.

-spec is_value_already_stored(config_item_name()) -> boolean().
is_value_already_stored(ConfigItemName) ->
    case dotenv_config_storage:get(ConfigItemName) of
        {error, not_found} ->
            false;
        {ok, _StoredValue} ->
            true
    end.

-spec parse_config_item(config_item_raw_value(), config_item_type()) ->
    {ok, config_item_value()} | error.
parse_config_item(ConfigItemRawValue, str) ->
    {ok, ConfigItemRawValue};
parse_config_item(ConfigItemRawValue, int) ->
    case string:to_integer(ConfigItemRawValue) of
        {IntValue, _Rest} ->
            {ok, IntValue};
        _ ->
            error
    end;
parse_config_item(ConfigItemRawValue, bool) ->
    try binary_to_existing_atom(ConfigItemRawValue) of
        true -> {ok, true};
        false -> {ok, false}
    catch
        _:_ ->
            error
    end;
parse_config_item(ConfigItemRawValue, json) ->
    try jiffy:decode(ConfigItemRawValue) of
        JsonValue ->
            {ok, JsonValue}
    catch
        _:_ ->
            error
    end;
parse_config_item(ConfigItemRawValue, atom) ->
    try binary_to_atom(ConfigItemRawValue) of
        AtomValue ->
            {ok, AtomValue}
    catch
        _:_ ->
            error
    end;
parse_config_item(ConfigItemRawValue, module) ->
    try binary_to_existing_atom(ConfigItemRawValue) of
        ModuleValue ->
            case code:ensure_loaded(ModuleValue) of
                {module, ModuleValue} ->
                    {ok, ModuleValue};
                _ ->
                    error
            end
    catch
        _:_ ->
            error
    end;
parse_config_item(ConfigItemRawValue, charlist) ->
    {ok, binary_to_list(ConfigItemRawValue)}.
