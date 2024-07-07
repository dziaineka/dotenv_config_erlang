-module(dotenv_config_parser).

-include("dotenv_config.hrl").

-callback get_parser() -> parser().

-ifdef(EUNIT).
-compile(export_all).
-endif.

-export([parse_file/1, parse_config/2]).
-export_type([parser/0, parsed_config/0, parsed_config_raw/0]).

-spec parse_file(file:name_all()) -> {ok, parsed_config_raw()} | {error, any()}.
parse_file(FileName) ->
    case file:read_file(FileName) of
        {ok, FileContent} ->
            parse_file_content(FileContent);
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_file_content(binary()) -> {ok, parsed_config_raw()} | {error, any()}.
parse_file_content(FileContent) ->
    Lines = binary:split(FileContent, <<"\n">>, [global, trim_all]),
    {_, ParsedConfigFileRaw} = lists:foldl(
        fun
            (Line, {{multi_line, {ConfigItemName, ParsedHead}}, ParsedLines}) ->
                case parse_multi_line(Line, ParsedHead) of
                    {ok, end_multi_line} ->
                        {single_line, maps:put(ConfigItemName, ParsedHead, ParsedLines)};
                    {ok, ParsedHead1} ->
                        {{multi_line, {ConfigItemName, ParsedHead1}}, ParsedLines}
                end;
            (Line, {single_line, ParsedLines}) ->
                case parse_single_line(Line) of
                    {ok, {ConfigItemName, multi_line_start}} ->
                        {{multi_line, {ConfigItemName, <<>>}}, ParsedLines};
                    {ok, {ConfigItemName, ConfigItemRawValue}} ->
                        {single_line, maps:put(ConfigItemName, ConfigItemRawValue, ParsedLines)};
                    error ->
                        exit(<<"Can't parse this line from doteenv config: ", Line/binary>>)
                end
        end,
        {single_line, #{}},
        Lines
    ),
    {ok, ParsedConfigFileRaw}.

-spec parse_multi_line(binary(), config_item_raw_value()) ->
    {ok, config_item_raw_value()} | {ok, end_multi_line}.
parse_multi_line(Line, ParsedHead) ->
    case {ParsedHead, string:trim(Line)} of
        {_, <<"\"\"\"">>} ->
            {ok, end_multi_line};
        {<<>>, _} ->
            {ok, <<Line/binary>>};
        {ParsedHead, _} ->
            {ok, <<ParsedHead/binary, "\n", Line/binary>>}
    end.

-spec parse_single_line(binary()) ->
    {ok, {config_item_name(), config_item_raw_value()}}
    | {ok, {config_item_name(), multi_line_start}}
    | error.
parse_single_line(Line) ->
    case binary:split(Line, <<"=">>, [trim_all]) of
        [Key, <<"\"\"\"">>] ->
            {ok, {string:trim(Key), multi_line_start}};
        [Key, Value] ->
            {ok, {string:trim(Key), string:trim(Value)}};
        _ ->
            error
    end.

-spec parse_config(parsed_config_raw(), module()) -> {ok, parsed_config()}.
parse_config(Config, Module) ->
    ConfigItemsParsers = Module:get_parser(),

    ParsedConfig = lists:foldl(
        fun({ConfigItemName, Parser}, ParsedConfigAcc) ->
            DefaultValue = get_default_value(Config, ConfigItemName),

            case get_environment_variable(ConfigItemName, DefaultValue) of
                not_found ->
                    exit(<<"Can't find config item: ", ConfigItemName/binary>>);
                already_stored ->
                    ParsedConfigAcc;
                ConfigItemRawValue ->
                    ConfigItemRawValue1 = maybe_trim_quotes(ConfigItemRawValue),
                    case parse_config_item(ConfigItemRawValue1, Config, Parser) of
                        {ok, ConfigItemValue} ->
                            [{ConfigItemName, ConfigItemValue} | ParsedConfigAcc];
                        error ->
                            BinParser = dotenv_config_error:to_binary(Parser),
                            exit(
                                <<"Can't parse config item: ", ConfigItemName/binary, " of value: ",
                                    ConfigItemRawValue1/binary, " to type: ", BinParser/binary>>
                            )
                    end
            end
        end,
        [],
        ConfigItemsParsers
    ),
    {ok, ParsedConfig}.

-spec maybe_trim_quotes(config_item_raw_value()) -> config_item_raw_value().
maybe_trim_quotes(ConfigItemRawValue) ->
    LastByte = byte_size(ConfigItemRawValue) - 1,
    case {binary:at(ConfigItemRawValue, 0), binary:at(ConfigItemRawValue, LastByte)} of
        % double quotes
        {34, 34} ->
            string:trim(ConfigItemRawValue, both, "\"");
        % single quotes
        {39, 39} ->
            string:trim(ConfigItemRawValue, both, "\'");
        _ ->
            ConfigItemRawValue
    end.

-spec get_default_value(parsed_config_raw(), config_item_name()) ->
    config_item_raw_value() | not_found | already_stored.
get_default_value(Config, ConfigItemName) ->
    MaybeValueFromConfig = maps:get(ConfigItemName, Config, not_found),
    IsAlreadyStored = is_value_already_stored(ConfigItemName),
    case {MaybeValueFromConfig, IsAlreadyStored} of
        {not_found, false} ->
            not_found;
        {not_found, true} ->
            already_stored;
        {ValueFromConfig, _} ->
            ValueFromConfig
    end.

-spec get_environment_variable(
    config_item_name(), config_item_raw_value() | not_found | already_stored
) -> config_item_raw_value() | not_found | already_stored.
get_environment_variable(ConfigItemName, Default) ->
    case os:getenv(binary_to_list(ConfigItemName)) of
        false ->
            Default;
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

-spec parse_config_item(config_item_raw_value(), parsed_config_raw(), config_item_type()) ->
    {ok, config_item_value()} | error.
parse_config_item(ConfigItemRawValue, _ConfigRaw, str) ->
    {ok, ConfigItemRawValue};
parse_config_item(ConfigItemRawValue, _ConfigRaw, int) ->
    case string:to_integer(ConfigItemRawValue) of
        {Value, _Rest} when is_integer(Value) ->
            {ok, Value};
        _ ->
            error
    end;
parse_config_item(ConfigItemRawValue, _ConfigRaw, bool) ->
    try binary_to_existing_atom(ConfigItemRawValue) of
        true -> {ok, true};
        false -> {ok, false}
    catch
        _:_ ->
            error
    end;
parse_config_item(ConfigItemRawValue, _ConfigRaw, json) ->
    try jiffy:decode(ConfigItemRawValue, [return_maps]) of
        JsonValue ->
            {ok, JsonValue}
    catch
        _:_ ->
            error
    end;
parse_config_item(ConfigItemRawValue, _ConfigRaw, atom) ->
    try binary_to_atom(ConfigItemRawValue) of
        AtomValue ->
            {ok, AtomValue}
    catch
        _:_ ->
            error
    end;
parse_config_item(ConfigItemRawValue, _ConfigRaw, module) ->
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
parse_config_item(ConfigItemRawValue, _ConfigRaw, charlist) ->
    {ok, binary_to_list(ConfigItemRawValue)};
parse_config_item(ConfigItemRawValue, ConfigRaw, [Type | Rest]) ->
    case Type of
        {exact, ExactValue} when ExactValue =:= ConfigItemRawValue ->
            {ok, ExactValue};
        SingleType when is_atom(SingleType) ->
            parse_config_item(ConfigItemRawValue, ConfigRaw, SingleType);
        {exact, _} ->
            parse_config_item(ConfigItemRawValue, ConfigRaw, Rest)
    end;
parse_config_item(ConfigItemRawValue, ConfigRaw, ConvertFunction) when
    is_function(ConvertFunction)
->
    FunctionToRun =
        case proplists:get_value(arity, erlang:fun_info(ConvertFunction)) of
            1 ->
                fun() -> ConvertFunction(ConfigItemRawValue) end;
            2 ->
                fun() -> ConvertFunction(ConfigItemRawValue, ConfigRaw) end
        end,

    try FunctionToRun() of
        ConfigItemValue ->
            {ok, ConfigItemValue}
    catch
        _:_ ->
            error
    end;
parse_config_item(_, _, _) ->
    error.
