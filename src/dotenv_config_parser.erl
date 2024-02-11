-module(dotenv_config_parser).

-callback get_parser() -> parser().

-type config_item_name() :: binary().
-type config_item_raw_value() :: binary().
-type config_item_value() :: any().

-type parser() :: [{config_item_name(), config_item_type()}].

-type simple_config_type() :: str | int | bool | json | atom | module | charlist.
-type exact_value() :: {exact, binary()}.
-type convert_function() :: fun((config_item_raw_value()) -> config_item_value()).

-type config_item_type() ::
    simple_config_type()
    | [exact_value() | simple_config_type()]
    | convert_function().

-type parsed_config_file_raw() :: #{config_item_name() => config_item_raw_value()}.
-type parsed_config_file() :: [{config_item_name(), config_item_value()}].

-export_type([parser/0, convert_function/0]).
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
            case maps:get(ConfigItemName, Config, not_found) of
                not_found ->
                    exit(<<"Can't find config item: ", ConfigItemName/binary>>);
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
