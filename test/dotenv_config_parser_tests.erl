-module(dotenv_config_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parse_file_test() ->
    {ok, _} = dotenv_config_parser:parse_file("./test/data/test.env").

parse_file_content_test() ->
    FileContent = <<"KEY=VALUE\n">>,
    {ok, Parsed} = dotenv_config_parser:parse_file_content(FileContent),
    ?assertEqual(#{<<"KEY">> => <<"VALUE">>}, Parsed).

parse_single_line_test() ->
    Line = <<"KEY=VALUE">>,
    {ok, {Key, Value}} = dotenv_config_parser:parse_single_line(Line),
    ?assertEqual(<<"KEY">>, Key),
    ?assertEqual(<<"VALUE">>, Value).

parse_multi_line_test() ->
    Line = <<"\"\"\"">>,
    ParsedHead = <<"HEAD">>,
    {ok, end_multi_line} = dotenv_config_parser:parse_multi_line(Line, ParsedHead),

    Line1 = <<"LINE1">>,
    {ok, ParsedHead1} = dotenv_config_parser:parse_multi_line(Line1, ParsedHead),
    ?assertEqual(<<ParsedHead/binary, "\n", Line1/binary>>, ParsedHead1).

parse_config_test() ->
    Config = #{<<"CLIENT_ID">> => <<"VALUE">>},
    Module = parser_module_client_id,
    {ok, Parsed} = dotenv_config_parser:parse_config(Config, Module),
    ?assertEqual([{<<"CLIENT_ID">>, <<"VALUE">>}], Parsed).

get_default_value_test() ->
    ConfigItemName = <<"KEY">>,
    Config = #{<<"KEY">> => <<"VALUE">>},
    ?assertEqual(<<"VALUE">>, dotenv_config_parser:get_default_value(Config, ConfigItemName)),

    ConfigItemNameNotFound = <<"NON_EXISTENT_KEY">>,
    ?assertEqual(not_found, dotenv_config_parser:get_default_value(Config, ConfigItemNameNotFound)),

    ConfigItemNameStored = <<"STORED_KEY">>,
    dotenv_config:set(ConfigItemNameStored, <<"STORED_VALUE">>),
    ?assertEqual(
        already_stored, dotenv_config_parser:get_default_value(Config, ConfigItemNameStored)
    ).

get_environment_variable_test() ->
    ConfigItemName = <<"PATH">>,
    Default = not_found,
    ?assertNotEqual(
        Default, dotenv_config_parser:get_environment_variable(ConfigItemName, Default)
    ),

    NonExistentConfigItemName = <<"NON_EXISTENT_ENV_VAR">>,
    ?assertEqual(
        Default, dotenv_config_parser:get_environment_variable(NonExistentConfigItemName, Default)
    ),

    SetEnvVarName = <<"TEST_ENV_VAR">>,
    SetEnvVarValue = <<"TEST_VALUE">>,
    os:putenv(binary_to_list(SetEnvVarName), binary_to_list(SetEnvVarValue)),
    ?assertEqual(
        SetEnvVarValue, dotenv_config_parser:get_environment_variable(SetEnvVarName, Default)
    ).

is_value_already_stored_test() ->
    ConfigItemName = <<"KEY">>,
    ?assertEqual(false, dotenv_config_parser:is_value_already_stored(ConfigItemName)).

parse_config_item_test() ->
    ConfigItemRawValue = <<"123">>,
    ConfigItemType = int,
    {ok, Value} = dotenv_config_parser:parse_config_item(ConfigItemRawValue, #{}, ConfigItemType),
    ?assertEqual(123, Value).

parse_by_function_test() ->
    ConfigItemRawValue = <<"123">>,
    RawParsedConfig = #{<<"KEY2">> => <<"456">>},
    ConfigItemType = fun(ConfigItemRawValue1, RawParsedConfig1) ->
        Key2 = maps:get(<<"KEY2">>, RawParsedConfig1),
        list_to_integer(binary_to_list(ConfigItemRawValue1) ++ binary_to_list(Key2))
    end,
    {ok, Value} = dotenv_config_parser:parse_config_item(
        ConfigItemRawValue, RawParsedConfig, ConfigItemType
    ),
    ?assertEqual(123456, Value).
