-module(dotenv_config_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parse_file_test() ->
    {ok, _} = dotenv_config_parser:parse_file("./test/data/test.env").

parse_file_content_test() ->
    FileContent = <<"KEY=VALUE\n">>,
    {ok, Parsed} = dotenv_config_parser:parse_file_content(FileContent),
    ?assertEqual(#{<<"KEY">> => <<"VALUE">>}, Parsed).

parse_line_test() ->
    Line = <<"KEY=VALUE">>,
    {ok, {Key, Value}} = dotenv_config_parser:parse_line(Line),
    ?assertEqual(<<"KEY">>, Key),
    ?assertEqual(<<"VALUE">>, Value).

parse_config_test() ->
    Config = #{<<"CLIENT_ID">> => <<"VALUE">>},
    Module = parser_module_client_id,
    {ok, Parsed} = dotenv_config_parser:parse_config(Config, Module),
    ?assertEqual([{<<"CLIENT_ID">>, <<"VALUE">>}], Parsed).

get_default_value_test() ->
    ConfigItemName = <<"KEY">>,
    ?assertEqual(not_found, dotenv_config_parser:get_default_value(ConfigItemName)).

get_environment_variable_test() ->
    ConfigItemName = <<"PATH">>,
    ?assertNotEqual(not_found, dotenv_config_parser:get_environment_variable(ConfigItemName)).

is_value_already_stored_test() ->
    ConfigItemName = <<"KEY">>,
    ?assertEqual(false, dotenv_config_parser:is_value_already_stored(ConfigItemName)).

parse_config_item_test() ->
    ConfigItemRawValue = <<"123">>,
    ConfigItemType = int,
    {ok, Value} = dotenv_config_parser:parse_config_item(ConfigItemRawValue, ConfigItemType),
    ?assertEqual(123, Value).
