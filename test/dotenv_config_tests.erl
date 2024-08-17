-module(dotenv_config_tests).

-include_lib("eunit/include/eunit.hrl").

init_from_files_list_test() ->
    dotenv_config:stop(),

    ok = dotenv_config:init(
        parser_module_sample,
        ["./test/data/init_test_part_1.env", "./test/data/init_test_part_2.env"]
    ),
    ?assertEqual({ok, <<"client_id_value">>}, dotenv_config:fetch(<<"CLIENT_ID">>)),
    ?assertEqual({ok, 8080}, dotenv_config:fetch(<<"PORT">>)),
    ?assertEqual({ok, true}, dotenv_config:fetch(<<"DEBUG">>)),
    ?assertEqual({ok, [<<"name1">>, <<"name2">>]}, dotenv_config:fetch(<<"NAMES">>)),
    ?assertEqual({ok, debug}, dotenv_config:fetch(<<"LOG_LEVEL">>)),
    ?assertEqual({ok, dotenv_config}, dotenv_config:fetch(<<"CALLBACK_MODULE">>)),
    ?assertEqual({ok, "abc"}, dotenv_config:fetch(<<"CHARLIST">>)),
    ?assertEqual({ok, <<"allowlist">>}, dotenv_config:fetch(<<"SOME_LIST_MODE">>)),
    ?assertEqual({ok, <<"infinity">>}, dotenv_config:fetch(<<"SOME_CALL_TIMEOUT">>)),
    ?assertEqual({ok, 5}, dotenv_config:fetch(<<"ANOTHER_CALL_TIMEOUT">>)),
    ?assertEqual({ok, <<"im_a_complex_type_42">>}, dotenv_config:fetch(<<"SOME_COMPLEX_TYPE">>)),
    ?assertEqual(
        {ok, #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => true,
            <<"key3">> => 123,
            <<"key4">> => [1, 2, 3],
            <<"key5">> => #{<<"key6">> => <<"value6">>},
            <<"key7">> => null
        }},
        dotenv_config:fetch(<<"JSON_OBJECT">>)
    ).

init_from_env_files_test() ->
    dotenv_config:stop(),

    os:putenv("DOTENV_CONFIG_ENV_FILES", "./test/data/init_test_part_1.env,./test/data/init_test_part_2.env"),

    ok = dotenv_config:init(parser_module_sample),
    ?assertEqual({ok, <<"client_id_value">>}, dotenv_config:fetch(<<"CLIENT_ID">>)),
    ?assertEqual({ok, 8080}, dotenv_config:fetch(<<"PORT">>)),
    ?assertEqual({ok, true}, dotenv_config:fetch(<<"DEBUG">>)),
    ?assertEqual({ok, [<<"name1">>, <<"name2">>]}, dotenv_config:fetch(<<"NAMES">>)),
    ?assertEqual({ok, debug}, dotenv_config:fetch(<<"LOG_LEVEL">>)),
    ?assertEqual({ok, dotenv_config}, dotenv_config:fetch(<<"CALLBACK_MODULE">>)),
    ?assertEqual({ok, "abc"}, dotenv_config:fetch(<<"CHARLIST">>)),
    ?assertEqual({ok, <<"allowlist">>}, dotenv_config:fetch(<<"SOME_LIST_MODE">>)),
    ?assertEqual({ok, <<"infinity">>}, dotenv_config:fetch(<<"SOME_CALL_TIMEOUT">>)),
    ?assertEqual({ok, 5}, dotenv_config:fetch(<<"ANOTHER_CALL_TIMEOUT">>)),
    ?assertEqual({ok, <<"im_a_complex_type_42">>}, dotenv_config:fetch(<<"SOME_COMPLEX_TYPE">>)),
    ?assertEqual(
        {ok, #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => true,
            <<"key3">> => 123,
            <<"key4">> => [1, 2, 3],
            <<"key5">> => #{<<"key6">> => <<"value6">>},
            <<"key7">> => null
        }},
        dotenv_config:fetch(<<"JSON_OBJECT">>)
    ).

stop_test() ->
    dotenv_config:stop(),

    ok = dotenv_config:init(
        parser_module_sample,
        ["./test/data/init_test_part_1.env", "./test/data/init_test_part_2.env"]
    ),
    ?assertEqual({ok, <<"client_id_value">>}, dotenv_config:fetch(<<"CLIENT_ID">>)),
    ?assertEqual({ok, 8080}, dotenv_config:fetch(<<"PORT">>)),
    ?assertEqual({ok, true}, dotenv_config:fetch(<<"DEBUG">>)),
    ?assertEqual({ok, [<<"name1">>, <<"name2">>]}, dotenv_config:fetch(<<"NAMES">>)),
    ?assertEqual({ok, debug}, dotenv_config:fetch(<<"LOG_LEVEL">>)),
    ?assertEqual({ok, dotenv_config}, dotenv_config:fetch(<<"CALLBACK_MODULE">>)),
    ?assertEqual({ok, "abc"}, dotenv_config:fetch(<<"CHARLIST">>)),
    ?assertEqual({ok, <<"allowlist">>}, dotenv_config:fetch(<<"SOME_LIST_MODE">>)),
    ?assertEqual({ok, <<"infinity">>}, dotenv_config:fetch(<<"SOME_CALL_TIMEOUT">>)),
    ?assertEqual({ok, 5}, dotenv_config:fetch(<<"ANOTHER_CALL_TIMEOUT">>)),
    ?assertEqual({ok, <<"im_a_complex_type_42">>}, dotenv_config:fetch(<<"SOME_COMPLEX_TYPE">>)),
    ?assertEqual(
        {ok, #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => true,
            <<"key3">> => 123,
            <<"key4">> => [1, 2, 3],
            <<"key5">> => #{<<"key6">> => <<"value6">>},
            <<"key7">> => null
        }},
        dotenv_config:fetch(<<"JSON_OBJECT">>)
    ),

    ok = dotenv_config:stop(),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"CLIENT_ID">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"PORT">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"DEBUG">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"NAMES">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"LOG_LEVEL">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"CALLBACK_MODULE">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"CHARLIST">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"SOME_LIST_MODE">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"SOME_CALL_TIMEOUT">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"ANOTHER_CALL_TIMEOUT">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"SOME_COMPLEX_TYPE">>)),
    ?assertEqual({error, not_found}, dotenv_config:fetch(<<"JSON_OBJECT">>)).

init_multiline_test() ->
    dotenv_config:stop(),

    ok = dotenv_config:init(
        parser_module_multiline,
        ["./test/data/init_test_multiline.env"]
    ),
    ?assertEqual(
        <<"singleline_value">>,
        dotenv_config:get(<<"SINGLELINE">>)
    ),
    ?assertEqual(
        <<"line1\n  line2\nline3    line4">>,
        dotenv_config:get(<<"MULTILINE">>)
    ).

get_all_test() ->
    dotenv_config:stop(),

    ok = dotenv_config:init(
        parser_module_sample,
        ["./test/data/init_test_part_1.env", "./test/data/init_test_part_2.env"]
    ),
    Expected = maps:from_list([
        {<<"CLIENT_ID">>, <<"client_id_value">>},
        {<<"PORT">>, 8080},
        {<<"DEBUG">>, true},
        {<<"NAMES">>, [<<"name1">>, <<"name2">>]},
        {<<"LOG_LEVEL">>, debug},
        {<<"CALLBACK_MODULE">>, dotenv_config},
        {<<"CHARLIST">>, "abc"},
        {<<"SOME_LIST_MODE">>, <<"allowlist">>},
        {<<"SOME_CALL_TIMEOUT">>, <<"infinity">>},
        {<<"ANOTHER_CALL_TIMEOUT">>, 5},
        {<<"SOME_COMPLEX_TYPE">>, <<"im_a_complex_type_42">>},
        {<<"JSON_OBJECT">>, #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => true,
            <<"key3">> => 123,
            <<"key4">> => [1, 2, 3],
            <<"key5">> => #{<<"key6">> => <<"value6">>},
            <<"key7">> => null
        }}
    ]),
    Actual = maps:from_list(dotenv_config:get_all()),
    ?assertEqual(Expected, Actual).

env_variable_replaces_value_from_dotenv_test() ->
    dotenv_config:stop(),

    SetEnvVarName = <<"VALUE_TO_OVERRIDE">>,
    SetEnvVarValue = <<"override_from_env_variables">>,
    os:putenv(binary_to_list(SetEnvVarName), binary_to_list(SetEnvVarValue)),

    ok = dotenv_config:init(
        parser_test_env_vars_supremacy,
        ["./test/data/test_env_vars_supremacy.env"]
    ),
    ?assertEqual(
        <<"override_from_env_variables">>, dotenv_config:get(<<"VALUE_TO_OVERRIDE">>)
    ),
    ?assertEqual(8080, dotenv_config:get(<<"PORT">>)).
