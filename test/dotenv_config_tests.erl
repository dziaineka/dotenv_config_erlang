-module(dotenv_config_tests).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    ok = dotenv_config:init(
        parser_module_sample,
        ["./test/data/init_test_part_1.env", "./test/data/init_test_part_2.env"]
    ),
    ?assertEqual({ok, <<"client_id_value">>}, dotenv_config:get(<<"CLIENT_ID">>)),
    ?assertEqual({ok, 8080}, dotenv_config:get(<<"PORT">>)),
    ?assertEqual({ok, true}, dotenv_config:get(<<"DEBUG">>)),
    ?assertEqual({ok, [<<"name1">>, <<"name2">>]}, dotenv_config:get(<<"NAMES">>)),
    ?assertEqual({ok, debug}, dotenv_config:get(<<"LOG_LEVEL">>)),
    ?assertEqual({ok, dotenv_config}, dotenv_config:get(<<"CALLBACK_MODULE">>)),
    ?assertEqual({ok, "abc"}, dotenv_config:get(<<"CHARLIST">>)),
    ?assertEqual({ok, <<"allowlist">>}, dotenv_config:get(<<"SOME_LIST_MODE">>)),
    ?assertEqual({ok, <<"infinity">>}, dotenv_config:get(<<"SOME_CALL_TIMEOUT">>)),
    ?assertEqual({ok, 5}, dotenv_config:get(<<"ANOTHER_CALL_TIMEOUT">>)),
    ?assertEqual({ok, <<"im_a_complex_type_42">>}, dotenv_config:get(<<"SOME_COMPLEX_TYPE">>)),
    ?assertEqual(
        {ok, #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => true,
            <<"key3">> => 123,
            <<"key4">> => [1, 2, 3],
            <<"key5">> => #{<<"key6">> => <<"value6">>},
            <<"key7">> => null
        }},
        dotenv_config:get(<<"JSON_OBJECT">>)
    ).

get_all_test() ->
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
