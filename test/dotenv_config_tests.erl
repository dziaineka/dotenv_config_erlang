-module(dotenv_config_tests).

-include_lib("eunit/include/eunit.hrl").

load_from_file_test() ->
    ok = dotenv_config:load_from_file(
        "./test/data/load_from_file_test.env", parser_module_sample
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