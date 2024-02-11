-module(dotenv_config_storage_tests).

-include_lib("eunit/include/eunit.hrl").

store_test() ->
    Config = [{<<"KEY">>, <<"VALUE">>}],
    ?assertEqual(ok, dotenv_config_storage:store(Config)).

get_test() ->
    ConfigItemName = <<"KEY">>,
    ConfigItemValue = <<"VALUE">>,
    dotenv_config_storage:set(ConfigItemName, ConfigItemValue),
    {ok, RetrievedValue} = dotenv_config_storage:get(ConfigItemName),
    ?assertEqual(ConfigItemValue, RetrievedValue).

get_not_found_test() ->
    ConfigItemName = <<"NON_EXISTENT_KEY">>,
    ?assertEqual({error, not_found}, dotenv_config_storage:get(ConfigItemName)).

set_test() ->
    ConfigItemName = <<"KEY">>,
    ConfigItemValue = <<"VALUE">>,
    ?assertEqual(ok, dotenv_config_storage:set(ConfigItemName, ConfigItemValue)).
