-module(dotenv_config_storage).

-include("dotenv_config.hrl").

-ifdef(EUNIT).
-compile(export_all).
-endif.

-export([store/1, get/1, set/2, get_all/0, clean/0]).
-export_type([config_item_name/0, config_item_value/0, parsed_config/0]).

-spec store(parsed_config()) -> ok.
store(Config) ->
    lists:foreach(
        fun({ConfigItemName, ConfigItemValue}) ->
            set(ConfigItemName, ConfigItemValue)
        end,
        Config
    ).

-spec get(config_item_name()) -> {ok, config_item_value()} | {error, not_found}.
get(ConfigItemName) ->
    case persistent_term:get({?MODULE, ConfigItemName}, {error, not_found}) of
        {error, not_found} ->
            {error, not_found};
        ConfigItemValue ->
            {ok, ConfigItemValue}
    end.

-spec set(config_item_name(), config_item_value()) -> ok.
set(ConfigItemName, ConfigItemValue) ->
    persistent_term:put({?MODULE, ConfigItemName}, ConfigItemValue).

-spec get_all() -> parsed_config().
get_all() ->
    AllItems = persistent_term:get(),
    lists:filtermap(
        fun
            ({{?MODULE, Key}, Value}) ->
                {true, {Key, Value}};
            (_) ->
                false
        end,
        AllItems
    ).

-spec clean() -> ok.
clean() ->
    AllItems = persistent_term:get(),
    lists:foreach(
        fun
            ({{?MODULE, Key}, _Value}) ->
                persistent_term:erase({?MODULE, Key});
            (_) ->
                skip
        end,
        AllItems
    ).
