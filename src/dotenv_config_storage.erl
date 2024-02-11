-module(dotenv_config_storage).

-include("dotenv_config.hrl").

-export([store/1, get/1, set/2]).
-export_type([config_item_name/0, config_item_value/0, parsed_config_file/0]).

-spec store(parsed_config_file()) -> ok.
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
