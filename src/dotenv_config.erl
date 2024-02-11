-module(dotenv_config).

-include("dotenv_config.hrl").

-export([load_from_file/2, get/1, set/2]).
-export_type([config_item_name/0, config_item_value/0, parser/0]).

-ignore_xref({load_from_file, 2}).
-ignore_xref({get, 1}).
-ignore_xref({set, 2}).

%%------------------------------------------------------------------------------
%% @doc load_from_file/2
%% Load configuration from .env file and store it in the persistent term storage.
%% Parameters:
%%   FileName - name of the file to load configuration from
%%   Module - module which will be used to parse the configuration (see readme for details)
%% Returns:
%%   ok - if configuration was successfully loaded and stored
%%   {error, Reason} - if there was an error during loading or storing
%% @end
%%------------------------------------------------------------------------------
-spec load_from_file(file:name_all(), module()) -> ok.
load_from_file(FileName, Module) ->
    case dotenv_config_parser:parse_file(FileName) of
        {ok, Config} ->
            {ok, ParsedConfig} = dotenv_config_parser:parse_config(Config, Module),
            dotenv_config_storage:store(ParsedConfig);
        {error, Reason} ->
            BinReason = dotenv_config_error:to_binary(Reason),
            BinFileName = dotenv_config_error:to_binary(FileName),
            exit(<<"Can't read file ", BinFileName/binary, ": ", BinReason/binary>>)
    end.

%%------------------------------------------------------------------------------
%% @doc get/1
%% Get configuration item from the persistent term storage.
%% @end
%%------------------------------------------------------------------------------
-spec get(config_item_name()) -> {ok, config_item_value()} | {error, not_found}.
get(ConfigItemName) ->
    dotenv_config_storage:get(ConfigItemName).

%%------------------------------------------------------------------------------
%% @doc set/2
%% Set configuration item in the persistent term storage.
%% @end
%%------------------------------------------------------------------------------
-spec set(config_item_name(), config_item_value()) -> ok.
set(ConfigItemName, ConfigItemValue) ->
    dotenv_config_storage:set(ConfigItemName, ConfigItemValue).
