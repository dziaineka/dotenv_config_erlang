-module(dotenv_config).

-include("dotenv_config.hrl").

-export([init/2, get/1, set/2]).
-export_type([config_item_name/0, config_item_value/0, parser/0]).

-ignore_xref({init, 2}).
-ignore_xref({get, 1}).
-ignore_xref({set, 2}).

%%------------------------------------------------------------------------------
%% @doc init/2
%% Load configuration from environment variables and override it by provided .env files
%% (if duplicate last provided file wins) and store it in the persistent term storage.
%% Parameters:
%%   Module - module which will be used to parse the configuration (see readme for details)
%%   FileNames - list of names of the files to load configuration from
%% Returns:
%%   ok - if configuration was successfully loaded and stored
%%   {error, Reason} - if there was an error during loading or storing
%% @end
%%------------------------------------------------------------------------------
-spec init(module(), [file:name_all()]) -> ok.
init(Module, FileNames) ->
    Config = lists:foldl(
        fun(FileName, ConfigAcc) ->
            case dotenv_config_parser:parse_file(FileName) of
                {ok, Config} ->
                    maps:merge(ConfigAcc, Config);
                {error, Reason} ->
                    BinReason = dotenv_config_error:to_binary(Reason),
                    BinFileName = dotenv_config_error:to_binary(FileName),
                    exit(<<"Can't read file ", BinFileName/binary, ": ", BinReason/binary>>)
            end
        end,
        #{},
        FileNames
    ),
    {ok, ParsedConfig} = dotenv_config_parser:parse_config(Config, Module),
    dotenv_config_storage:store(ParsedConfig).

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
