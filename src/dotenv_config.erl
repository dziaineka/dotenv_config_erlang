-module(dotenv_config).

-include("dotenv_config.hrl").

-export([init/1, init/2, stop/0, get/1, fetch/1, set/2, get_all/0]).
-export_type([config_item_name/0, config_item_value/0, parser/0, parsed_config/0]).

-ignore_xref({init, 1}).
-ignore_xref({init, 2}).
-ignore_xref({stop, 0}).
-ignore_xref({get, 1}).
-ignore_xref({fetch, 1}).
-ignore_xref({set, 2}).
-ignore_xref({get_all, 0}).

%%------------------------------------------------------------------------------
%% @doc init/2
%% Load configuration from environment variables and store it in the persistent term storage.
%%
%% You can set DOTENV_CONFIG_ENV_FILES environment variable to list of files to load
%% configuration from (comma separated).
%%
%% Environment variables have higher priority than .env files.
%%
%% Parameters:
%%   Module - module which will be used to parse the configuration (see readme for details)
%% Returns:
%%   ok - if configuration was successfully loaded and stored
%%   {error, Reason} - if there was an error during loading or storing
%% @end
%%------------------------------------------------------------------------------
-spec init(module()) -> ok.
init(Module) ->
    EnvFiles =
        case os:getenv("DOTENV_CONFIG_ENV_FILES") of
            false -> [];
            EnvFilesStr -> string:tokens(EnvFilesStr, ",")
        end,
    init(Module, EnvFiles).

%%------------------------------------------------------------------------------
%% @doc init/2
%% Load configuration from .env files and override it by environment variables
%% (if duplicate last read place wins) and store it in the persistent term storage.
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
%% Get configuration item from the persistent term storage. But if it's not found
%% raise an exception.
%% @end
%%------------------------------------------------------------------------------
-spec get(config_item_name()) -> config_item_value().
get(ConfigItemName) ->
    case dotenv_config_storage:get(ConfigItemName) of
        {ok, ConfigItemValue} ->
            ConfigItemValue;
        {error, not_found} ->
            error({config_item_not_found, ConfigItemName})
    end.

%%------------------------------------------------------------------------------
%% @doc fetch/1
%% Get configuration item from the persistent term storage.
%% @end
%%------------------------------------------------------------------------------
-spec fetch(config_item_name()) -> {ok, config_item_value()} | {error, not_found}.

fetch(ConfigItemName) ->
    dotenv_config_storage:get(ConfigItemName).

%%------------------------------------------------------------------------------
%% @doc set/2
%% Set configuration item in the persistent term storage.
%% @end
%%------------------------------------------------------------------------------
-spec set(config_item_name(), config_item_value()) -> ok.
set(ConfigItemName, ConfigItemValue) ->
    dotenv_config_storage:set(ConfigItemName, ConfigItemValue).

%%------------------------------------------------------------------------------
%% @doc get_all/0
%% Get all configuration items from the persistent term storage.
%% @end
%%------------------------------------------------------------------------------
-spec get_all() -> parsed_config().
get_all() ->
    dotenv_config_storage:get_all().

%%------------------------------------------------------------------------------
%% @doc stop/0
%% Stop config storage and delete all stored configuration.
%% @end
%%------------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    dotenv_config_storage:clean().
