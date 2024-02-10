-module(dotenv_config).

-export([load_from_file/2]).

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
-spec load_from_file(file:name_all(), module()) -> ok | {error, binary()}.
load_from_file(FileName, Module) ->
    % rewrite to maybe expr in 10 years. 2024-10-02
    case dotenv_config_parser:parse_file(FileName) of
        {ok, Config} ->
            case dotenv_config_parser:parse_config(Config, Module) of
                {ok, ParsedConfig} ->
                    case dotenv_config_storage:store(ParsedConfig) of
                        ok -> ok;
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
