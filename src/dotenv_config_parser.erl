-module(dotenv_config_parser).

-callback get_parser() -> Parser :: parser().

-type parser() :: [{ConfigItemName :: binary(), ConfigItemType :: config_item_type()}].

-type simple_config_type() :: str | int | bool | json | atom | module | charlist.
-type exact_value() :: {exact, binary()}.
-type convert_function() :: fun((RawValue :: binary()) -> ConvertedValue :: any()).

-type config_item_type() ::
    simple_config_type()
    | [exact_value() | simple_config_type()]
    | convert_function().

-export_type([parser/0, convert_function/0]).
-export([parse_file/1, parse_config/2]).

-spec parse_file(file:name_all()) -> {ok, [{binary(), binary()}]} | {error, binary()}.
parse_file(FileName) ->
    case file:read_file(FileName) of
        {ok, FileContent} ->
            parse_file_content(FileContent);
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_file_content(binary()) -> {ok, [{binary(), binary()}]} | {error, binary()}.
parse_file_content(FileContent) ->
    Lines = binary:split(FileContent, <<"\n">>, [global, trim_all]),
    lists:foldl(
        fun(Line, ParsedLines) ->
            case parse_line(Line) of
                {ok, ParsedLine} ->
                    [ParsedLine | ParsedLines];
                error ->
                    exit(<<"Can't parse this line from doteenv config: ", Line/binary>>)
            end
        end,
        [],
        Lines
    ).

-spec parse_line(binary()) -> {ok, {binary(), binary()}} | error.
parse_line(Line) ->
    case binary:split(Line, <<"=">>, [trim_all]) of
        [Key, Value] ->
            {ok, {string:trim(Key), string:trim(Value)}};
        _ ->
            error
    end.
