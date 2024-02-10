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
-export([parse_file/1, parse_config/1]).
