-type config_item_name() :: binary().
-type config_item_raw_value() :: binary().
-type config_item_value() :: any().

-type parser() :: [{config_item_name(), config_item_type()}].

-type simple_config_type() :: str | int | bool | json | atom | module | charlist.
-type exact_value() :: {exact, binary()}.
-type convert_function() :: fun((config_item_raw_value()) -> config_item_value()).

-type config_item_type() ::
    simple_config_type()
    | [exact_value() | simple_config_type()]
    | convert_function().

-type parsed_config_file_raw() :: #{config_item_name() => config_item_raw_value()}.
-type parsed_config_file() :: [{config_item_name(), config_item_value()}].
