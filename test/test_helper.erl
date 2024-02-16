-module(test_helper).

-export([clear_persistent_term/0]).

clear_persistent_term() ->
    AllConfig = persistent_term:get(),
    lists:foreach(
        fun({Key, _}) ->
            persistent_term:erase(Key)
        end,
        AllConfig
    ).
