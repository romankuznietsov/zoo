-module(zoo_vision_tests).
-include_lib("eunit/include/eunit.hrl").

view_test() ->
    Signals = zoo_vision:view({0, 0}, 0, [{1, 0}, {1, 1}, {1, -1}]),
    erlang:display(Signals).
