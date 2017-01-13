-module(zoo_lists_test).
-include_lib("eunit/include/eunit.hrl").

modify_random_test() ->
    Fun = fun(X) -> X + 1 end,
    ?assertEqual([2], zoo_lists:modify_random(Fun, [1])),

    Modified = zoo_lists:modify_random(Fun, [2, 4]),
    ?assert((Modified =:= [3, 4]) or (Modified =:= [2, 5])).
