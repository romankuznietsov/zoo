-include_lib("eunit/include/eunit.hrl").
-module(zoo_network_tests).

random_network_test() ->
    Network = zoo_network:new(2, 3, 5),
    erlang:display(Network),
    {UpdatedNetwork, Output} = zoo_network:run([1, 2], Network),
    erlang:display(UpdatedNetwork),
    erlang:display(Output).
