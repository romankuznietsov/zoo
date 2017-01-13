-module(zoo_network_tests).
-include_lib("eunit/include/eunit.hrl").

random_network_test() ->
    Network = zoo_network:new(2, 3, 5),
    erlang:display(Network),
    {UpdatedNetwork, Output} = zoo_network:run([1, 2], Network),
    erlang:display(UpdatedNetwork),
    erlang:display(Output).
