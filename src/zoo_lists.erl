-module(zoo_lists).

-export([modify_random/2]).

% @doc modifies a random element of a List by passing it through Fun
-spec modify_random(fun(), list()) -> list().
modify_random(Fun, List) ->
    Position = rand:uniform(length(List)) - 1,
    Preceding = lists:sublist(List, Position),
    [Element | Following] = lists:nthtail(Position, List),
    Replacement = Fun(Element),
    lists:append([Preceding, [Replacement], Following]).
