-module(zoo_vector).
-export([random_position/0, distance/2]).

-define(MAX_COORD, 400).

-spec random_position() -> {number(), number()}.
random_position() ->
    {random_coordinate(), random_coordinate()}.

-spec random_coordinate() -> number().
random_coordinate() ->
    (rand:uniform() * 2 - 1) * ?MAX_COORD.

-spec distance({number(), number()}, {number(), number()}) -> number().
distance({X1, Y1}, {X2, Y2}) ->
    Dx = X1 - X2,
    Dy = Y1 - Y2,
    math:sqrt(Dx * Dx + Dy * Dy).
