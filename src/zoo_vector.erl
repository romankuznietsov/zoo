-module(zoo_vector).
-export([random_position/0]).

-define(MAX_COORD, 100).

-spec random_position() -> {number(), number()}.
random_position() ->
    {random_coordinate(), random_coordinate()}.

-spec random_coordinate() -> number().
random_coordinate() ->
    (rand:uniform() * 2 - 1) * ?MAX_COORD.
