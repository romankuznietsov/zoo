-module(zoo_angle).
-export([random_direction/0, normalize/1]).

-define(MAX_ANGLE, 6.283185307179586).
-spec random_direction() -> number().
random_direction() ->
    rand:uniform() * ?MAX_ANGLE.

-spec normalize(number()) -> number().
normalize(Angle) when Angle < 0 ->
    normalize(Angle + ?MAX_ANGLE);
normalize(Angle) when Angle >= ?MAX_ANGLE ->
    normalize(Angle - ?MAX_ANGLE);
normalize(Angle) -> Angle.
