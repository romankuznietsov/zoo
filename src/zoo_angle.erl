-module(zoo_angle).
-export([random_direction/0, normalize/1]).

-define(PI, 3.141592653589793).
-spec random_direction() -> number().
random_direction() ->
    rand:uniform() * ?PI * 2.

-spec normalize(number()) -> number().
normalize(Angle) when Angle < -?PI ->
    normalize(Angle + ?PI * 2);
normalize(Angle) when Angle > ?PI ->
    normalize(Angle - ?PI * 2);
normalize(Angle) -> Angle.
