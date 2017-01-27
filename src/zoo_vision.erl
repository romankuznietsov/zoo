-module(zoo_vision).

-export([view/3]).

-define(FIELD_OF_VIEW, 0.3490658503988659). % PI / 9 = 20 deg
-define(EYE_DIRECTIONS, [-0.6981317007977318, 0, 0.6981317007977318]).
-define(MAX_DISTANCE, 100).

-spec view({number(), number()}, number(), [{number(), number()}]) -> [number()].
view(Position, Direction, Objects) ->
    Signals = [view_object(Position, Direction, Object) || Object <- Objects],
    AggregatedSignals = lists:foldl(fun([S1, S2, S3], [A1, A2, A3]) ->
                                            [A1 + S1, A2 + S2, A3 + S3]
                                    end, [0, 0, 0], Signals),
    [S * 2 - 1 || S <- AggregatedSignals].


-spec view_object({number(), number()}, number(), {number(), number()}) -> [number()].
view_object(Position, _, Position) -> [0, 0, 0];
view_object(Position, Direction, Object) ->
    Distance = zoo_vector:distance(Position, Object),
    case Distance < ?MAX_DISTANCE of
        true ->
            [view_eye(Position, Direction, Object, EyeDirection, Distance)
             || EyeDirection <- ?EYE_DIRECTIONS];
        false -> [0, 0, 0]
    end.

-spec view_eye({number(), number()}, number(), {number(), number()}, number(), number()) -> number().
view_eye({X, Y}, Direction, {ObjX, ObjY}, EyeDirection, Distance) ->
    AbsoluteEyeDirection = zoo_angle:normalize(Direction + EyeDirection),
    DirectionToObject = math:atan2(ObjY - Y, ObjX - X),
    EyeAngle = abs(zoo_angle:normalize(DirectionToObject - AbsoluteEyeDirection)),
    case EyeAngle =< ?FIELD_OF_VIEW of
        true -> 1 - (Distance / ?MAX_DISTANCE);
        false -> 0
    end.
