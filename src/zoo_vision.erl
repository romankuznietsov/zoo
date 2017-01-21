-module(zoo_vision).

-export([view/3]).

-define(FIELD_OF_VIEW, 0.3490658503988659). % PI / 9 = 20 deg
-define(EYE_DIRECTIONS, [-0.6981317007977318, 0, 0.6981317007977318]).
-define(MAX_DISTANCE, 100).

-spec view({number(), number()}, number(), [{number(), number()}]) -> [number()].
view(Position, Direction, Objects) ->
    lists:map(fun(EyeDirection) ->
                      AbsoluteDirection = zoo_angle:normalize(Direction + EyeDirection),
                      view_eye(Position, AbsoluteDirection, Objects)
              end, ?EYE_DIRECTIONS).

-spec view_eye({number(), number()}, number(), [{number(), number()}]) -> number().
view_eye(Position, Direction, Objects) ->
    Signals = [view_object(Position, Direction, Object) || Object <- Objects],
    lists:sum(Signals).

-spec view_object({number(), number()}, number(), {number(), number()}) -> number().
view_object(Position = {X, Y}, Direction, ObjectPosition = {ObjX, ObjY}) ->
    DirectionToObject = math:atan2(ObjY - Y, ObjX - X),
    EyeAngle = abs(zoo_angle:normalize(DirectionToObject - Direction)),
    case EyeAngle =< ?FIELD_OF_VIEW of
        true ->
            Distance = zoo_vector:distance(Position, ObjectPosition),
            case Distance < ?MAX_DISTANCE of
                true -> 1 - (Distance / ?MAX_DISTANCE);
                false -> 0
            end;
        false -> 0
    end.
