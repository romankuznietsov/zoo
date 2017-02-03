-module(zoo_vision).

-export([view/3]).

-define(FIELD_OF_VIEW, 0.3490658503988659).
-define(EYE_DIRECTIONS, [
                         -0.6981317007977318,
                         0,
                         0.6981317007977318
                        ]).
-define(MAX_DISTANCE, 100).

-spec view({number(), number()}, number(), [{number(), number()}]) -> [number()].
view(Position, Direction, Objects) ->
    AbsoluteEyeDirections = absolute_eye_directions(Direction),
    Signals = [view_object(Position, Object, AbsoluteEyeDirections) || Object <- Objects],
    combine_signals(Signals).


-spec view_object({number(), number()}, {number(), number()}, [number()]) -> [number()].
view_object(Position, Position, _) ->
    blank_signal();
view_object(Position, Object, EyeDirections) ->
    Distance = zoo_vector:distance(Position, Object),
    Direction = zoo_vector:direction(Position, Object),
    case Distance < ?MAX_DISTANCE of
        true ->
            [view_eye(Distance, Direction, EyeDirection)
             || EyeDirection <- EyeDirections];
        false -> blank_signal()
    end.

-spec view_eye(number(), number(), number()) -> number().
view_eye(Distance, Direction, EyeDirection) ->
    AngleFromEyeCenter = abs(zoo_angle:normalize(Direction - EyeDirection)),
    case AngleFromEyeCenter =< ?FIELD_OF_VIEW of
        true -> 1 - (Distance / ?MAX_DISTANCE);
        false -> 0
    end.

-spec combine_signals([[number()]]) -> [number()].
combine_signals(Signals) ->
    SummedSignals = lists:foldl(fun sum_signals/2, blank_signal(), Signals),
    [S * 2 - 1 || S <- SummedSignals].

-spec sum_signals([number()], [number()]) -> [number()].
sum_signals(Signals, Acc) ->
    [S + A || {S, A} <- lists:zip(Signals, Acc)].

-spec absolute_eye_directions(number()) -> [number()].
absolute_eye_directions(Direction) ->
    [zoo_angle:normalize(Direction + EyeDir) || EyeDir <- ?EYE_DIRECTIONS].

-spec blank_signal() -> [number()].
blank_signal() ->
    [0 || _ <- ?EYE_DIRECTIONS].
