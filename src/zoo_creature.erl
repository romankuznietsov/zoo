-module(zoo_creature).
-export([new/0, tick/1]).
-export_type([zoo_creature/0]).

-define(STARTING_ENERGY, 1000).
-define(BRAIN_INPUTS, 3).
-define(BRAIN_OUTPUTS, 2).
-define(BRAIN_SIZE, 7).
-define(TURN_SPEED, 0.01).
-define(MOVE_SPEED, 0.1).

-record(zoo_creature, {
          id :: reference(),
          position :: {number(), number()},
          direction :: number(),
          energy :: number(),
          brain :: zoo_network:zoo_network()
         }).

-opaque zoo_creature() :: #zoo_creature{}.

-spec new() -> zoo_creature().
new() ->
    #zoo_creature{
       id = make_ref(),
       position = zoo_vector:random_position(),
       direction = zoo_angle:random_direction(),
       energy = ?STARTING_ENERGY,
       brain = zoo_network:new(?BRAIN_INPUTS, ?BRAIN_OUTPUTS, ?BRAIN_SIZE)
      }.

-spec tick(zoo_creature()) -> zoo_creature().
tick(Creature = #zoo_creature{position = Position, direction = Direction,
                              brain = Brain}) ->
    {NewBrain, [MoveSignal, TurnSignal]} = zoo_network:run([1, 1, 1], Brain),
    erlang:display({move_signal, MoveSignal}),
    erlang:display({turn_signal, TurnSignal}),
    NewDirection = tick_direction(Direction, TurnSignal),
    NewPosition = tick_position(Position, NewDirection, MoveSignal),
    Creature#zoo_creature{
      position = NewPosition,
      direction = NewDirection,
      brain = NewBrain
     }.

-spec tick_direction(number(), number()) -> number().
tick_direction(Direction, TurnSignal) ->
    Rotation = TurnSignal * ?TURN_SPEED,
    zoo_angle:normalize(Direction + Rotation).

-spec tick_position({number(), number()}, number(), number()) -> {number(), number()}.
tick_position({X, Y}, Direction, MoveSignal) ->
    Dx = math:cos(Direction) * (MoveSignal + 1) * ?MOVE_SPEED,
    Dy = math:sin(Direction) * (MoveSignal + 1) * ?MOVE_SPEED,
    {X + Dx, Y + Dy}.
