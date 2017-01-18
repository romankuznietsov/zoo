-module(zoo_creature).
-export([new/0, update/1, feed/1, position/1, as_json/1]).
-export_type([zoo_creature/0]).

-define(STARTING_ENERGY, 1000).
-define(BRAIN_INPUTS, 3).
-define(BRAIN_OUTPUTS, 2).
-define(BRAIN_SIZE, 7).
-define(TURN_SPEED, 0.1).
-define(MOVE_SPEED, 1).
-define(FOOD_ENERGY, 100).

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

-spec update(zoo_creature()) -> zoo_creature().
update(Creature = #zoo_creature{position = Position, direction = Direction,
                              brain = Brain, energy = Energy}) ->
    {NewBrain, [MoveSignal, TurnSignal]} = zoo_network:run([1, 1, 1], Brain),
    NewDirection = update_direction(Direction, TurnSignal),
    NewPosition = update_position(Position, NewDirection, MoveSignal),
    NewEnergy = update_energy(Energy),
    Creature#zoo_creature{
      position = NewPosition,
      direction = NewDirection,
      brain = NewBrain,
      energy = NewEnergy
     }.

-spec feed(zoo_creature()) -> zoo_creature().
feed(Creature = #zoo_creature{energy = Energy}) ->
    Creature#zoo_creature{energy = Energy + ?FOOD_ENERGY}.

-spec position(zoo_creature()) -> {number(), number()}.
position(#zoo_creature{position = Position}) ->
    Position.

-spec as_json(zoo_creature()) -> term().
as_json(#zoo_creature{id = Id, position = {X, Y}, direction = Direction, energy = Energy}) ->
    {[
      {<<"id">>, list_to_binary(erlang:ref_to_list(Id))},
      {<<"position">>, {[
                         {<<"x">>, X},
                         {<<"y">>, Y}
                        ]}},
      {<<"direction">>, Direction},
      {<<"energy">>, Energy}
     ]}.

-spec update_direction(number(), number()) -> number().
update_direction(Direction, TurnSignal) ->
    Rotation = TurnSignal * ?TURN_SPEED,
    zoo_angle:normalize(Direction + Rotation).

-spec update_position({number(), number()}, number(), number()) -> {number(), number()}.
update_position({X, Y}, Direction, MoveSignal) ->
    Dx = math:cos(Direction) * (MoveSignal + 1) * ?MOVE_SPEED,
    Dy = math:sin(Direction) * (MoveSignal + 1) * ?MOVE_SPEED,
    {X + Dx, Y + Dy}.

-spec update_energy(number()) -> number().
update_energy(Energy) when Energy > 0 ->
    Energy - 1;
update_energy(_) -> 0.
