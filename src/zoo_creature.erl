-module(zoo_creature).
-export([new/0, update/2, feed/1, position/1, direction/1, alive/1,
         can_reproduce/1, reproduce/1, as_json/1]).
-export_type([zoo_creature/0]).

-define(STARTING_ENERGY, 1000).
-define(BRAIN_SIZE, 10).
-define(BRAIN_OUTPUTS, 2).
-define(TURN_SPEED, 0.1).
-define(MOVE_SPEED, 1).
-define(FOOD_ENERGY, 100).
-define(MUTATIONS, 10).

-record(zoo_creature, {
          position :: {number(), number()},
          direction :: number(),
          energy :: number(),
          brain :: zoo_network:zoo_network()
         }).

-opaque zoo_creature() :: #zoo_creature{}.

-spec new() -> zoo_creature().
new() ->
    #zoo_creature{
       position = zoo_vector:random_position(),
       direction = zoo_angle:random_direction(),
       energy = ?STARTING_ENERGY,
       brain = zoo_network:new(?BRAIN_SIZE)
      }.

-spec update(zoo_creature(), [number()]) -> zoo_creature().
update(Creature = #zoo_creature{position = Position, direction = Direction,
                              brain = Brain, energy = Energy}, Signals) ->
    {NewBrain, [MoveSignal, TurnSignal]} = zoo_network:run(Signals, ?BRAIN_OUTPUTS, Brain),
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

-spec direction(zoo_creature()) -> number().
direction(#zoo_creature{direction = Direction}) ->
    Direction.

-spec alive(zoo_creature()) -> boolean().
alive(#zoo_creature{energy = Energy}) ->
    Energy > 0.

-spec as_json(zoo_creature()) -> term().
as_json(#zoo_creature{position = {X, Y}, direction = Direction, energy = Energy}) ->
    {[
      {<<"position">>, {[
                         {<<"x">>, X},
                         {<<"y">>, Y}
                        ]}},
      {<<"direction">>, Direction},
      {<<"energy">>, Energy}
     ]}.

-spec can_reproduce(zoo_creature()) -> boolean().
can_reproduce(#zoo_creature{energy = Energy}) ->
    Energy >= ?STARTING_ENERGY * 2.

-spec reproduce(zoo_creature()) -> {zoo_creature(), zoo_creature()}.
reproduce(Parent = #zoo_creature{position = Position, energy = Energy, brain = Brain}) ->
    Mutations = rand:uniform(?MUTATIONS),
    ChildBrain = zoo_network:mutate(zoo_network:clone(Brain), Mutations),
    Child = #zoo_creature{
               position = Position,
               direction = zoo_angle:random_direction(),
               energy = ?STARTING_ENERGY,
               brain = ChildBrain
              },
    {Parent#zoo_creature{energy = Energy - ?STARTING_ENERGY}, Child}.

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
