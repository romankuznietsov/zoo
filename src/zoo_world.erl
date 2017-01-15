-module(zoo_world).
-export([new/0, tick/1]).
-export_type([zoo_world/0]).

-define(INITIAL_POPULATION_SIZE, 10).

-record(zoo_world, {
          creatures :: [zoo_creature:zoo_creature()],
          food :: [zoo_food:zoo_food()]
         }).

-opaque zoo_world() :: #zoo_world{}.

-spec new() -> zoo_world().
new() ->
    Creatures = [zoo_creature:new()
                 || _ <- lists:seq(1, ?INITIAL_POPULATION_SIZE)],
    #zoo_world{
       creatures = Creatures,
       food = []
      }.

-spec tick(zoo_world()) -> zoo_world().
tick(World = #zoo_world{creatures = Creatures}) ->
    NewCreatures = [zoo_creature:tick(Creature)
                    || Creature <- Creatures],
    World#zoo_world{creatures = NewCreatures}.
