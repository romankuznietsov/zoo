-module(zoo_world).
-export([new/0, update/1, age/1, set_age/2, creatures/1, set_creatures/2,
         plants/1, set_plants/2, as_json/1]).
-export_type([zoo_world/0]).

-define(INITIAL_POPULATION_SIZE, 10).
-define(INITIAL_PLANT_NUMBER, 300).

-record(zoo_world, {
          age :: non_neg_integer(),
          creatures :: [zoo_creature:zoo_creature()],
          plants :: [zoo_plant:zoo_plant()]
         }).

-opaque zoo_world() :: #zoo_world{}.

-spec new() -> zoo_world().
new() ->
    Creatures = [zoo_creature:new()
                 || _ <- lists:seq(1, ?INITIAL_POPULATION_SIZE)],
    Plants = [zoo_plant:new()
              || _ <- lists:seq(1, ?INITIAL_PLANT_NUMBER)],
    #zoo_world{
       age = 0,
       creatures = Creatures,
       plants = Plants
      }.

-define(UPDATES, [
                  zoo_world_update_creatures,
                  zoo_world_feed_creatures,
                  zoo_world_clear_dead_creatures,
                  zoo_world_regenerate_plants,
                  zoo_world_breed_creatures,
                  zoo_world_update_age
                 ]).

-spec update(zoo_world()) -> zoo_world().
update(World) ->
    lists:foldl(fun(Module, W) ->
                        Module:run(W)
                end, World, ?UPDATES).

-spec as_json(zoo_world()) -> term().
as_json(#zoo_world{creatures = Creatures, plants = Plants}) ->
    {[
      {<<"creatures">>,
       [zoo_creature:as_json(Creature) || Creature <- Creatures]
      },
      {<<"plants">>,
       [zoo_plant:as_json(Plant) || Plant <- Plants]
      }
     ]}.

-spec age(zoo_world()) -> non_neg_integer().
age(#zoo_world{age = Age}) ->
    Age.

-spec set_age(non_neg_integer(), zoo_world()) -> zoo_world().
set_age(Age, World) ->
    World#zoo_world{age = Age}.

-spec creatures(zoo_world()) -> [zoo_creature:zoo_creature()].
creatures(#zoo_world{creatures = Creatures}) ->
    Creatures.

-spec set_creatures([zoo_creature:zoo_creature()], zoo_world()) -> zoo_world().
set_creatures(Creatures, World) ->
    World#zoo_world{creatures = Creatures}.

-spec plants(zoo_world()) -> [zoo_plant:zoo_plant()].
plants(#zoo_world{plants = Plants}) ->
    Plants.

-spec set_plants([zoo_plant:zoo_plant()], zoo_world()) -> zoo_world().
set_plants(Plants, World) ->
    World#zoo_world{plants = Plants}.
