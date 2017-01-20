-module(zoo_world).
-export([new/0, update/1, as_json/1]).
-export_type([zoo_world/0]).

-define(INITIAL_POPULATION_SIZE, 10).
-define(INITIAL_PLANT_NUMBER, 300).
-define(FEED_DISTANCE, 10).

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

-spec update(zoo_world()) -> zoo_world().
update(World) ->
    World1 = update_creatures(World),
    World2 = feed_creatures(World1),
    World3 = clear_dead_creatures(World2),
    World4 = regenerate_plants(World3),
    update_age(World4).

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

-spec update_creatures(#zoo_world{}) -> #zoo_world{}.
update_creatures(World = #zoo_world{creatures = Creatures}) ->
    UpdatedCreatures = [zoo_creature:update(Creature) || Creature <- Creatures],
    World#zoo_world{creatures = UpdatedCreatures}.

-spec update_age(#zoo_world{}) -> #zoo_world{}.
update_age(World = #zoo_world{age = Age}) ->
    World#zoo_world{age = Age + 1}.

-spec feed_creatures(#zoo_world{}) -> #zoo_world{}.
feed_creatures(World = #zoo_world{creatures = Creatures, plants = Plants}) ->
    {UpdatedCreatures, RemainingPlants}
    = lists:mapfoldl(fun(Creature, Plants1) ->
                             feed_creature(Creature, Plants1)
                     end, Plants, Creatures),
    World#zoo_world{creatures = UpdatedCreatures, plants = RemainingPlants}.

-spec feed_creature(zoo_creature:zoo_creature(), [zoo_plant:zoo_plant()]) ->
    {zoo_creature:zoo_creature(), [zoo_plant:zoo_plant()]}.
feed_creature(Creature, Plants) ->
    CreaturePosition = zoo_creature:position(Creature),
    lists:foldl(fun(Plant, {Creature1, RemainingPlants}) ->
                        PlantPosition = zoo_plant:position(Plant),
                        Distance = zoo_vector:distance(CreaturePosition, PlantPosition),
                        case Distance =< ?FEED_DISTANCE of
                            true ->
                                {zoo_creature:feed(Creature1), RemainingPlants};
                            false ->
                                {Creature1, [Plant | RemainingPlants]}
                        end
                end, {Creature, []}, Plants).

-spec clear_dead_creatures(#zoo_world{}) -> #zoo_world{}.
clear_dead_creatures(World = #zoo_world{creatures = Creatures}) ->
    SurvivedCreatures = lists:foldl(fun(Creature, Survived) ->
                                            case zoo_creature:alive(Creature) of
                                                true -> [Creature | Survived];
                                                false -> Survived
                                            end
                                    end, [], Creatures),
    World#zoo_world{creatures = SurvivedCreatures}.

-spec regenerate_plants(#zoo_world{}) -> #zoo_world{}.
regenerate_plants(World = #zoo_world{plants = Plants}) ->
    World#zoo_world{plants = [zoo_plant:new() | Plants]}.
