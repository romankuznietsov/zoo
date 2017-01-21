-module(zoo_world_feed_creatures).
-define(FEED_DISTANCE, 10).

-export([run/1]).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Creatures = zoo_world:creatures(World),
    Plants = zoo_world:plants(World),
    {UpdatedCreatures, RemainingPlants}
    = lists:mapfoldl(fun(Creature, Plants1) ->
                             feed_creature(Creature, Plants1)
                     end, Plants, Creatures),
    zoo_world:set_plants(RemainingPlants,
                         zoo_world:set_creatures(UpdatedCreatures, World)).

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
