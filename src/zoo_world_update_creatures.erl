-module(zoo_world_update_creatures).

-export([run/1]).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Creatures = zoo_world:creatures(World),
    Plants = zoo_world:plants(World),
    PlantPositions = [zoo_plant:position(Plant) || Plant <- Plants],
    UpdatedCreatures = lists:map(fun(Creature) ->
                                         Position = zoo_creature:position(Creature),
                                         Direction = zoo_creature:direction(Creature),
                                         Signals = zoo_vision:view(Position, Direction, PlantPositions),
                                         zoo_creature:update(Creature, Signals)
                                 end, Creatures),
    zoo_world:set_creatures(UpdatedCreatures, World).
