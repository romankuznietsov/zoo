-module(zoo_world_update_creatures).

-export([run/1]).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Creatures = zoo_world:creatures(World),
    Plants = zoo_world:plants(World),
    PlantPositions = [zoo_plant:position(Plant) || Plant <- Plants],
    CreaturePositions = [zoo_creature:position(Creature) || Creature <- Creatures],
    UpdatedCreatures = lists:map(fun(Creature) ->
                                         Position = zoo_creature:position(Creature),
                                         Direction = zoo_creature:direction(Creature),
                                         PlantSignals = zoo_vision:view(Position, Direction, PlantPositions),
                                         CreatureSignals = zoo_vision:view(Position, Direction, CreaturePositions),
                                         zoo_creature:update(Creature, PlantSignals ++ CreatureSignals)
                                 end, Creatures),
    zoo_world:set_creatures(UpdatedCreatures, World).
