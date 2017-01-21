-module(zoo_world_update_creatures).

-export([run/1]).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Creatures = zoo_world:creatures(World),
    UpdatedCreatures = [zoo_creature:update(Creature) || Creature <- Creatures],
    zoo_world:set_creatures(UpdatedCreatures, World).
