-module(zoo_world_clear_dead_creatures).

-export([run/1]).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Creatures = zoo_world:creatures(World),
    SurvivedCreatures = lists:foldl(fun(Creature, Survived) ->
                                            case zoo_creature:alive(Creature) of
                                                true -> [Creature | Survived];
                                                false -> Survived
                                            end
                                    end, [], Creatures),
    zoo_world:set_creatures(SurvivedCreatures, World).
