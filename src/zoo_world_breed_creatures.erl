-module(zoo_world_breed_creatures).
-export([run/1]).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Creatures = zoo_world:creatures(World),
    NewCreatures = lists:foldl(
                     fun(Creature, Acc) ->
                             case zoo_creature:can_reproduce(Creature) of
                                 true ->
                                     {Parent, Child} = zoo_creature:reproduce(Creature),
                                     [Parent, Child | Acc];
                                 false ->
                                     [Creature | Acc]
                             end
                     end, [], Creatures),
    zoo_world:set_creatures(NewCreatures, World).
