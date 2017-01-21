-module(zoo_world_update_age).

-export([run/1]).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Age = zoo_world:age(World),
    zoo_world:set_age(Age + 1, World).
