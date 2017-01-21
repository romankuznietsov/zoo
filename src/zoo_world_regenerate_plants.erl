-module(zoo_world_regenerate_plants).
-export([run/1]).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Plants = zoo_world:plants(World),
    zoo_world:set_plants([zoo_plant:new() | Plants], World).
