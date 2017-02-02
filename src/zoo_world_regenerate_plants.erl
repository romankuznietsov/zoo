-module(zoo_world_regenerate_plants).
-export([run/1]).

-define(MAX_PLANTS, 1000).

-spec run(zoo_world:zoo_world()) -> zoo_world:zoo_world().
run(World) ->
    Plants = zoo_world:plants(World),
    case length(Plants) < ?MAX_PLANTS of
        true ->
            zoo_world:set_plants([zoo_plant:new() | Plants], World);
        false ->
            World
    end.
