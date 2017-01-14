-module(zoo_food).
-export([new/0]).
-export_type([zoo_food/0]).

-record(zoo_food, {
          id :: reference(),
          position :: {number(), number()}
         }).

-opaque zoo_food() :: #zoo_food{}.

-spec new() -> zoo_food().
new() ->
    #zoo_food{
       id = make_ref(),
       position = zoo_vector:random_position()
      }.
