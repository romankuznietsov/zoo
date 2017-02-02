-module(zoo_plant).
-export([new/0, position/1, as_json/1]).
-export_type([zoo_plant/0]).

-record(zoo_plant, {
          position :: {number(), number()}
         }).

-opaque zoo_plant() :: #zoo_plant{}.

-spec new() -> zoo_plant().
new() ->
    #zoo_plant{
       position = zoo_vector:random_position()
      }.

-spec position(zoo_plant()) -> {number(), number()}.
position(#zoo_plant{position = Position}) ->
    Position.

-spec as_json(zoo_plant()) -> term().
as_json(#zoo_plant{position = {X, Y}}) ->
    {[
      {<<"position">>, {[
                         {<<"x">>, X},
                         {<<"y">>, Y}
                        ]}}
     ]}.
