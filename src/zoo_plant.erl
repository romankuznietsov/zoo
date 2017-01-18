-module(zoo_plant).
-export([new/0, position/1, as_json/1]).
-export_type([zoo_plant/0]).

-record(zoo_plant, {
          id :: reference(),
          position :: {number(), number()}
         }).

-opaque zoo_plant() :: #zoo_plant{}.

-spec new() -> zoo_plant().
new() ->
    #zoo_plant{
       id = make_ref(),
       position = zoo_vector:random_position()
      }.

-spec position(zoo_plant()) -> {number(), number()}.
position(#zoo_plant{position = Position}) ->
    Position.

-spec as_json(zoo_plant()) -> term().
as_json(#zoo_plant{id = Id, position = {X, Y}}) ->
    {[
      {<<"id">>, list_to_binary(erlang:ref_to_list(Id))},
      {<<"position">>, {[
                         {<<"x">>, X},
                         {<<"y">>, Y}
                        ]}}
     ]}.
