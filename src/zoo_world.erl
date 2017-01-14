-module(zoo_world).
-export([]).

-record(zoo_world, {
          creatures :: [zoo_creature:zoo_creature()],
          food :: [zoo_food:zoo_food()]
         }).
