%%%-------------------------------------------------------------------
%% @doc zoo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(zoo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpecs = [zoo_world_srv_childspec()],
    {ok, { {one_for_all, 0, 1}, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================

zoo_world_srv_childspec() ->
    #{id => zoo_world_srv,
      start => {zoo_world_srv, start_link, []},
      restart => temporary,
      shutdown => 10,
      type => worker,
      modules => [zoo_world_srv]
     }.
