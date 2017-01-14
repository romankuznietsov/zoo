%%%-------------------------------------------------------------------
%% @doc zoo public API
%% @end
%%%-------------------------------------------------------------------

-module(zoo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_cowboy(),
    zoo_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_cowboy() ->
    Routes = [
              {"/", cowboy_static, {priv_file, zoo, "static/index.html"}},
              {"/[...]", cowboy_static, {priv_dir, zoo, "static"}}
             ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 4000}],
                                [{env, [{dispatch, Dispatch}]}]
                               ).
