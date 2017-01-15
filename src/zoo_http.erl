-module(zoo_http).

-export([start/0, stop/0]).

start() ->
    Routes = [
              {"/", cowboy_static, {priv_file, zoo, "static/index.html"}},
              {"/ws", zoo_websocket_handler, []},
              {"/[...]", cowboy_static, {priv_dir, zoo, "static"}}
             ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_http(zoo_http_listener, 100, [{port, 4000}],
                                [{env, [{dispatch, Dispatch}]}]
                               ).

stop() ->
    cowboy:stop_listener(zoo_http_listener).
