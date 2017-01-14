-module(zoo).
-export([start/0, stop/0]).

start() ->
    application:ensure_all_started(zoo).

stop() ->
    application:stop(zoo).
