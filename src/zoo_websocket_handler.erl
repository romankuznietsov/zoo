-module(zoo_websocket_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-define(UPDATE_PERIOD, 100).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
    {ok, TRef} = timer:send_interval(?UPDATE_PERIOD, update),
    {ok, Req, TRef}.

websocket_handle(Frame = {text, _}, Req, State) ->
    {reply, Frame, Req, State};
websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.

websocket_info(update, Req, State) ->
    WorldAsJson = zoo_world_srv:get_as_json(),
    WorldJson = jiffy:encode(WorldAsJson),
    {reply, {text, WorldJson}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, TRef) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.
