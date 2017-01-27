-module(zoo_world_srv).
-behaviour(gen_server).

-export([start_link/0, update/0, get_state/0, get_as_json/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, format_status/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(UPDATE_PERIOD, 100).

-record(state, {
          world :: zoo_world:zoo_world(),
          tref :: timer:tref()
         }).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update() ->
    gen_server:cast(?MODULE, update).

get_state() ->
    gen_server:call(?MODULE, get_state).

get_as_json() ->
    gen_server:call(?MODULE, get_as_json).

%%====================================================================
%% Gen server callbacks
%%====================================================================

init([]) ->
    World = zoo_world:new(),
    {ok, TRef} = timer:apply_interval(?UPDATE_PERIOD, ?MODULE, update, []),
    State = #state{world = World, tref = TRef},
    {ok, State}.

handle_call(get_as_json, _From, State = #state{world = World}) ->
    Reply = zoo_world:as_json(World),
    {reply, Reply, State};
handle_call(get_state, _From, State = #state{world = World}) ->
    {reply, World, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(update, State = #state{world = World}) ->
    NewWorld = zoo_world:update(World),
    {noreply, State#state{world = NewWorld}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, [_PDict, _State]) ->
    ok.

terminate(_Reason, #state{tref = TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
