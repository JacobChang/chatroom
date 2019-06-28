-module(channel_server).

-behaviour(gen_server).

-include("channel.hrl").

%% API
-export([start_link/2]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-record(state, {id, config, clients, tref}).

start_link(ChannelId, ChannelConfig) ->
    Name = uuid:uuid_to_string(ChannelId),
    gen_server:start_link({local, list_to_atom(Name)},
			  ?MODULE, [ChannelId, ChannelConfig], []).

init([ChannelId, ChannelConfig]) ->
    channel_registry_server:register(ChannelId,
				     ChannelConfig, self()),
    {ok, TRef} =
	timer:send_after(ChannelConfig#channel_config.duration *
			   60
			   * 1000,
			 {expired}),
    {ok,
     #state{id = ChannelId, config = ChannelConfig,
	    clients = [], tref = TRef}}.

handle_call({join, Pid}, _From, State) ->
    NewState = State#state{clients =
			       [Pid | State#state.clients]},
    {reply, ok, NewState};
handle_call({broadcast, Pid0, Msg}, _From, State) ->
    lists:foreach(fun (Pid) -> Pid ! Msg end,
		  State#state.clients),
    {reply, ok, State};
handle_call({leave, Pid0}, _From, State) ->
    NewState = State#state{clients =
			       [Pid
				|| Pid <- State#state.clients, Pid =/= Pid0]},
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({expired}, State) -> {stop, normal, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    channel_registry_server:unregister(State#state.id), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
