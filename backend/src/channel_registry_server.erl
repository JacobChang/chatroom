-module(channel_registry_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([query/1, list/0, register/3, start_link/0,
	 unregister/1]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-record(state, {channels}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],
			  []).

register(ChannelId, ChannelConfig, Pid) ->
    gen_server:call(?SERVER,
		    {register, ChannelId, ChannelConfig, Pid}).

unregister(ChannelId) ->
    gen_server:call(?SERVER, {unregister, ChannelId}).

list() -> gen_server:call(?SERVER, {list}).

query(ChannelId) -> gen_server:call(?SERVER, { query, ChannelId}).

init(_Args) -> {ok, #state{channels = #{}}}.

handle_call({register, ChannelId, Channel, Pid}, _From,
	    State) ->
    NewChannels = maps:put(ChannelId, {Channel, Pid},
			   State#state.channels),
    {reply, ok, State#state{channels = NewChannels}};
handle_call({unregister, ChannelId}, _From, State) ->
    NewChannels = maps:remove(ChannelId,
			      State#state.channels),
    {reply, ok, State#state{channels = NewChannels}};
handle_call({list}, _From, State) ->
    {reply, State#state.channels, State};
handle_call({query, ChannelId}, _From, State) ->
    Channel = maps:find(ChannelId, State#state.channels),
    {reply, Channel, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
