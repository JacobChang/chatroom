%%%-------------------------------------------------------------------
%% @doc backend top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(backend_sup).

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
    SessionRoute = {"/api/v1/session", session_handler, []},
    OAuthRoute = {"/api/v1/oauth", oauth_handler, []},
    ChannelRoute = {"/api/v1/channels", channel_handler,
		    []},
    WebsocketRoute = {"/websocket", websocket_handler, []},
    Dispatch = cowboy_router:compile([{'_',
				       [SessionRoute, OAuthRoute, ChannelRoute,
					WebsocketRoute]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 7071}],
				 #{env => #{dispatch => Dispatch}}),
    channel_sup:start_link(),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) -> {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================

