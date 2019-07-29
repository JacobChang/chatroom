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
    JwtSecret = <<"hurry.feblr.org">>,
    SessionRoute = {"/api/v1/session", session_handler,
		    #{<<"jwt_secret">> => JwtSecret}},
    OAuthRoute = {"/api/v1/oauth", oauth_handler,
		  #{<<"client_id">> =>
			<<"75ebf552048471679c396db88e85f795784d61787bee4"
			  "dcf647f75db011edf8a738bf9b707662297b9645da949"
			  "80a148dbefc1053b98481aae1e882c61a4189e"/utf8>>,
		    <<"client_secret">> =>
			<<"029a9265594a513978dbb6399ba72d3e05bd2e1121bb6"
			  "1227af9fe01b28a1894843af0b4165cdde7fd35e81a34"
			  "c59e7c53fd7c045ef431a31290d9e1ad6058a7233e4b8"
			  "48c47039454d1ebfdddfeac1cd3683537cd71db8a077c"
			  "5792a85208adf950b4e46ea0cda71e99f6715e2623add"
			  "ed738002b08fd6e7c3316fc0941a82c"/utf8>>,
		    <<"endpoint_domain">> => "sso.feblr.org",
		    <<"endpoint_port">> => 80,
		    <<"jwt_secret">> => JwtSecret}},
    ChannelRoute = {"/api/v1/channels", channel_handler,
		    []},
    WebsocketRoute = {"/websocket", websocket_handler,
		      [#{}]},
    Dispatch = cowboy_router:compile([{'_',
				       [SessionRoute, OAuthRoute, ChannelRoute,
					WebsocketRoute]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 7071}],
				 #{env => #{dispatch => Dispatch}}),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpec = [{channel_sup,
		  {channel_sup, start_link, []}, permanent, infinity,
		  supervisor, [channel_sup]}],
    {ok, {{one_for_one, 0, 1}, ChildSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================

