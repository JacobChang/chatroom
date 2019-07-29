-module(oauth_handler).

-export([init/2]).

init(Req = #{method := <<"POST">>}, State) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    {ok, ClientId} = maps:find(<<"client_id">>, State),
    {ok, ClientSecret} = maps:find(<<"client_secret">>,
				   State),
    {ok, EndpointDomain} = maps:find(<<"endpoint_domain">>,
				     State),
    {ok, EndpointPort} = maps:find(<<"endpoint_port">>,
				   State),
    OAuth = jiffy:decode(Data, [return_maps]),
    Params = #{code => maps:get(<<"code">>, OAuth),
	       client_id => ClientId, client_secret => ClientSecret},
    {ok, ConnPid} = gun:open(EndpointDomain, EndpointPort),
    StreamRef = gun:post(ConnPid, "/api/v1/tickets",
			 [{<<"content-type">>, "application/json"}],
			 jiffy:encode(Params)),
    {response, nofin, Status, Headers} = gun:await(ConnPid,
						   StreamRef),
    {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
    gun:close(ConnPid),
    Ticket = jiffy:decode(ResBody, [return_maps]),
    {ok, JwtKey} = maps:find(<<"jwt_key">>, State),
    Claims = [{user_id, maps:get(<<"id">>, Ticket)},
	      {open_id, maps:get(<<"open_id">>, Ticket)}],
    ExpirationSeconds = 86400,
    {ok, Token} = jwt:encode(<<"HS256">>, Claims,
			     ExpirationSeconds, JwtKey),
    Req2 = cowboy_req:set_resp_cookie(<<"sessionid">>,
				      Token, Req1, #{http_only => true}),
    Req3 = cowboy_req:reply(200,
			    #{<<"content-type">> => <<"application/json">>},
			    jiffy:encode(#{token => Token}), Req2),
    {ok, Req3, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405,
			   #{<<"allow">> => <<"POST">>}, Req0),
    {ok, Req, State}.
