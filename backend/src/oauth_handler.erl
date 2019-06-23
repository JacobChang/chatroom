-module(oauth_handler).

-export([init/2]).

init(Req = #{method := <<"POST">>}, State) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req),
    OAuth = jiffy:decode(Data, [return_maps]),
    Params = #{code => maps:get(<<"code">>, OAuth),
	       client_id =>
		   <<"75ebf552048471679c396db88e85f795784d61787bee4"
		     "dcf647f75db011edf8a738bf9b707662297b9645da949"
		     "80a148dbefc1053b98481aae1e882c61a4189e"/utf8>>,
	       client_secret =>
		   <<"029a9265594a513978dbb6399ba72d3e05bd2e1121bb6"
		     "1227af9fe01b28a1894843af0b4165cdde7fd35e81a34"
		     "c59e7c53fd7c045ef431a31290d9e1ad6058a7233e4b8"
		     "48c47039454d1ebfdddfeac1cd3683537cd71db8a077c"
		     "5792a85208adf950b4e46ea0cda71e99f6715e2623add"
		     "ed738002b08fd6e7c3316fc0941a82c"/utf8>>},
    {ok, ConnPid} = gun:open("sso.feblr.org", 80),
    StreamRef = gun:post(ConnPid, "/api/v1/tickets",
			 [{<<"content-type">>, "application/json"}],
			 jiffy:encode(Params)),
    {response, nofin, Status, Headers} = gun:await(ConnPid,
						   StreamRef),
    {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
    gun:close(ConnPid),
    Ticket = jiffy:decode(ResBody, [return_maps]),
    Key = <<"hurry.feblr.org">>,
    Claims = [{user_id, maps:get(<<"id">>, Ticket)},
	      {open_id, maps:get(<<"open_id">>, Ticket)}],
    ExpirationSeconds = 86400,
    {ok, Token} = jwt:encode(<<"HS256">>, Claims,
			     ExpirationSeconds, Key),
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
