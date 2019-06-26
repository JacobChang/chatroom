-module(session_handler).

-export([init/2]).

-include("session.hrl").

init(Req = #{method := <<"GET">>}, State) ->
    Cookies = cowboy_req:parse_cookies(Req),
    Req0 = case lists:keyfind(<<"sessionid">>, 1, Cookies)
	       of
	     false ->
		 cowboy_req:reply(404,
				  #{<<"content-type">> =>
					<<"application/json">>},
				  "", Req);
	     {_, SessionCookie} ->
		 {ok, Session} = jwt:decode(SessionCookie,
					    <<"hurry.feblr.org">>),
		 cowboy_req:reply(200,
				  #{<<"content-type">> =>
					<<"application/json">>},
				  jiffy:encode(Session), Req)
	   end,
    {ok, Req0, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{<<"allow">> => <<"GET">>},
			   Req0),
    {ok, Req, State}.
