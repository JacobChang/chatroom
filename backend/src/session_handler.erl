-module(session_handler).

-export([init/2]).

-include("session.hrl").

init(Req = #{method := <<"GET">>}, State) ->
    Cookies = cowboy_req:parse_cookies(Req),
    {_, SessionCookie} = lists:keyfind(<<"sessionid">>, 1,
				       Cookies),
    {ok, Session} = jwt:decode(SessionCookie,
			       <<"hurry.feblr.org">>),
    Req0 = cowboy_req:reply(200,
			    #{<<"content-type">> => <<"application/json">>},
			    jiffy:encode(Session), Req),
    {ok, Req0, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{<<"allow">> => <<"GET">>},
			   Req0),
    {ok, Req, State}.
