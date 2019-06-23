-module(channel_handler).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
    Req = cowboy_req:reply(200,
			   #{<<"content-type">> => <<"text/plain">>},
			   <<"Hello world!">>, Req0),
    {ok, Req, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {ok, Data, Req} = cowboy_req:read_body(Req0),
    Channel = jiffy:decode(Data),
    Req = cowboy_req:reply(201,
			   #{<<"content-type">> => <<"text/plain">>},
			   <<"Hello world!">>, Req0),
    {ok, Req, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405,
			   #{<<"allow">> => <<"GET, POST">>}, Req0),
    {ok, Req, State}.
