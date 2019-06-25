-module(channel_handler).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
    Channels = channel_registry_server:query(),
    Req = cowboy_req:reply(200,
			   #{<<"content-type">> => <<"application/json">>},
			   jiffy:encode(Channels), Req0),
    {ok, Req, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {ok, Data, Req} = cowboy_req:read_body(Req0),
    ChannelConfig = jiffy:decode(Data),
    ChannelId = uuid:get_v4(),
    {ok, Pid} = channel_sup:start_channel(ChannelId,
					  ChannelConfig),
    Req = cowboy_req:reply(201,
			   #{<<"content-type">> => <<"application/json">>},
			   jiffy:encode(#{id => ChannelId}), Req0),
    {ok, Req, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405,
			   #{<<"allow">> => <<"GET, POST">>}, Req0),
    {ok, Req, State}.
