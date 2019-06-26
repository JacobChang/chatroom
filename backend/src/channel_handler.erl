-module(channel_handler).

-include("channel.hrl").

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
    Channels = channel_registry_server:query(),
    Req = cowboy_req:reply(200,
			   #{<<"content-type">> => <<"application/json">>},
			   jiffy:encode(Channels), Req0),
    {ok, Req, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {ok, Data, Req} = cowboy_req:read_body(Req0),
    ChannelConfig = jiffy:decode(Data, [return_maps]),
    {ok, Title} = maps:find(<<"title">>, ChannelConfig),
    {ok, Duration} = maps:find(<<"duration">>,
			       ChannelConfig),
    {ok, MaxMembers} = maps:find(<<"max_members">>,
				 ChannelConfig),
    ChannelId = uuid:get_v4(),
    {ok, Pid} = channel_sup:start_channel(ChannelId,
					  #channel_config{title = Title,
							  duration = Duration,
							  max_members =
							      MaxMembers}),
    Req = cowboy_req:reply(201,
			   #{<<"content-type">> => <<"application/json">>},
			   jiffy:encode(#{id => ChannelId}), Req0),
    {ok, Req, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405,
			   #{<<"allow">> => <<"GET, POST">>}, Req0),
    {ok, Req, State}.
