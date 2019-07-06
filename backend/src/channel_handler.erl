-module(channel_handler).

-include("channel.hrl").

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
    Channels = channel_registry_server:list(),
    ChannelsList = maps:to_list(Channels),
    Body = lists:map(fun ({ChannelId, {Channel, _Pid}}) ->
			     #{id =>
				   uuid:uuid_to_string(ChannelId,
						       binary_standard),
			       title => Channel#channel_config.title,
			       duration => Channel#channel_config.duration,
			       max_members =>
				   Channel#channel_config.max_members}
		     end,
		     ChannelsList),
    Req1 = cowboy_req:reply(200,
			    #{<<"content-type">> => <<"application/json">>},
			    jiffy:encode(Body), Req0),
    {ok, Req1, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
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
    Id = uuid:uuid_to_string(ChannelId, binary_standard),
    Req2 = cowboy_req:reply(201,
			    #{<<"content-type">> => <<"application/json">>},
			    jiffy:encode(#{id => Id}), Req1),
    {ok, Req2, State};
init(Req0, State) ->
    Req1 = cowboy_req:reply(405,
			    #{<<"allow">> => <<"GET, POST">>}, Req0),
    {ok, Req1, State}.
