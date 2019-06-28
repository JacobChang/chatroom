-module(websocket_handler).

-export([init/2]).

-export([websocket_init/1]).

-export([websocket_handle/2]).

-export([websocket_info/2]).

init(Req, Opts) -> {cowboy_websocket, Req, Opts}.

websocket_init(State) -> {ok, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, <<Msg/binary>>}, State};
websocket_handle(_Data, State) -> {ok, State}.

websocket_info(_Info, State) -> {ok, State}.
