-module(frontend_server_websocket_handler).

-export([init/2, websocket_handle/2, websocket_info/2]).

init(Req, State) ->
    Peer = cowboy_req:peer(Req),
    io:format("New websocket connection from ~p~n", [Peer]),
    {cowboy_websocket, Req, State}.

websocket_handle({text, <<"Authorization: Bearer ", ClientToken/binary>>}, State=#{token := ReferenceToken, authorized := false}) ->
    if ClientToken =:= ReferenceToken ->
        {[{text, <<"Authorization Successful">>}], State#{authorized => true}};
    true ->
        {[
          {text, <<"Authorization Failed">>},
          {close, 1002, <<"Authorization Failed">>}
         ], State}
    end;
websocket_handle(Frame={text, _}, State=#{authorized := true}) ->
    % echo
    {[Frame], State};
websocket_handle(_Frame, State) ->
    {[], State}.

websocket_info({reply, text, Text}, State) ->
    {[{text, Text}], State};
websocket_info(_Info, State) ->
    {[], State}.
