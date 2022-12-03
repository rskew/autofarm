-module(device_listener).

-behaviour(gen_server).

-export([start_link/0, init/1]).
-export([handle_call/3, handle_cast/2]).

-export([listener/0, acceptor/1, dispatcher/1]).

-define(PORT, 9222).
-define(DISPATCH_TIMEOUT, 10000).

start_link() ->
    io:format("hello start link"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("hello init"),
    spawn_link(?MODULE, listener, []),
    {ok, #{}}.

listener() ->
    case gen_tcp:listen(?PORT, [{active, false}, binary, {keepalive, true}, {reuseaddr, true}, {nodelay, true},
                                % https://github.com/torvalds/linux/blob/master/include/uapi/linux/tcp.h#L91
                                %{raw, 6, 18, <<1:64/native>>}, % TCP_USER_TIMEOUT response timeout on send (including keepalives)
                                {raw, 6, 4, <<1:64/native>>},  % TCP_KEEPIDLE time before first keepalive
                                {raw, 6, 5, <<1:64/native>>},  % TCP_KEEPINTVL interval between keepalives
                                {raw, 6, 6, <<2:64/native>>}   % TCP_KEEPCNT number of keepalives before death
    ]) of
        {ok, Listen} ->
            io:format("Listening on port ~p~n", [?PORT]),
            acceptor(Listen);
        {error, Reason} ->
            io:format("Couldn't listen on port ~p ~p~n", [?PORT, Reason]),
            timer:sleep(5000),
            listener()
    end.

acceptor(Listen) ->
    % gen_tcp:accept blocks until a connection arrives on Listen socket
    case gen_tcp:accept(Listen) of
        {ok, Accept} ->
            io:format("Accepted connection~n"),
            Dispatcher = spawn_link(?MODULE, dispatcher, [Accept]),
            ok = gen_tcp:controlling_process(Accept, Dispatcher);
        {error, Reason} ->
            io:format("Could not accept connection: ~p~n", [Reason])
    end,
    acceptor(Listen).

dispatcher(Socket) ->
    case gen_tcp:recv(Socket, 0, ?DISPATCH_TIMEOUT) of
        {ok, Packet} ->
            [FirstMessage,RestOfPacket|[]] = binary:split(Packet, <<0>>),
            case jsone:try_decode(FirstMessage) of
                {ok, [<<"connect">>, #{<<"type">> := DeviceType, <<"id">> := DeviceID}], <<>>} ->
                    case gproc:lookup_pids({n, l, {DeviceType, DeviceID}}) of
                        [HandlerPid] ->
                            gen_statem:cast(HandlerPid, {connection, Socket, RestOfPacket}),
                            ok = gen_tcp:controlling_process(Socket, HandlerPid),
                            io:format("Dispatched connection to ~p ~p~n", [DeviceType, DeviceID]);
                        [] ->
                            io:format("Couldn't find handler. TODO create if not exist? ~p ~p~n", [DeviceType, DeviceID]);
                        _ ->
                            io:format("Error: multiple pids registered with ~p ~p~n", [DeviceType, DeviceID])
                    end;
                Error ->
                    io:format("Couldn't dispatch connection ~p: ~p~n", [Socket, Error])
            end;
        Error ->
            io:format("Error on socket ~p: ~p~n", [Socket, Error])
    end.

handle_call(Message, _From, Data) ->
    io:format("Unhandled call: ~p~n", Message),
    {noreply, Data}.

handle_cast(Message, Data) ->
    io:format("Unhandled cast: ~p~n", Message),
    {noreply, Data}.