-module(device_monitor).

-behaviour(gen_statem).

-export([flash_lights/0, reboot_device/0, activate_solenoid/2, update_device_app/0]).

-export([start_link/0, init/1, callback_mode/0, handle_common/3, terminate/3]).
-export([waiting/3, connected/3]).
-export([listen/2]).

-define(PORT, 9222).

%%%===================================================================
%%% API
%%%===================================================================

flash_lights() ->
    gen_server:cast(?MODULE, flash_lights).

reboot_device() ->
    gen_server:cast(?MODULE, reboot_device).

activate_solenoid(SolenoidId, DurationSeconds) ->
    gen_server:cast(?MODULE, {activate_solenoid, SolenoidId, DurationSeconds}).

update_device_app() ->
    gen_server:cast(?MODULE, update_device_app).

%%%===================================================================
%%% CallBack
%%%===================================================================

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, ?PORT, []).


callback_mode() ->
    [state_functions, state_enter].


init(Port) ->
    io:format("Listening on port ~p~n", [Port]),
    {ok, Listen} = gen_tcp:listen(Port, [{active, false}]),
    {ok, waiting, #{listen => Listen}}.


terminate(Reason, _State, #{connection := Socket, listen := Listen}) ->
    gen_tcp:close(Listen),
    gen_tcp:close(Socket),
    io:format("Termination: ~p~n", [Reason]);

terminate(Reason, _State, #{listen := Listen}) ->
    gen_tcp:close(Listen),
    io:format("Termination: ~p~n", [Reason]).

%%%===================================================================
%%% State Handlers
%%%===================================================================

connected(enter, _OldState, _Data) -> keep_state_and_data;

connected(info, {tcp, _Socket, Message}, _Data) ->
    io:format("Received message ~p~n", [Message]),
    keep_state_and_data;

connected(info, {tcp_closed, _OldSocket}, Data=#{listen := _Listen}) ->
    io:format("Connection closed~n"),
    {next_state, waiting, Data};

connected(cast, flash_lights, #{connection := Socket}) ->
    io:format("Sending flash-lights command~n"),
    gen_tcp:send(Socket, [<<"flash">>, <<0>>]),
    keep_state_and_data;

connected(cast, reboot_device, #{connection := Socket}) ->
    io:format("Sending reboot command~n"),
    gen_tcp:send(Socket, [<<"reboot">>, <<0>>]),
    keep_state_and_data;

connected(cast, {activate_solenoid, SolenoidId, DurationSeconds}, #{connection := Socket}) ->
    io:format("Sending command to activate solenoid ~p for ~p seconds~n", [SolenoidId, DurationSeconds]),
    SolenoidIdStr = io_lib:format("~3..0B", [SolenoidId]),
    DurationSecondsStr = io_lib:format("~5..0B", [DurationSeconds]),
    gen_tcp:send(Socket, [<<"activate_solenoid">>, SolenoidIdStr, DurationSecondsStr, <<0>>]),
    keep_state_and_data;

connected(cast, update_device_app, #{connection := Socket}) ->
    case file:read_file("../hello_ota.js") of
        {ok, Content} ->
            io:format("Updating device application~n"),
            gen_tcp:send(Socket, [<<"update">>, <<0>>, integer_to_list(byte_size(Content)), <<0>>, Content, <<0>>]);
        {error, Reason} ->
            io:format("Could not read application file: ~p~n", [Reason])
    end,
    keep_state_and_data;

connected(Event, EventContent, Data) -> handle_common(Event, EventContent, Data).


waiting(enter, _OldState, _Data=#{listen := Listen}) ->
    spawn_link(?MODULE, listen, [Listen, self()]),
    keep_state_and_data;

waiting(cast, {connection, Socket}, Data) ->
    io:format("New connection~n"),
    ok = inet:setopts(Socket, [{active, true}]),
    {next_state, connected, Data#{connection => Socket}};

waiting(Event, EventContent, Data) -> handle_common(Event, EventContent, Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_common(Event, EventContent, _Data) ->
    io:format("Unhandled event: ~p ~p~n", [Event, EventContent]),
    keep_state_and_data.


listen(Listen, Receiver) ->
    % gen_tcp:accept blocks until a connection arrives on Listen socket
    case gen_tcp:accept(Listen) of
        {ok, Accept} ->
            ok = gen_tcp:controlling_process(Accept, Receiver),
            gen_statem:cast(?MODULE, {connection, Accept});
        {error, Reason} ->
            io:format("Could not listen on socket: ~p~n", [Reason])
    end.
