-module(pump_controller).

-behaviour(gen_statem).

-export([reboot_device/1, write_file_to_device/3, copy_file_on_device/3, delete_file_on_device/2,
         flash_lights/1, pump_on/2, pump_off/1]).

-export([start_link/1, init/1, callback_mode/0, terminate/3]).
-export([waiting/3, connected/3]).

-define(MODBIN, <<"pump_controller">>).

%%%===================================================================
%%% API
%%%===================================================================

flash_lights(DeviceID) ->
    gproc:send({n, l, {?MODBIN, DeviceID}}, flash_lights).

reboot_device(DeviceID) ->
    gproc:send({n, l, {?MODBIN, DeviceID}}, reboot_device).

say_hello(DeviceID) ->
    gproc:send({n, l, {?MODBIN, DeviceID}}, say_hello).

write_file_to_device(DeviceID, LocalFileName, DeviceFileName) ->
    gproc:send({n, l, {?MODBIN, DeviceID}}, {write_file, LocalFileName, DeviceFileName}).

copy_file_on_device(DeviceID, CopyFromFileName, CopyToFileName) ->
    gproc:send({n, l, {?MODBIN, DeviceID}}, {copy_file, CopyFromFileName, CopyToFileName}).

delete_file_on_device(DeviceID, FileName) ->
    gproc:send({n, l, {?MODBIN, DeviceID}}, {delete_file, FileName}).

pump_on(DeviceID, DurationSeconds) ->
    gproc:send({n, l, {?MODBIN, DeviceID}}, {pump_on, DurationSeconds}).

pump_off(DeviceID) ->
    gproc:send({n, l, {?MODBIN, DeviceID}}, pump_off).

%%%===================================================================
%%% CallBack
%%%===================================================================

start_link(ID) ->
    gen_statem:start_link({via, gproc, {n, l, {?MODBIN, ID}}}, ?MODULE, ID, []).

callback_mode() ->
    [state_functions, state_enter].

init(ID) ->
    {ok, waiting, #{device_id => ID, connection => false}}.

terminate(Reason, _State, #{connection := Socket}) ->
    gen_tcp:close(Socket),
    io:format("Termination: ~p~n", [Reason]).

%%%===================================================================
%%% State Handlers
%%%===================================================================

connected(enter, _OldState, _Data) ->
    keep_state_and_data;

connected(info, {tcp, _Socket, Packet}, Data=#{parser_state := ParserState}) ->
    NewParserState = device_monitor:parse_packet(self(), Packet, ParserState),
    {keep_state, Data#{parser_state => NewParserState}};

connected(cast, {message, Message}, _Data=#{device_id := DeviceID}) ->
    io:format("Received message from staging_device ~p:~n~p~n", [DeviceID, Message]),
    keep_state_and_data;

connected(info, {tcp_closed, OldSocket}, Data) ->
    io:format("Connection closed ~p~n", [OldSocket]),
    {next_state, waiting, Data};

connected(info, {tcp_error, OldSocket, etimedout}, _Data) ->
    io:format("Connection timeout ~p~n", [OldSocket]),
    keep_state_and_data;

connected(info, reboot_device, #{connection := Socket}) ->
    io:format("Sending reboot command~n"),
    gen_tcp:send(Socket, device_monitor:format_command(<<"reboot">>, #{})),
    keep_state_and_data;

connected(info, {write_file, LocalFileName, DeviceFileName}, #{connection := Socket}) ->
    case file:read_file(LocalFileName) of
        {ok, Content} ->
            io:format("Writing file ~p to device file ~p with digest ~p~n", [LocalFileName, DeviceFileName, lists:sum(binary_to_list(Content))]),
            gen_tcp:send(Socket, [device_monitor:format_command(<<"writeFile">>, #{<<"fileName">> => DeviceFileName, <<"fileLength">> => byte_size(Content)}),
                                  device_monitor:format_raw_message(Content)]);
        {error, Reason} ->
            io:format("Could not read application file: ~p~n", [Reason])
    end,
    keep_state_and_data;

connected(info, {copy_file, CopyFromFileName, CopyToFileName}, #{connection := Socket}) ->
    io:format("Copying file on device from ~p to ~p~n", [CopyFromFileName, CopyToFileName]),
    gen_tcp:send(Socket, device_monitor:format_command(<<"copyFile">>, #{<<"copyFromFileName">> => CopyFromFileName, <<"copyToFileName">> => CopyToFileName})),
    keep_state_and_data;

connected(info, {delete_file, FileName}, #{connection := Socket}) ->
    io:format("Deleting file on device ~p~n", [FileName]),
    gen_tcp:send(Socket, device_monitor:format_command(<<"deleteFile">>, #{<<"fileName">> => FileName})),
    keep_state_and_data;

connected(info, flash_lights, #{connection := Socket}) ->
    io:format("Sending flash-lights command~n"),
    gen_tcp:send(Socket, device_monitor:format_command(<<"flash">>, #{})),
    keep_state_and_data;

connected(info, {pump_on, DurationSeconds}, #{connection := Socket}) ->
    io:format("Sending command to turn pump on for ~p seconds~n", [DurationSeconds]),
    gen_tcp:send(Socket, device_monitor:format_command(<<"pumpOn">>, #{durationSeconds => DurationSeconds})),
    keep_state_and_data;

connected(info, pump_off, #{connection := Socket}) ->
    io:format("Sending command to turn pump off~n"),
    gen_tcp:send(Socket, device_monitor:format_command(<<"pumpOff">>, #{})),
    keep_state_and_data;

connected(Event, EventContent, Data) -> handle_common(connected, Event, EventContent, Data).

waiting(enter, _OldState, _Data) ->
    keep_state_and_data;

waiting(Event, EventContent, Data) -> handle_common(waiting, Event, EventContent, Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_common(_State, cast, {connection, Socket, InitialPacket}, Data=#{connection := OldSocket}) ->
    io:format("New connection ~p~n", [Socket]),
    if OldSocket =/= false ->
           io:format("Closing old connection ~p~n", [OldSocket]),
           gen_tcp:close(OldSocket);
       true -> true
    end,
    inet:setopts(Socket, [{active, true}]),
    {next_state, connected, Data#{connection => Socket, parser_state => device_monitor:empty_parser_state()},
     [{next_event, info, {tcp, Socket, InitialPacket}}]};

handle_common(State, Event, EventContent, _Data) ->
    io:format("Unhandled event ~p in state '~p' with content: ~p~n", [Event, State, EventContent]),
    keep_state_and_data.
