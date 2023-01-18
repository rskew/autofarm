-module(tank_monitor).

-behaviour(gen_statem).

-export([reboot_device/1, stay_awake/1, write_file_to_device/3, copy_file_on_device/3, delete_file_on_device/2,
         flash_lights/1, read_tank_level_sensor/1]).

-export([start_link/1, init/1, callback_mode/0, terminate/3]).
-export([waiting/3, connected/3]).

-define(MODBIN, <<"tank_monitor">>).

%%%===================================================================
%%% API
%%%===================================================================

flash_lights(DeviceID) ->
    gen_statem:cast({via, gproc, {n, l, {?MODBIN, DeviceID}}}, flash_lights).

reboot_device(DeviceID) ->
    gen_statem:cast({via, gproc, {n, l, {?MODBIN, DeviceID}}}, reboot_device).

read_tank_level_sensor(DeviceID) ->
    gen_statem:cast({via, gproc, {n, l, {?MODBIN, DeviceID}}}, read_tank_level_sensor).

stay_awake(DeviceID) ->
    gen_statem:cast({via, gproc, {n, l, {?MODBIN, DeviceID}}}, stay_awake).

write_file_to_device(DeviceID, LocalFileName, DeviceFileName) ->
    gen_statem:cast({via, gproc, {n, l, {?MODBIN, DeviceID}}}, {write_file, LocalFileName, DeviceFileName}).

copy_file_on_device(DeviceID, CopyFromFileName, CopyToFileName) ->
    gen_statem:cast({via, gproc, {n, l, {?MODBIN, DeviceID}}}, {copy_file, CopyFromFileName, CopyToFileName}).

delete_file_on_device(DeviceID, FileName) ->
    gen_statem:cast({via, gproc, {n, l, {?MODBIN, DeviceID}}}, {delete_file, FileName}).

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

connected(cast, {message, Message=[<<"update_state">>, #{<<"battery_voltage">> := BatteryVoltageReading}]}, _Data=#{device_id := DeviceID}) ->
    %io:format("Received battery reading message from tank_monitor ~p:~n~p~n", [DeviceID, Message]),
    InfluxdbPort = list_to_integer(os:getenv("AUTOFARM_DEVICE_MONITOR_INFLUXDB_PORT")),
    post_reading_to_influxdb("http", "localhost", InfluxdbPort, ?MODBIN, "battery_voltage_volts", io_lib:format("~f", [BatteryVoltageReading])),
    keep_state_and_data;

connected(cast,
          {message, Message=[<<"update_state">>, #{<<"tank_level_meters">> := TankLevelReading,
                                                   <<"tank_float_switch_lower">> := LowerFloatSwitchPosition,
                                                   <<"tank_float_switch_upper">> := UpperFloatSwitchPosition}]},
          _Data=#{device_id := DeviceID}) ->
    %io:format("Received tank-level message from tank_monitor ~p:~n~p~n", [DeviceID, Message]),
    InfluxdbPort = list_to_integer(os:getenv("AUTOFARM_DEVICE_MONITOR_INFLUXDB_PORT")),
    post_reading_to_influxdb("http", "localhost", InfluxdbPort, ?MODBIN, "tank_level_meters", io_lib:format("~f", [TankLevelReading])),
    post_reading_to_influxdb("http", "localhost", InfluxdbPort, ?MODBIN, "lower_float_switch_up", if LowerFloatSwitchPosition == <<"down">> -> "0"; true -> "1" end),
    post_reading_to_influxdb("http", "localhost", InfluxdbPort, ?MODBIN, "upper_float_switch_up", if UpperFloatSwitchPosition == <<"down">> -> "0"; true -> "1" end),
    keep_state_and_data;

connected(cast, {message, Message}, _Data=#{device_id := DeviceID}) ->
    io:format("Received unhandled message from tank_monitor ~p:~n~p~n", [DeviceID, Message]),
    keep_state_and_data;

connected(info, {tcp_closed, OldSocket}, Data) ->
    io:format("Connection closed ~p~n", [OldSocket]),
    {next_state, waiting, Data};

connected(info, {tcp_error, OldSocket, etimedout}, _Data) ->
    io:format("Connection timeout ~p~n", [OldSocket]),
    keep_state_and_data;

connected(cast, reboot_device, #{connection := Socket}) ->
    io:format("Sending reboot command~n"),
    gen_tcp:send(Socket, device_monitor:format_command(<<"reboot">>, #{})),
    keep_state_and_data;

connected(cast, stay_awake, #{connection := Socket}) ->
    io:format("Sending stay-awake command~n"),
    gen_tcp:send(Socket, device_monitor:format_command(<<"stayAwake">>, #{})),
    keep_state_and_data;

connected(cast, {write_file, LocalFileName, DeviceFileName}, #{connection := Socket}) ->
    case file:read_file(LocalFileName) of
        {ok, Content} ->
            io:format("Writing file ~p to device file ~p with digest ~p~n", [LocalFileName, DeviceFileName, lists:sum(binary_to_list(Content))]),
            gen_tcp:send(Socket, [device_monitor:format_command(<<"writeFile">>, #{<<"fileName">> => DeviceFileName, <<"fileLength">> => byte_size(Content)}),
                                  device_monitor:format_raw_message(Content)]);
        {error, Reason} ->
            io:format("Could not read application file: ~p~n", [Reason])
    end,
    keep_state_and_data;

connected(cast, {copy_file, CopyFromFileName, CopyToFileName}, #{connection := Socket}) ->
    io:format("Copying file on device from ~p to ~p~n", [CopyFromFileName, CopyToFileName]),
    gen_tcp:send(Socket, device_monitor:format_command(<<"copyFile">>, #{<<"copyFromFileName">> => CopyFromFileName, <<"copyToFileName">> => CopyToFileName})),
    keep_state_and_data;

connected(cast, {delete_file, FileName}, #{connection := Socket}) ->
    io:format("Deleting file on device ~p~n", [FileName]),
    gen_tcp:send(Socket, device_monitor:format_command(<<"deleteFile">>, #{<<"fileName">> => FileName})),
    keep_state_and_data;

connected(cast, flash_lights, #{connection := Socket}) ->
    io:format("Sending flash-lights command~n"),
    gen_tcp:send(Socket, device_monitor:format_command(<<"flash">>, #{})),
    keep_state_and_data;

connected(cast, read_tank_level_sensor, #{connection := Socket}) ->
    io:format("Sending command to read tank level sensor~n"),
    gen_tcp:send(Socket, device_monitor:format_command(<<"readTankLevelSensor">>, #{})),
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

post_reading_to_influxdb(Scheme, Hostname, Port, DatabaseName, SeriesName, Reading) ->
    Now = os:system_time(nanosecond),
    Path = iolist_to_binary([<<"api/v2/write?bucket=">>, DatabaseName]),
    Message = iolist_to_binary([SeriesName, " value=", Reading, " ", integer_to_list(Now)]),
    httpPost(Scheme, Hostname, Port, Path, Message).

httpPost(Scheme, Hostname, Port, Path, Message) ->
    Url = iolist_to_binary([Scheme, <<"://">>, Hostname, <<":">>, integer_to_binary(Port), <<"/">>, Path]),
    %io:format("sending message ~p~n", [Message]),
    Blah = spawn(fun() ->
                         receive {http, {_, {{_, ResponseCode, _}, _, _}}} ->
                                     true
                                     %io:format("Received response ~p~n", [ResponseCode])
                         end
                 end),
    case httpc:request(post,
                       {Url,
                        [],
                        "application/octet-stream",
                        Message},
                       [],
                       [{sync, false},
                        {receiver, Blah}
                        ])
    of
        {ok, _RequestId} ->
            true;
        {error, Reason} ->
            io:format("problem sending http request: ~p~n", [Reason])
    end.
