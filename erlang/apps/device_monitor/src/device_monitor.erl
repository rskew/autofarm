-module(device_monitor).

-behaviour(gen_statem).

-export([reboot_device/0, write_file_to_device/2, copy_file_on_device/2, delete_file_on_device/1]).
-export([flash_lights/0, activate_solenoid/2]).

-export([start_link/0, init/1, callback_mode/0, terminate/3]).
-export([waiting/3, connected/3]).


%%%===================================================================
%%% API
%%%===================================================================

flash_lights() ->
    gen_statem:cast(?MODULE, flash_lights).

reboot_device() ->
    gen_statem:cast(?MODULE, reboot_device).

activate_solenoid(SolenoidId, DurationSeconds) ->
    gen_statem:cast(?MODULE, {activate_solenoid, SolenoidId, DurationSeconds}).

write_file_to_device(LocalFileName, DeviceFileName) ->
    gen_statem:cast(?MODULE, {write_file, LocalFileName, DeviceFileName}).

copy_file_on_device(CopyFromFileName, CopyToFileName) ->
    gen_statem:cast(?MODULE, {copy_file, CopyFromFileName, CopyToFileName}).

delete_file_on_device(FileName) ->
    gen_statem:cast(?MODULE, {delete_file, FileName}).

%%%===================================================================
%%% CallBack
%%%===================================================================

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, 0, []).


callback_mode() ->
    [state_functions, state_enter].


init(ID) ->
    gproc:register_name({n, l, {<<"device_monitor">>, ID}}, self()),
    {ok, waiting, #{}}.


terminate(Reason, _State, #{connection := Socket}) ->
    gen_tcp:close(Socket),
    io:format("Termination: ~p~n", [Reason]).

%%%===================================================================
%%% State Handlers
%%%===================================================================

connected(enter, _OldState, _Data) -> keep_state_and_data;

connected(info, {tcp, _Socket, Packet}, Data=#{partial_message := PartialMessage}) ->
    %io:format("Received tcp packet ~p~n", [Packet]),
    Packet1 = <<PartialMessage/binary, Packet/binary>>,
    Messages1 = binary:split(Packet1, <<0>>, [global]),
    NewPartialMessage = lists:last(Messages1), % This is <<>> if there is no partial message
    Messages2 = lists:droplast(Messages1),
    %io:format("Received messages ~p~n", [Messages2]),
    lists:map(fun(Message) ->
                  case jsone:try_decode(Message) of
                      {ok, MessageMap, <<>>} ->
                          gen_statem:cast(?MODULE, {message, MessageMap});
                      {error, _} ->
                          io:format("Couldn't decode message ~p~n", [Message])
                  end
              end,
              Messages2),
    {keep_state, Data#{partial_message := NewPartialMessage}};

connected(cast, {message, Message}, _Data) ->
    io:format("Received message ~p~n", [Message]),
    keep_state_and_data;

connected(info, {tcp_closed, OldSocket}, Data) ->
    io:format("Connection closed ~p~n", [OldSocket]),
    {next_state, waiting, Data};

connected(info, {tcp_error, OldSocket, etimedout}, _Data) ->
    io:format("Connection timeout ~p~n", [OldSocket]),
    keep_state_and_data;

connected(cast, reboot_device, #{connection := Socket}) ->
    io:format("Sending reboot command~n"),
    gen_tcp:send(Socket, [jsone:encode([<<"reboot">>, #{}]), <<0>>]),
    keep_state_and_data;

connected(cast, {write_file, LocalFileName, DeviceFileName}, #{connection := Socket}) ->
    case file:read_file(LocalFileName) of
        {ok, Content} ->
            io:format("Writing file ~p to device file ~p with digest ~p~n", [LocalFileName, DeviceFileName, lists:sum(binary_to_list(Content))]),
            gen_tcp:send(Socket, [jsone:encode([<<"writeFile">>, #{<<"fileName">> => DeviceFileName, <<"fileLength">> => byte_size(Content)}]), <<0>>, Content, <<0>>]);
        {error, Reason} ->
            io:format("Could not read application file: ~p~n", [Reason])
    end,
    keep_state_and_data;

connected(cast, {copy_file, CopyFromFileName, CopyToFileName}, #{connection := Socket}) ->
    io:format("Copying file on device from ~p to ~p~n", [CopyFromFileName, CopyToFileName]),
    gen_tcp:send(Socket, [jsone:encode([<<"copyFile">>, #{<<"copyFromFileName">> => CopyFromFileName, <<"copyToFileName">> => CopyToFileName}]), <<0>>]),
    keep_state_and_data;

connected(cast, {delete_file, FileName}, #{connection := Socket}) ->
    io:format("Deleting file on device ~p~n", [FileName]),
    gen_tcp:send(Socket, [jsone:encode([<<"deleteFile">>, #{<<"fileName">> => FileName}]), <<0>>]),
    keep_state_and_data;

connected(cast, flash_lights, #{connection := Socket}) ->
    io:format("Sending flash-lights command~n"),
    gen_tcp:send(Socket, [jsone:encode([<<"flash">>, #{}]), <<0>>]),
    keep_state_and_data;

connected(cast, {activate_solenoid, SolenoidId, DurationSeconds}, #{connection := Socket}) ->
    io:format("Sending command to activate solenoid ~p for ~p seconds~n", [SolenoidId, DurationSeconds]),
    gen_tcp:send(Socket, [jsone:encode([<<"activateSolenoid">>, #{<<"solenoidID">> => SolenoidId, <<"durationSeconds">> => DurationSeconds}]), <<0>>]),
    keep_state_and_data;

connected(Event, EventContent, Data) -> handle_common(connected, Event, EventContent, Data).


waiting(enter, _OldState, _Data) ->
    keep_state_and_data;

waiting(cast, {connection, Socket, InitialPacket}, Data) ->
    io:format("New connection~n"),
    inet:setopts(Socket, [{active, true}]),
    {next_state, connected, Data#{connection => Socket, partial_message => <<>>},
     [{next_event, info, {tcp, Socket, InitialPacket}}]};

waiting(Event, EventContent, Data) -> handle_common(waiting, Event, EventContent, Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_common(State, Event, EventContent, _Data) ->
    io:format("Unhandled event ~p in state '~p' with content: ~p~n", [Event, State, EventContent]),
    keep_state_and_data.
