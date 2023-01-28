-module(irrigation_controller).

-behaviour(gen_server).

-export([set_relay/2, set_relay_on_for/2]).

-export([start_link/0, init/1, handle_info/2, handle_cast/2, handle_call/3]).

%%%===================================================================
%%% API
%%%===================================================================

set_relay(RelayID, RelayState) ->
    erlang:send(?MODULE, {set_relay, RelayID, RelayState}).

set_relay_on_for(RelayID, DurationSeconds) ->
    gen_server:cast(?MODULE, {set_relay_on_for, RelayID, DurationSeconds}).

%%%===================================================================
%%% CallBack
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Data = #{1 => 0, 2 => 0, 3 => 0, 4 => 0},
    bang(Data),
    {ok, Data}.

handle_info({set_relay, RelayID, RelayState}, Data) ->
    UpdatedData = Data#{RelayID => RelayState},
    io:format("Setting relay ~p to ~p~n", [RelayID, RelayState]),
    bang(UpdatedData),
    {noreply, UpdatedData};

% If ActionRef is supplied, it must match the recorded values in the process state.
% This enables setting actions to happen later that can be overriden by newer actions,
% e.g. if a relay is scheduled to turn off via `time:send_after`, then if that relay is re-scheduled,
% the older off action won't have any effect because the ActionRef in the state will have changed.
handle_info({set_relay, RelayID, RelayState, ActionRef}, Data) ->
    #{{action, RelayID} := StateActionRef} = Data,
    if StateActionRef == ActionRef ->
            UpdatedData = Data#{RelayID => RelayState},
            io:format("Setting relay ~p to ~p~n", [RelayID, RelayState]),
            bang(UpdatedData),
            {noreply, UpdatedData};
        true ->
            {noreply, Data}
    end;

handle_info(Message, Data) ->
    io:format("Unhandled info: ~p~n", Message),
    {noreply, Data}.

handle_cast({set_relay_on_for, RelayID, DurationSeconds}, Data) ->
    ActionRef = rand:bytes(32),
    timer:send_after(DurationSeconds * 1000, {set_relay, RelayID, 0, ActionRef}),
    erlang:send(?MODULE, {set_relay, RelayID, 1}),
    {noreply, Data#{{action, RelayID} => ActionRef}};

handle_cast(Message, Data) ->
    io:format("Unhandled cast: ~p~n", Message),
    {noreply, Data}.

handle_call(Message, _From, Data) ->
    io:format("Unhandled call: ~p~n", Message),
    {noreply, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

bang(#{1 := Relay1State, 2 := Relay2State, 3 := Relay3State, 4 := Relay4State}) ->
    os:cmd(io_lib:format("$bang ~B ~B ~B ~B", [Relay1State, Relay2State, Relay3State, Relay4State])).
