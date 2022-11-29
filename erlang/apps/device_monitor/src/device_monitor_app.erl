%%%-------------------------------------------------------------------
%% @doc device_monitor public API
%% @end
%%%-------------------------------------------------------------------

-module(device_monitor_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    device_monitor_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
