%%%-------------------------------------------------------------------
%% @doc cron_schedule public API
%% @end
%%%-------------------------------------------------------------------

-module(cron_schedule_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cron_schedule_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
