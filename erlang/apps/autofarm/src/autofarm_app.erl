%%%-------------------------------------------------------------------
%% @doc autofarm public API
%% @end
%%%-------------------------------------------------------------------

-module(autofarm_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    autofarm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
