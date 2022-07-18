%%%-------------------------------------------------------------------
%% @doc supervisor for the websocket handler for the UI.
%% @end
%%%-------------------------------------------------------------------

-module(ui_websocket_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    %ChildSpecs = [#{id => ui_websocket_handler,
    %                start => {ui_websocket_handler, start_link, []},
    %                restart => permanent,
    %                modules => [ui_websocket_handler]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
