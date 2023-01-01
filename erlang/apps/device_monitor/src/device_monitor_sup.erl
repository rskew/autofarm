%%%-------------------------------------------------------------------
%% @doc device_monitor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(device_monitor_sup).

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
    ChildSpecs = [#{id => device_listener,
                    start => {device_listener, start_link, []},
                    restart => permanent,
                    modules => [device_listener]},
                  #{id => irrigation_controller_0,
                    start => {irrigation_controller, start_link, [0]},
                    restart => permanent,
                    modules => [irrigation_controller]},
                  #{id => tank_monitor_0,
                    start => {tank_monitor, start_link, [0]},
                    restart => permanent,
                    modules => [tank_monitor]},
                  #{id => staging_device_0,
                    start => {staging_device, start_link, [0]},
                    restart => permanent,
                    modules => [staging_device]}
                  ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
