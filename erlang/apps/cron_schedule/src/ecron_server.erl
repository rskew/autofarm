-module(ecron_server).

-behaviour(gen_server).

-export([add_cronjob/3, delete_cronjob/1, list_cronjobs/0]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(TABLE_NAME, ecrontab).

% ecron_server:list_cronjobs().
% ecron_server:add_cronjob(hello, "*/20 * * * * *", {erlang, spawn, [fun() -> io:format("hello from ~p at ~p~n", [self(), calendar:now_to_datetime(now())]) end]}).
% ecron_server:delete_cronjob(hello).
% calendar:now_to_datetime(erlang:timestamp()).
%
% Old watering schedule
% %%%ecron_server:add_cronjob(water_tommies_and_toprow, "00 15 * * *", {irrigation_controller, set_relay_on_for, [1, 1800]}).
% %%%ecron_server:add_cronjob(water_cuczucs, "30 15 * * *", {irrigation_controller, set_relay_on_for, [2, 1800]}).
% %%%ecron_server:add_cronjob(water_upper_pumpkins, "00 16 * * *", {irrigation_controller, set_relay_on_for, [3, 1800]}).
% %%%%%%%%%%%%%%ecron_server:add_cronjob(water_old_poly_tunnel, "30 18 * * *", {irrigation_controller, set_relay_on_for, [4, 1200]}).
%
% Just water things in closhes
% ecron_server:add_cronjob(water_cuczucs, "30 15 * * *", {irrigation_controller, set_relay_on_for, [2, 1800]}).
%irrigation_controller:set_relay_on_for(2, 2700).

%%%===================================================================
%%% API
%%%===================================================================

add_cronjob(Name, CronJob, Job) ->
    gen_server:call(?MODULE, {add_cronjob, Name, CronJob, Job}).

delete_cronjob(Name) ->
    gen_server:call(?MODULE, {delete_cronjob, Name}).

list_cronjobs() ->
    gen_server:call(?MODULE, {list_cronjobs}).

%%%===================================================================
%%% CallBack
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    TableFileName = os:getenv("AUTOFARM_ECRON_SERVER_ECRONTAB"),
    case filelib:ensure_dir(TableFileName) of
        ok -> true;
        {error, ReasonMkdir} -> exit(ReasonMkdir)
    end,
    case dets:open_file(?TABLE_NAME,
                        [{file, TableFileName},
                         {type, set}
                        ])
    of
      {ok, ?TABLE_NAME} -> true;
      {error, ReasonOpen} -> exit(ReasonOpen)
    end,
    [ecron:add(Name, Spec, Job) || {Name, Spec, Job} <- list_cronjobs_()],
    {ok, #{}}.

terminate(_Reason, _State) ->
    dets:close(?TABLE_NAME),
    ok.

handle_call({add_cronjob, Name, CronSpec, Job}, _From, State) ->
    case ecron:parse_spec(CronSpec, 1) of
      {error, invalid_spec, _} ->
            {reply, {error, invalid_spec, CronSpec}, State};
      {ok, _} ->
            dets:insert(?TABLE_NAME, {Name, CronSpec, Job}),
            ecron:delete(Name),
            ecron:add(Name, CronSpec, Job),
            {reply, ok, State}
    end;
handle_call({delete_cronjob, Name}, _From, State) ->
    dets:delete(?TABLE_NAME, Name),
    ecron:delete(Name),
    {reply, ok, State};
handle_call({list_cronjobs}, _From, State) ->
    CronJobs = list_cronjobs_(),
    {reply, CronJobs, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

list_cronjobs_() ->
    dets:match_object(?TABLE_NAME, {'_', '_', '_'}).
