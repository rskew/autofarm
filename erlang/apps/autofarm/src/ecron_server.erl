-module(ecron_server).

-behaviour(gen_server).

-export([add_cronjob/3, delete_cronjob/1, list_cronjobs/0]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).

-define(TABLE_FILE, "ecrontab").
-define(TABLE_NAME, ecrontab).

% ecron_server:list_cronjobs().
% ecron_server:add_cronjob(hello, "*/20 * * * * *", {erlang, spawn, [fun() -> io:format("hello from ~p at ~p~n", [self(), calendar:now_to_datetime(now())]) end]}).
% ecron_server:delete_cronjob(hello).
% calendar:now_to_datetime(erlang:timestamp()).

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
    dets:open_file(?TABLE_NAME,
                   [{file, ?TABLE_FILE},
                    {type, set}
                   ]),
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
