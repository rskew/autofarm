-module(eventScheduler@foreign).

-export([initDb/0, updateScheduleImpl/2, removeScheduleImpl/1]).

-define(EVENT_SCHEDULE_TABLE_NAME, eventSchedule).
-define(EVENT_LOG_TABLE_NAME, eventLog).

initDb() ->
    fun() ->
        EventScheduleFileName = os:getenv("AUTOFARM_EVENT_SCHEDULER_TABLE_FILE"),
        EventLogFileName = os:getenv("AUTOFARM_EVENT_LOG_TABLE_FILE"),
        case dets:open_file(?EVENT_SCHEDULE_TABLE_NAME,
                            [{file, EventScheduleFileName},
                             {type, set}
                            ])
        of
          {ok, ?EVENT_SCHEDULE_TABLE_NAME} -> true;
          {error, ReasonOpen1} -> exit(ReasonOpen1)
        end,
        case dets:open_file(?EVENT_LOG_TABLE_NAME,
                            [{file, EventLogFileName},
                             {type, set}
                            ])
        of
          {ok, ?EVENT_LOG_TABLE_NAME} -> true;
          {error, ReasonOpen2} -> exit(ReasonOpen2)
        end
    end.

updateScheduleImpl(Key,
                   EventSchedule=#{action := _Action,
                                   runAfter := _RunAfter,
                                   runBefore := _RunBefore,
                                   timeoutSeconds := _TimeoutSeconds,
                                   errorHandler := _ErrorHandler,
                                   everyDays := _EveryDays}) ->
    fun() ->
        dets:insert(?EVENT_SCHEDULE_TABLE_NAME, {Key, EventSchedule})
    end.

removeScheduleImpl(Key) ->
    fun() ->
        dets:delete(?EVENT_SCHEDULE_TABLE_NAME, Key)
    end.
