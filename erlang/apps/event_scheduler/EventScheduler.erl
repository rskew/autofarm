-module(eventScheduler@foreign).

-export([initDb/0, updateScheduleImpl/1]).

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
          {error, ReasonOpen} -> exit(ReasonOpen)
        end,
        case dets:open_file(?EVENT_LOG_TABLE_NAME,
                            [{file, EventLogFileName},
                             {type, set}
                            ])
        of
          {ok, ?EVENT_LOG_TABLE_NAME} -> true;
          {error, ReasonOpen} -> exit(ReasonOpen)
        end
    end.

updateScheduleImpl(EventSchedule=#{name := Name,
                                   action := Action,
                                   runAfter := RunAfter,
                                   runBefore := RunBefore,
                                   timeoutSeconds := TimeoutSeconds,
                                   errorHandler := ErrorHandler,
                                   everyDays := EveryDays}) ->
  dets:insert(?EVENT_SCHEDULE_TABLE_NAME, {Name, EventSchedule}).
