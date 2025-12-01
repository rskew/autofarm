import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{type Option, Some, None}
import gleam/string
import gleam/time/calendar
import gleam/time/duration.{type Duration, seconds}
import shared/day_of_week.{type DayOfWeek, Monday}

pub type ScheduledAction {
  ScheduledActionOnOnce(
    target_name: String,
    start_time: calendar.TimeOfDay,
    day_of_week: DayOfWeek,
    duration: Duration)
  ScheduledActionOnRecurring(
    target_name: String,
    start_time: calendar.TimeOfDay,
    day_of_week: Option(DayOfWeek),
    duration: Duration)
}

pub fn zero() -> ScheduledAction {
  ScheduledActionOnOnce("", calendar.TimeOfDay(0, 0, 0, 0), Monday, seconds(0))
}

pub fn decoder() {
  use target_name <- decode.field(0, decode.string)
  use start_time_hour <- decode.field(1, decode.int)
  use start_time_minute <- decode.field(2, decode.int)
  use day_of_week <- decode.field(3, decode.optional(day_of_week.decoder()))
  use duration_seconds <- decode.field(4, decode.int)
  use recurring <- decode.field(5, decode.bool)
  let start_time = calendar.TimeOfDay(
    hours: start_time_hour,
    minutes: start_time_minute,
    seconds: 0,
    nanoseconds: 0)
  case calendar.is_valid_time_of_day(start_time), recurring, day_of_week {
    True, True, option_dow -> decode.success(ScheduledActionOnRecurring(target_name, start_time, option_dow, seconds(duration_seconds)))
    True, False, Some(dow) -> decode.success(ScheduledActionOnOnce(target_name, start_time, dow, seconds(duration_seconds)))
    True, False, None -> decode.failure(zero(), "DayOfWeek")
    False, _, _ -> decode.failure(zero(), "TimeOfDay")
  }
}

pub fn to_json(scheduled_action: ScheduledAction) -> json.Json {
  case scheduled_action {
    ScheduledActionOnOnce(target_name, start_time, day_of_week, duration) -> json.object([
      #("target_name", json.string(target_name)),
      #("start_time", json.string(start_time_to_string(start_time))),
      #("day_of_week", json.string(day_of_week.to_string(day_of_week))),
      #("duration_seconds", json.float(duration.to_seconds(duration))),
      #("recurring", json.bool(False)),
    ])
    ScheduledActionOnRecurring(target_name, start_time, day_of_week, duration) -> json.object([
      #("target_name", json.string(target_name)),
      #("start_time", json.string(start_time_to_string(start_time))),
      #("day_of_week", json.nullable(option.map(day_of_week, day_of_week.to_string), json.string)),
      #("duration_seconds", json.float(duration.to_seconds(duration))),
      #("recurring", json.bool(True)),
    ])
  }
}

fn start_time_to_string(start_time: calendar.TimeOfDay) -> String {
  let hours = string.pad_start(int.to_string(start_time.hours), to: 2, with: "0")
  let minutes = string.pad_start(int.to_string(start_time.minutes), to: 2, with: "0")
  hours <> ":" <> minutes
}
