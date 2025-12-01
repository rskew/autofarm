import gleam/dynamic/decode
import gleam/json
import gleam/time/duration.{type Duration, seconds}

pub type Action {
  ActionOn(target_name: String,
           duration: Duration)
}

pub fn zero() -> Action {
  ActionOn("", seconds(0))
}

pub fn decoder() {
  use target_name <- decode.field(0, decode.string)
  use duration_seconds <- decode.field(1, decode.int)
  decode.success(ActionOn(target_name, seconds(duration_seconds)))
}

pub fn to_json(action: Action) -> json.Json {
  json.object([
    #("target_name", json.string(action.target_name)),
    #("duration_seconds", json.float(duration.to_seconds(action.duration))),
  ])
}
