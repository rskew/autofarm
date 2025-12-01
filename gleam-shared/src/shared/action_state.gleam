import gleam/dynamic/decode
import gleam/json
import gleam/time/timestamp
import shared/on_or_off.{On, Off}

pub type ActionState {
  ActionStateOn(target_name: String, off_time: timestamp.Timestamp)
  ActionStateOff(target_name: String)
}

pub fn zero() -> ActionState {
  ActionStateOff("")
}

pub fn decoder() {
  use target_name <- decode.field(0, decode.string)
  use on_or_off <- decode.field(1, on_or_off.decoder())
  case on_or_off {
    On -> {
      use off_time_int <- decode.field(2, decode.int)
      let off_time = timestamp.from_unix_seconds(off_time_int)
      decode.success(ActionStateOn(target_name, off_time))
    }
    Off -> decode.success(ActionStateOff(target_name))
  }
}

pub fn to_json(action_state: ActionState) -> json.Json {
  case action_state {
    ActionStateOn(target_name, off_time) -> json.object([
      #("target_name", json.string(target_name)),
      #("on_or_off", json.string("on")),
      #("off_time", json.float(timestamp.to_unix_seconds(off_time))),
    ])
    ActionStateOff(target_name) -> json.object([
      #("target_name", json.string(target_name)),
      #("on_or_off", json.string("off")),
    ])
  }
}
