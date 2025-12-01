import gleam/dynamic/decode
import gleam/string

pub type OnOrOff {
  On
  Off
}

pub fn decoder() -> decode.Decoder(OnOrOff) {
  decode.new_primitive_decoder("OnOrOff", fn(data) {
    case decode.run(data, decode.string) {
      Ok(str) -> case string.lowercase(str) {
        "on" -> Ok(On)
        "off" -> Ok(Off)
        _ -> Error(Off)
      }
      _ -> Error(Off)
    }
  })
}

pub fn to_string(val: OnOrOff) -> String {
  case val {
    On -> "On"
    Off -> "Off"
  }
}
