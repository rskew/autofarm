import gleam/dynamic/decode
import gleam/list
import gleam/string

pub fn render_decode_errors(errors: List(decode.DecodeError)) -> String {
  errors
  |> list.map(fn(de) {
      string.concat(["Decode Error: Expected: ", de.expected, ", found: ", de.found])})
  |> string.concat()
}

