import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/erlang/atom
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import types.{type GoStop, Go, Stop}

pub fn add_selector(selector: process.Selector(#(GoStop, String))) -> process.Selector(#(GoStop, String)) {
  selector
  |> process.select_record(tag: atom.create("circuits_uart"),
                           fields: 2,
                           mapping: handle_uart_message
                          )
}

fn handle_uart_message(uart_message: dynamic.Dynamic) -> #(GoStop, String) {
  case parse_uart_message(uart_message) {
    Error(decode_errors) -> {
      erlang_format("Message received: ~w~n", [uart_message])
      #(Stop, render_decode_errors(decode_errors))
    }
    Ok(#(tag, device, message)) -> {
      case message {
        Ok(m) -> {
          #(Go, string.concat([tag, ": ", device, " \"", m, "\""]))
        }
        Error(err) -> {
          #(Stop, "Error from " <> tag <> " " <> device <> ": " <> err)
        }
      }
    }
  }
}

fn parse_uart_message(payload: dynamic.Dynamic) -> Result(#(String, String, Result(String, String)), List(decode.DecodeError)) {
  use tag <- result.try(decode.run(payload, decode.at([0], atom.decoder())))
  use device <- result.try(decode.run(payload, decode.at([1], decode.string)))
  let message_decoder = decode.one_of(
    decode.string |> decode.map(Ok),
    [decode.at([1], atom.decoder() |> decode.map(atom.to_string) |> decode.map(Error))]
  )
  use message <- result.map(decode.run(payload, decode.at([2], message_decoder)))
  #(atom.to_string(tag), device, message)
}

@external(erlang, "Elixir.Circuits.UART", "start_link")
pub fn start_link() -> Result(process.Pid, String)

@external(erlang, "Elixir.Circuits.UART", "open")
fn ext_serial_open(uart: process.Pid, device: String, opts: List(#(atom.Atom, dynamic.Dynamic))) -> dynamic.Dynamic

pub fn open(uart: process.Pid, device: String) -> Result(Nil, String) {
  let opts = [
    #(atom.create("active"), atom.to_dynamic(atom.create("true"))),
    #(atom.create("speed"), dynamic.int(115200)),
    #(atom.create("framing"), dynamic.array([
      atom.to_dynamic(atom.create("Elixir.Circuits.UART.Framing.Line")),
      dynamic.list([dynamic.array([atom.to_dynamic(atom.create("separator")),
                                   dynamic.string("\r\n")]),
      ])
    ])),
  ]
  //erlang_format("opening serial with opts ~w~n",
  //  [dynamic.list(list.map(opts, fn(asdf) {dynamic.array([atom.to_dynamic(asdf.0), asdf.1])}))])
  ext_serial_open(uart, device, opts)
  |> decode_ok_or_errortuple()
}

fn render_decode_errors(errors: List(decode.DecodeError)) -> String {
  errors
  |> list.map(fn(de) {
      string.concat(["Decode Error: Expected: ", de.expected, ", found: ", de.found])})
  |> string.concat()
}

@external(erlang, "io", "format")
fn erlang_format(formatstr: String, args: List(dynamic.Dynamic)) -> Nil

@external(erlang, "Elixir.Circuits.UART", "write")
fn ext_write(pid: process.Pid, message: String) -> dynamic.Dynamic

pub fn write(pid: process.Pid, message: String) -> Result(Nil, String) {
  io.println("Writing message to serial: " <> message)
  ext_write(pid, message)
  |> decode_ok_or_errortuple()
}

fn decode_ok_or_errortuple(ok_or_errortuple: dynamic.Dynamic) -> Result(Nil, String) {
  let decoder = decode.one_of(
    // ok
    atom.decoder()
    |> decode.map(fn(_){Ok(Nil)}),
    // {error, enoent}
    [decode.at([1], atom.decoder()
                    |> decode.map(atom.to_string)
                    |> decode.map(Error))])
  decode.run(ok_or_errortuple, decoder)
  |> result.map_error(render_decode_errors)
  |> result.flatten()
}
