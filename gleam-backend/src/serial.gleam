import gleam/otp/actor
import gleam/dynamic
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/erlang/atom
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import shared

pub type Message {
  Connect(reply_to: Option(process.Subject(Result(Nil, String))))
  Disconnect
  SerialReceive(dynamic.Dynamic)
  SerialSend(reply_to: Option(process.Subject(Result(Nil, String))), message: String)
  SerialMonitorError(String)
}

pub type Serial {
  Serial(call_connect: fn() -> Result(Nil, String),
         call_message: fn(String) -> Result(Nil, String))
}

pub fn start_link(device: String, send_to_client: fn(String) -> Nil) -> Result(Serial, String) {
  use serial_actor <- result.map(
    actor.new_with_initialiser(1000, initialise)
    |> actor.on_message(serial_handle_message(device, send_to_client))
    |> actor.start
    |> result.map_error(string.inspect))
  let _ = process.monitor(serial_actor.pid)
  process.send(serial_actor.data, Connect(None))
  Serial(call_connect: fn() {process.call(serial_actor.data, 100, fn(reply_to) {Connect(Some(reply_to))})},
         call_message: fn(msg) {process.call(serial_actor.data, 100, fn(reply_to) {SerialSend(Some(reply_to), msg)})})
}

type ConnectedState {
  Connected
  Disconnected
}

const initial_retry_delay_millis = 1000
const backoff_multiplier = 2
const backoff_maximum_millis = 10_000

type State {
  State(connection_state: ConnectedState,
        serial_pid: process.Pid,
        self: process.Subject(Message),
        retry_delay_millis: Int)
}

fn initialise(subject) {
  use serial_pid <- result.map(circuits_uart_start_link())
  let selector =
    process.new_selector()
    |> process.select(subject)
    |> process.select_record(
         tag: atom.create("circuits_uart"),
         fields: 2,
         mapping: SerialReceive,
       )
    |> process.select_monitors(fn(down) {SerialMonitorError(string.inspect(down.reason))})
  actor.initialised(State(connection_state: Disconnected,
                          serial_pid: serial_pid,
                          self: subject,
                          retry_delay_millis: initial_retry_delay_millis))
  |> actor.selecting(selector)
  |> actor.returning(subject)
}

fn optional_send(optional_subject, reply) {
  case optional_subject {
    Some(subject) -> process.send(subject, reply)
    None -> Nil
  }
}

fn serial_handle_message(device: String, send_to_client: fn(String) -> Nil) {
  fn(state: State, message: Message) -> actor.Next(State, Message) {
    case message, state {
      Connect(reply_to), _ -> {
        case open(state.serial_pid, device) {
          Ok(Nil) -> {
            io.println("Connected to serial device " <> device)
            optional_send(reply_to, Ok(Nil))
            actor.continue(State(..state, connection_state: Connected, retry_delay_millis: initial_retry_delay_millis))
          }
          // TODO: Ignore error due to already being connected
          // if one happens
          Error(error) -> {
            io.println("Error connecting to serial device '" <> device <> "': " <> error)
            io.println("Trying again in " <> int.to_string(state.retry_delay_millis) <> " milliseconds")
            process.send_after(state.self, state.retry_delay_millis, Connect(None))
            optional_send(reply_to, Error(error))
            let retry_delay_millis = int.min(state.retry_delay_millis * backoff_multiplier, backoff_maximum_millis)
            actor.continue(State(..state, connection_state: Disconnected, retry_delay_millis: retry_delay_millis))
          }
        }
      }
      Disconnect, _ -> {
        io.println("Disnnecting from serial device " <> device)
        case serial_close(state.serial_pid) {
          Ok(Nil) -> Nil
          Error(error) -> io.println("Error closing device '" <> device <> "': " <> error)
        }
        actor.continue(State(..state, connection_state: Disconnected, retry_delay_millis: initial_retry_delay_millis))
      }
      SerialReceive(serial_message), _ -> {
        case state.connection_state {
          Disconnected -> {
            io.println("WTF: Received message from disconnected device '" <> device <> "': " <> string.inspect(serial_message))
          }
          Connected -> Nil
        }
        case parse_serial_message(serial_message) {
          Ok(parsed_message) -> {
            send_to_client(parsed_message)
            actor.continue(State(..state, connection_state: Connected, retry_delay_millis: initial_retry_delay_millis))
          }
          Error(error) -> {
            io.println("Error from serial: " <> error)
            process.send_after(state.self, state.retry_delay_millis, Connect(None))
            actor.continue(State(..state, connection_state: Disconnected))
          }
        }
      }
      SerialSend(reply_to, client_message), State(connection_state: Connected, ..) -> {
        case write(state.serial_pid, client_message) {
          Ok(Nil) -> {
            optional_send(reply_to, Ok(Nil))
            actor.continue(state)
          }
          Error(error) -> {
            io.println("Error from serial: " <> error)
            optional_send(reply_to, Error(error))
            process.send_after(state.self, state.retry_delay_millis, Connect(None))
            actor.continue(State(..state, connection_state: Disconnected))
          }
        }
      }
      SerialSend(reply_to, _client_message), State(connection_state: Disconnected, ..) -> {
        optional_send(reply_to, Error("Not connected"))
        actor.continue(state)
      }
      SerialMonitorError(error), _ -> {
        io.println("Serial crashed: " <> error)
        actor.stop_abnormal(error)
      }
    }
  }
}

fn parse_serial_message(payload: dynamic.Dynamic) -> Result(String, String) {
  let decoded = {
    use tag_atom <- result.try(decode.run(payload, decode.at([0], atom.decoder())))
    use device <- result.try(decode.run(payload, decode.at([1], decode.string)))
    let message_decoder = decode.one_of(
      decode.string |> decode.map(Ok),
      [decode.at([1], atom.decoder() |> decode.map(atom.to_string) |> decode.map(Error))]
    )
    use message <- result.map(decode.run(payload, decode.at([2], message_decoder)))
    #(atom.to_string(tag_atom), device, message)
  }
  case decoded {
    Error(decode_errors) -> {
      erlang_format("Error decoding message: ~w~n", [payload])
      Error(shared.render_decode_errors(decode_errors))
    }
    Ok(#(tag, device, message)) -> {
      case message {
        Ok(m) -> {
          let ok_message = string.concat([tag, ": ", device, " \"", m, "\""])
          io.println("Received: " <> ok_message)
          Ok(ok_message)
        }
        Error(err) -> {
          let err_message = "Error from " <> tag <> " " <> device <> ": " <> err
          io.println("Received: " <> err_message)
          Error(err_message)
        }
      }
    }
  }
}

@external(erlang, "Elixir.Circuits.UART", "start_link")
pub fn circuits_uart_start_link() -> Result(process.Pid, String)

@external(erlang, "Elixir.Circuits.UART", "open")
fn ext_serial_open(uart: process.Pid, device: String, opts: List(#(atom.Atom, dynamic.Dynamic))) -> dynamic.Dynamic

pub fn open(serial_pid: process.Pid, device: String) -> Result(Nil, String) {
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
  ext_serial_open(serial_pid, device, opts)
  |> decode_ok_or_errortuple()
}

@external(erlang, "Elixir.Circuits.UART", "close")
fn ext_serial_close(serial_pid: process.Pid) -> dynamic.Dynamic

fn serial_close(serial_pid: process.Pid) -> Result(Nil, String) {
  ext_serial_close(serial_pid)
  |> decode_ok_or_errortuple()
}

@external(erlang, "io", "format")
fn erlang_format(formatstr: String, args: List(dynamic.Dynamic)) -> Nil

@external(erlang, "Elixir.Circuits.UART", "write")
fn ext_write(pid: process.Pid, message: String) -> dynamic.Dynamic

fn write(pid: process.Pid, message: String) -> Result(Nil, String) {
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
  |> result.map_error(shared.render_decode_errors)
  |> result.flatten()
}
