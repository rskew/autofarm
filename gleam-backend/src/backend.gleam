import argv
import gleam/bit_array
import gleam/bytes_tree
import gleam/dynamic
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import logging
import mist.{type Connection, type ResponseData}
import serial
import types.{type GoStop, Go, Stop}

const index = "<html lang='en'>
 <head>
    <title>Farm Control</title>
    <script type=\"module\">
      import { main } from '/static/dev-build/frontend/frontend.mjs';
      main();
    </script>
 </head>
 <body>
   <div id=\"app\"/>
 </body>
</html>"

type Broadcaster {
  Broadcast(message: String)
  Add(subscriber: process.Subject(MyMessage))
  Remove(subscriber: process.Subject(MyMessage))
}

fn handle_broadcaster_message(state, msg) {
  let #(messages, subscribers) = state
  case msg {
    Broadcast(m) -> {
      list.each(subscribers, fn(sub) {
        process.send(sub, Forward(m))
      })
      let messages = case list.length(messages) {
        a if a > 10 -> case list.rest(messages) {
          Ok(tail) -> tail
          Error(_) -> []  // Can't happen due to checking the length
        }
        _ -> messages
      }
      actor.continue(#(list.append(messages, [m]), subscribers))
    }
    Add(sub) -> {
      list.each(messages, fn(msg) {
        process.send(sub, Forward(msg))
      })
      actor.continue(#(messages, [sub, ..subscribers]))
    }
    Remove(sub) -> actor.continue(#(messages, list.filter(subscribers, fn(s) {s != sub})))
  }
}

pub fn main() {
  logging.configure()
  logging.set_level(logging.Debug)

  let result = {
    use device <- result.try(case argv.load().arguments {
      [device] -> Ok(device)
      _ -> Error("Must provide a single device argument")
    })
    io.println("Starting serial")
    use uart <- result.try(serial.start_link())
    io.println("Serial started. Opening device " <> device)
    use _ <- result.map(serial.open(uart, device))
    io.println("Device " <> device <> " opened")

    use actor <- result.try(
      actor.new(#([], [])) |> actor.on_message(handle_broadcaster_message) |> actor.start)
    let broadcaster = actor.data

    let not_found =
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_tree.new()))
  
    use _ <- result.map(
      fn(req: Request(Connection)) -> Response(ResponseData) {
        logging.log(
          logging.Info,
          "Got a request from: " <> string.inspect(mist.get_client_info(req.body)),
        )
        case request.path_segments(req) {
          [] ->
            response.new(200)
            |> response.prepend_header("my-value", "abc")
            |> response.prepend_header("my-value", "123")
            |> response.set_body(mist.Bytes(bytes_tree.from_string(index)))
          ["ws"] ->
            mist.websocket(
              request: req,
              on_init: fn(_conn) {
                let subject = process.new_subject()
                process.send(broadcaster, Add(subject))
                let selector = process.select(process.new_selector(), subject)
                #(subject, Some(selector))
              },
              on_close: fn(subject) {
                process.send(broadcaster, Remove(subject))
                io.println("goodbye!")
              },
              handler: handle_ws_message(fn(msg){serial.write(uart, msg)}),
            )
          ["static", ..rest] -> serve_file(req, ["static", ..rest])
          _ -> not_found
        }
      }
      |> mist.new
      |> mist.bind("localhost")
      |> mist.with_ipv6
      |> mist.port(8006)
      |> mist.start
    )
    io.println("Listening for messages from serial")
    receive_forever_loop(fn(msg) {process.send(broadcaster, Broadcast(msg))})
  }
  case result {
    Error(error) -> io.println("Error: " <> error)
    Ok(_) -> Nil
  }
}

pub type MyMessage {
  Forward(String)
}

fn handle_ws_message(forward_message) {
  let handler = fn(state, message, conn) {
    case message {
      mist.Text("ping") -> {
        let assert Ok(_) = mist.send_text_frame(conn, "pong")
        mist.continue(state)
      }
      mist.Text(msg) -> {
        logging.log(logging.Info, "Received text frame: " <> msg)
        let assert Ok(_) = mist.send_text_frame(conn, msg)
        forward_message(msg)
        mist.continue(state)
      }
      mist.Binary(msg) -> {
        logging.log(
          logging.Info,
          "Received binary frame ("
            <> int.to_string(bit_array.byte_size(msg))
            <> ")",
        )
        mist.continue(state)
      }
      mist.Custom(Forward(text)) -> {
        let assert Ok(_) = mist.send_text_frame(conn, text)
        mist.continue(state)
      }
      mist.Closed | mist.Shutdown -> mist.stop()
    }
  }
  handler
}

fn serve_file(
  _req: Request(Connection),
  path: List(String),
) -> Response(ResponseData) {
  let file_path = string.join(path, "/")
  io.println("Request for " <> file_path)

  // Omitting validation for brevity
  mist.send_file(file_path, offset: 0, limit: None)
  |> result.map(fn(file) {
    io.println("File found")
    let content_type = guess_content_type(file_path)
    response.new(200)
    |> response.prepend_header("content-type", content_type)
    |> response.set_body(file)
  })
  |> result.lazy_unwrap(fn() {
    io.println("File not found")
    response.new(404)
    |> response.set_body(mist.Bytes(bytes_tree.new()))
  })
}

fn guess_content_type(path: String) -> String {
  case string.reverse(path) {
    "sjm." <> _ | "sj." <> _ -> "text/javascript"
    _ -> "application/octet-stream"
  }
}

pub fn receive_forever_loop(broadcast) -> Nil {
  let selector =
    process.new_selector()
    |> serial.add_selector()
    |> process.select_other(mapping: handle_other_message)
  do_receive(selector, broadcast)
}

fn do_receive(selector, broadcast) {
  case process.selector_receive_forever(selector) {
    #(Go, msg) -> {
      io.println("Received message: " <> msg)
      broadcast(msg)
      do_receive(selector, broadcast)
    }
    #(Stop, msg) -> io.println("Stopping: " <> msg)
  }
}

fn handle_other_message(message: dynamic.Dynamic) -> #(GoStop, String) {
  erlang_format("Received unhandled message: ~w~n", [message])
  #(Go, "Received unhandled message")
}

@external(erlang, "io", "format")
fn erlang_format(formatstr: String, args: List(dynamic.Dynamic)) -> Nil
