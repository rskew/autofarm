import argv
import gleam/bit_array
import gleam/bytes_tree
import gleam/erlang/application
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/pair
import gleam/result.{try}
import gleam/string
import logging
import mist.{type Connection, type ResponseData}
import serial
import shared/scheduled_action.{type ScheduledAction}
import sqlight

// todo
//const initial_action_states = dict.from_list([
//  #("node1hi", NodeState(desired_state: ActionStateOff("node1hi"), known_state: ActionStateOff("node1hi"), last_heard_at: None),
//  #("node2hello", ActionStateOff("node2hello"))
//])

pub fn main() {
  logging.configure()
  logging.set_level(logging.Debug)

  let result = {
    use priv_directory <- try(
      application.priv_directory("backend")
      |> result.map_error(fn(_) {"Couldn't get backend priv directory"}))
    let serve_directory = string.append(priv_directory, "/public")

    use db_conn <- sqlight.with_connection(priv_directory <> "/db.sqlite")

    use _ <- try(initialise_database(db_conn))

    let broadcaster_name = process.new_name("broadcaster")
    use broadcaster_actor <- try(
      actor.new(#([], []))
      |> actor.on_message(broadcaster_handle_message)
      |> actor.named(broadcaster_name)
      |> actor.start
      |> result.map_error(string.inspect))
    let _ = process.monitor(broadcaster_actor.pid)
    let broadcast = process.send(process.named_subject(broadcaster_name), _)

    use device <- try(case argv.load().arguments {
      [device] -> Ok(device)
      _ -> Error("Must provide a single device argument")
    })
    use serial_manager <- try(serial.start_link(device, fn(msg) {broadcast(Broadcast(msg))}))

    use _ <- result.map(
      fn(req: Request(Connection)) -> Response(ResponseData) {
        logging.log(
          logging.Info,
          "Got a request from: " <> string.inspect(mist.get_client_info(req.body)),
        )
        case request.path_segments(req) {
          [] -> serve_file(req, [serve_directory, "index.html"])
          ["ws"] ->
            mist.websocket(
              request: req,
              on_init: fn(_conn) {
                let subject = process.new_subject()
                broadcast(Add(subject))
                let selector = process.select(process.new_selector(), subject)
                //process.send(subject, LoadActuatorStates)
                process.send(subject, LoadScheduledActions)
                #(#(db_conn, subject), Some(selector))
              },
              on_close: fn(state) {
                let subject = pair.second(state)
                broadcast(Remove(subject))
                io.println("goodbye!")
              },
              handler: handle_ws_message(fn(msg) {serial_manager.call_message(msg)})
            )
          path -> serve_file(req, [serve_directory, ..path])
        }
      }
      |> mist.new
      |> mist.bind("0.0.0.0")
      //|> mist.bind("localhost")
      |> mist.with_ipv6
      |> mist.port(8006)
      |> mist.start
      |> result.map_error(string.inspect)
    )
    io.println("Listening for messages from serial")
    let selector =
      process.new_selector()
      |> process.select_monitors(fn(down) {Error(string.inspect(down.reason))})
      |> process.select_other(mapping: fn(msg) {Ok(FromOther(msg))})
    receive_forever_loop(selector, fn(msg) {broadcast(Broadcast(msg))})
  }
  case result {
    Error(error) -> io.println("Error: " <> string.inspect(error))
    Ok(_) -> Nil
  }
}

type Broadcaster {
  Broadcast(message: String)
  Add(subscriber: process.Subject(MyMessage))
  Remove(subscriber: process.Subject(MyMessage))
}

fn broadcaster_handle_message(state, msg) {
  let #(messages, subscribers) = state
  case msg {
    Broadcast(m) -> {
      list.each(subscribers, fn(sub) {
        process.send(sub, ForwardFromBackend(m))
      })
      let messages = case list.length(messages) {
        a if a > 100 -> case list.rest(messages) {
          Ok(tail) -> tail
          Error(_) -> []  // Can't happen due to checking the length
        }
        _ -> messages
      }
      actor.continue(#(list.append(messages, [m]), subscribers))
    }
    Add(sub) -> {
      list.each(messages, fn(msg) {
        process.send(sub, ForwardFromBackend(msg))
      })
      actor.continue(#(messages, [sub, ..subscribers]))
    }
    Remove(sub) -> actor.continue(#(messages, list.filter(subscribers, fn(s) {s != sub})))
  }
}

pub type MyMessage {
  ForwardFromBackend(String)
  LoadActuatorStates
  LoadScheduledActions
}

fn handle_ws_message(call_serial) {
  fn(state, message, conn) {
    case message {
      mist.Text("ping") -> {
        let assert Ok(_) = mist.send_text_frame(conn, "pong")
        mist.continue(state)
      }
      mist.Text(msg) -> {
        logging.log(logging.Info, "Received text frame: " <> msg)
        // TODO: Handle error from serial
        let assert Ok(_) = case call_serial(msg) {
          Ok(_) -> mist.send_text_frame(conn, "Message '" <> msg <> "' forwarded to gateway")
          Error(error) -> mist.send_text_frame(conn, "Error forwarding message to gateway: " <> error)
        }
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
      mist.Custom(ForwardFromBackend(text)) -> {
        let assert Ok(_) = mist.send_text_frame(conn, text)
        mist.continue(state)
      }
      mist.Custom(LoadActuatorStates) -> {
        todo
      }
      //  let #(db_conn, sub, previous_action_states) = state
      //  let assert Ok(_) = case db_fetch_actions_states(db_conn) {
      //    Ok(loaded_action_states) -> {
      //      let foldBuddy = fn(acc, loaded_action_state) {
      //        dict.update(acc, loaded_action_state.target_name, fn(_) {loaded_action_state})
      //      }
      //      let updated_action_states = list.fold(loaded_action_states, previous_action_states, foldBuddy)
      //      state = #(db_conn, sub, updated_action_states)
      //      mist.continue(state)
      //    }
      //    Error(error) -> mist.continue(state)
      //  }
      //}
      mist.Custom(LoadScheduledActions) -> {
        let db_conn = pair.first(state)
        let assert Ok(_) = case db_fetch_scheduled_actions(db_conn) {
          Ok(scheduled_actions) -> {
            let scheduled_actions_json = json.array(scheduled_actions, scheduled_action.to_json)
            mist.send_text_frame(conn, json.to_string(scheduled_actions_json))
          }
          Error(error) -> mist.send_text_frame(conn, error)
        }
        mist.continue(state)
      }
      mist.Closed | mist.Shutdown -> mist.stop()
    }
  }
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
    let content_type = guess_content_type(file_path)
    io.println("File found, content type " <> content_type)
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
    "lmth." <> _ -> "text/html"
    _ -> "application/octet-stream"
  }
}

type ReceivedMessage(other) {
  FromSerial(String)
  FromOther(other)
}

fn receive_forever_loop(selector, broadcast) -> Nil {
  case process.selector_receive_forever(selector) {
    Ok(FromSerial(message)) -> {
      io.println("Received message: " <> message)
      broadcast(message)
      receive_forever_loop(selector, broadcast)
    }
    Ok(FromOther(message)) -> {
      io.println("Received unhandled message: " <> string.inspect(message))
      receive_forever_loop(selector, broadcast)
    }
    Error(reason) -> io.println("Stopping: " <> reason)
  }
}

fn initialise_database(conn: sqlight.Connection) -> Result(Nil, String) {
    let sql = "
    create table if not exists scheduled_actions (
      target_name text not null,
      start_time_hour int not null,
      start_time_minute int not null,
      day_of_week text,
      duration_seconds int not null,
      recurring bool not null
    );
    "
    sqlight.exec(sql, conn)
    |> result.map_error(fn(sqlight_error) {sqlight_error.message})
}

fn db_fetch_scheduled_actions(conn: sqlight.Connection) -> Result(List(ScheduledAction), String) {
  let sql = "select * from scheduled_actions"
  sqlight.query(sql, on: conn, expecting: scheduled_action.decoder(), with: [])
  |> result.map_error(fn(sqlight_error) {sqlight_error.message})
}

//fn db_fetch_action_states(conn: sqlight.Connection) -> Result(List(ActionState), String) {
//  let sql = "select * from action_states"
//  sqlight.query(sql, on: conn, expecting: action_state.decoder(), with: [])
//  |> result.map_error(fn(sqlight_error) {sqlight_error.message})
//}
