// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, Some, None}
import gleam/string
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/effect
import lustre_websocket as ws

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

/// The `Model` is the state of our entire application.
///
type Model {
  Model(
    state: IrrigationState,
    ws: Option(ws.WebSocket),
    minutes_on: Int,
    messages_rev: List(String),
  )
}

/// The `init` function gets called when we first start our app. It sets the
/// initial state of the app.
///
fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(Model(state: NotRunning, ws: None, minutes_on: 5, messages_rev: []),
    ws.init("/ws", WsWrapper))
}

// UPDATE ----------------------------------------------------------------------

/// The `Msg` type describes all the ways the outside world can talk to our app.
/// That includes user input, network requests, and any other external events.
///
type Msg {
  UserClickedRunIrrigation
  UserUpdatedOnTime(String)
  WsWrapper(ws.WebSocketEvent)
}

type IrrigationState {
  NotRunning
  UserClickedRun
  Running
}

/// The `update` function is called every time we receive a message from the
/// outside world. We get the message and the current state of the app, and we
/// use those to calculate the new state.
///
fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg, model.ws {
    UserClickedRunIrrigation, Some(socket) -> {
      let payload = "Run Irrigation for " <> int.to_string(model.minutes_on) <> " minutes"
      #(Model(..model, state: UserClickedRun), ws.send(socket, payload))
    }
    UserClickedRunIrrigation, None -> {
      let _ = alert("Disconnected from server, cannot start irrigation")
      #(model, effect.none())
    }
    UserUpdatedOnTime(on_time), _ -> {
      case int.parse(on_time) {
        Ok(minutes_on) -> {
          io.println("New on time: " <> int.to_string(minutes_on))
          #(Model(..model, minutes_on: minutes_on), effect.none())
        }
        Error(_) -> #(model, effect.none())
      }
    }
    WsWrapper(ws.InvalidUrl), _ -> panic
    WsWrapper(ws.OnOpen(socket)), Some(_) -> {
      io.println("Websocket opened")
      #(Model(..model, ws: Some(socket)), effect.none())
    }
    WsWrapper(ws.OnOpen(socket)), None -> {
      io.println("Websocket opened")
      #(Model(..model, ws: Some(socket)), ws.send(socket, "connected"))
    }
    WsWrapper(ws.OnTextMessage(msg)), _ -> {
      io.println("Received ws message: " <> msg)
      #(Model(..model, messages_rev: [msg, ..model.messages_rev]),
        effect.none())
    }
    WsWrapper(ws.OnBinaryMessage(msg)), _ -> {
      io.println("Received ws message: " <> string.inspect(msg))
      #(model, effect.none())
    }
    WsWrapper(ws.OnClose(reason)), _ -> {
      io.println("Websocket closed: " <> string.inspect(reason))
      #(Model(..model, ws: None), ws.init("/ws", WsWrapper))
    }
  }
}

// VIEW ------------------------------------------------------------------------

/// The `view` function is called after every `update`. It takes the current
/// state of our application and renders it as an `Element`
///
fn view(model: Model) -> Element(Msg) {
  html.div([], [
    html.div([], [
      html.button([event.on_click(UserClickedRunIrrigation)], [html.text("Run irrigation")]),
    ]),
    html.div([], [
     html.input([attribute.type_("number"),
                 attribute.value(int.to_string(model.minutes_on)),
                 event.on_input(UserUpdatedOnTime)]),
    ]),
    html.div([], [
     html.textarea([attribute.readonly(True),
                    attribute.styles([
                      #("width", "800"),
                      #("height", "400")])],
                   string.join(model.messages_rev, "\n"))
    ]),
  ])
}

@external(javascript, "./frontend.js", "raise_alert")
fn alert(msg: String) -> Nil
