import formal/form
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, Some, None}
import gleam/pair
import gleam/string
import gleam/time/calendar
import gleam/time/duration
import gleam/time/timestamp.{type Timestamp}
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/effect
import lustre_websocket as ws
import shared/scheduled_action
import shared/day_of_week.{Monday}
import shared/on_or_off.{type OnOrOff, On, Off}

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type SolenoidState {
  SolenoidState(
    name: String,
    on_or_off: Option(OnOrOff),
    minutes_next: String,
    scheduled_off_at: Option(Timestamp),
  )
}

type Model {
  Model(
    ws: Option(ws.WebSocket),
    solenoids: Dict(Int, SolenoidState),
    messages_rev: List(String),
    scheduled_action_form: form.Form(scheduled_action.ScheduledAction)
  )
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(Model(ws: None,
          solenoids: dict.from_list(list.map([
              #(1, "pumpkins"),
              #(2, "zucs/poly tunnel"),
              #(3, "brassica seedlings"),
              #(4, "flowers"),
              #(5, "sauce toms"),
            ], fn(solenoid: #(Int, String)) -> #(Int, SolenoidState) {
              #(solenoid.0, SolenoidState(name: solenoid.1,
                              on_or_off: None,
                              minutes_next: "20",
                              scheduled_off_at: None))})),
          messages_rev: [],
          scheduled_action_form: scheduled_action_form()),
    ws.init("/ws", WsWrapper))
}

type Msg {
  UserClickedRunIrrigation(solenoid_id: Int)
  UserClickedTurnOffIrrigation(solenoid_id: Int)
  UserUpdatedOnTime(solenoid_id: Int, on_time: String)
  WsWrapper(ws.WebSocketEvent)
  UserSubmittedScheduledActionForm(form_result: Result(scheduled_action.ScheduledAction, form.Form(scheduled_action.ScheduledAction)))
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg, model.ws {
    UserClickedRunIrrigation(solenoid_id), Some(socket) -> {
      case dict.get(model.solenoids, solenoid_id) {
        Ok(solenoid) -> {
          case int.parse(solenoid.minutes_next) {
            Ok(minutes_on) -> {
              let seconds_on = minutes_on * 60
              let payload = "n1s" <> int.to_string(solenoid_id) <> "on " <> int.to_string(seconds_on)
              let scheduled_off_at = timestamp.add(timestamp.system_time(), duration.seconds(seconds_on))
              let updated_solenoids = dict.insert(model.solenoids, solenoid_id, SolenoidState(..solenoid, scheduled_off_at: Some(scheduled_off_at)))
              #(Model(..model, solenoids: updated_solenoids), ws.send(socket, payload))
            }
            Error(_) -> {
              alert("Invalid on duration")
              #(model, effect.none())
            }
          }
        }
        Error(_) -> {
          alert("Failed to run irrigation!")
          #(model, effect.none())
        }
      }
    }
    UserClickedRunIrrigation(_solenoid_number), None -> {
      let _ = alert("Disconnected from server, cannot start irrigation")
      #(model, effect.none())
    }
    UserClickedTurnOffIrrigation(solenoid_id), Some(socket) -> {
      case dict.get(model.solenoids, solenoid_id) {
        Ok(solenoid) -> {
          let payload = "n1s" <> int.to_string(solenoid_id) <> "off"
          let updated_solenoids = dict.insert(model.solenoids, solenoid_id, SolenoidState(..solenoid, scheduled_off_at: None))
          #(Model(..model, solenoids: updated_solenoids), ws.send(socket, payload))
        }
        Error(_) -> {
          alert("Failed to turn off irrigation!")
          #(model, effect.none())
        }
      }
    }
    UserClickedTurnOffIrrigation(_solenoid_number), None -> {
      let _ = alert("Disconnected from server, cannot turn off irrigation")
      #(model, effect.none())
    }
    UserUpdatedOnTime(solenoid_id, on_time), _ -> {
      case dict.get(model.solenoids, solenoid_id) {
        Ok(solenoid) -> {
          let updated_solenoids = dict.insert(model.solenoids, solenoid_id,
                                              SolenoidState(..solenoid, minutes_next: on_time))
          #(Model(..model, solenoids: updated_solenoids), effect.none())
        }
        Error(_) -> {
          #(model, effect.none())
        }
      }
    }
    UserSubmittedScheduledActionForm(form_result: Ok(the_scheduled_action)), Some(socket) -> {
      io.println("Woooo: " <> string.inspect(the_scheduled_action))
      #(model, ws.send(socket, json.to_string(scheduled_action.to_json(the_scheduled_action))))
    }
    UserSubmittedScheduledActionForm(form_result: Ok(the_scheduled_action)), None -> {
      io.println("No WS but form success Woooo: " <> string.inspect(the_scheduled_action))
      alert("Can't submit action when disconnected from server")
      #(model, effect.none())
    }
    UserSubmittedScheduledActionForm(form_result: Error(form)), _ -> {
      io.println("Form Fail: " <> string.inspect(form))
      #(Model(..model, scheduled_action_form: form), effect.none())
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
      case parse_message(msg) {
        Ok(#(_node_number, solenoid_number, on_or_off)) -> {
          case dict.get(model.solenoids, solenoid_number) {
            Ok(solenoid) -> {
              let new_solenoid_state = case on_or_off {
                On -> SolenoidState(..solenoid, on_or_off: Some(On))
                Off -> SolenoidState(..solenoid, on_or_off: Some(Off), scheduled_off_at: None)
              }
              let updated_solenoids = dict.insert(model.solenoids, solenoid_number, new_solenoid_state)
              #(Model(..model, solenoids: updated_solenoids), effect.none())
            }
            Error(_) -> #(model, effect.none())
          }
        }
        Error(_) -> {
          #(Model(..model, messages_rev: [msg, ..model.messages_rev]),
            effect.none())
        }
      }
    }
    WsWrapper(ws.OnBinaryMessage(msg)), _ -> {
      io.println("Received binary ws message: " <> string.inspect(msg))
      #(model, effect.none())
    }
    WsWrapper(ws.OnClose(reason)), _ -> {
      io.println("Websocket closed: " <> string.inspect(reason))
      #(Model(..model, ws: None), ws.init("/ws", WsWrapper))
    }
  }
}

fn view(model: Model) -> Element(Msg) {
  let solenoids = list.sort(dict.to_list(model.solenoids), fn(a, b) {int.compare(a.0, b.0)})
  html.div([], [
    html.div([], [
      case model.ws {
        None -> html.div([
          attribute.styles([#("color", "red"),
                            #("font-size", "13px")]),
          ], [
            html.text("Disconnected from server")
          ])
        Some(_) -> html.div([
          attribute.styles([#("color", "#0af"),
                            #("font-size", "13px")]),
          ], [
            html.text("Connected to server")
          ])
      }
    ]),
    html.div([],
      list.map(solenoids, fn(solenoid_kv) {
        let #(solenoid_id, solenoid) = solenoid_kv
        html.div([attribute.styles([
          #("margin-top", "10px"),
          #("margin-bottom", "10px"),
          #("border", "1px solid #444")
        ])], [
          html.div([
            attribute.styles([
              #("margin", "5px"),
              #("color", "#444"),
              #("font-size", "20px"),
            ])
          ], [
            html.text(solenoid.name),
            case solenoid.on_or_off {
              None -> html.div([], [])
              Some(On) -> html.div([
                attribute.styles([
                  #("color", "#0af")
                ])], [
                  html.text("watering")
                ])
              Some(Off) -> html.div([
                attribute.styles([
                  #("color", "#666"),
                ])], [
                  html.text("off")
                ])
            }
          ]),
          html.div([attribute.styles([
            #("display", "flex"),
            #("flex-direction", "row")
          ])], [
            html.div([attribute.styles([
              #("margin", "10px"),
            ])], [
              html.button([event.on_click(UserClickedRunIrrigation(solenoid_id))], [html.text("ON")]),
            ]),
            html.div([attribute.styles([#("margin-top", "10px")])], [
              html.text("for "),
              html.input([
                attribute.type_("number"),
                attribute.value(solenoid.minutes_next),
                attribute.styles([#("width", "5ch")]),
                event.on_input(UserUpdatedOnTime(solenoid_id, _)),
              ]),
              html.text(" minutes"),
            ]),
            html.div([attribute.styles([
              #("margin", "10px")
            ])], [
              html.button([event.on_click(UserClickedTurnOffIrrigation(solenoid_id))], [html.text("OFF")])
            ]),
          ])
        ])
      })
    ),
    html.div([], [
      html.textarea([attribute.readonly(True),
                     attribute.styles([
                       #("width", "95vw"),
                       #("height", "30vh")])],
                    string.join(model.messages_rev, "\n"))
    ])
    // scheduled_action_form_view(model)
  ])
}

fn scheduled_action_form() -> form.Form(scheduled_action.ScheduledAction) {
  // TODO: add zone enum to Action, add dropdown for zone to form
  form.new({
    use hour <- form.field("hour", {
      form.parse_int
    })
    use minute <- form.field("minute", {
      form.parse_int
    })
    use someday <- form.field("day", {
      form.parse(fn(input) {
        case input {
          [] -> Error(#(Monday, "must provide day"))
          ["", ..] -> Error(#(Monday, "must provide day"))
          [val, ..] -> case day_of_week.parse(val) {
            Ok(day) -> Ok(day)
            Error(error) -> Error(#(Monday, error))
          }
        }
      })
    })
    use duration_seconds <- form.field("duration_seconds", {
      form.parse_int
    })
    form.success(scheduled_action.ScheduledActionOnOnce("node1hi", calendar.TimeOfDay(hour, minute, 0, 0), someday, duration.seconds(duration_seconds)))
  })
}

fn scheduled_action_form_view(model: Model) {
  let handle_form_submission = fn(fields) {
    scheduled_action_form()
    |> form.add_values(fields)
    |> form.run
    |> UserSubmittedScheduledActionForm
  }

  html.div([attribute.action("/create-schedule")], [
    html.legend([], [html.text("Add scheduled action")]),
    html.form([attribute.id("add-action"),
               event.on_submit(handle_form_submission)], [
      field_input(form: model.scheduled_action_form,
                  name: "hour", kind: "number", label: "Hour"),
      field_input(form: model.scheduled_action_form,
                  name: "minute", kind: "number", label: "Minute"),
      field_input(form: model.scheduled_action_form,
                  name: "text", kind: "number", label: "Day"),
      field_input(form: model.scheduled_action_form,
                  name: "duration_seconds", kind: "number", label: "Duration in Seconds"),
      html.input([attribute.type_("submit"), attribute.value("Submit")])
    ])
  ])
}

fn field_input(
  form form: form.Form(t),
  name name: String,
  kind kind: String,
  label label_text: String,
) -> Element(a) {
  let errors = form.field_error_messages(form, name)

  html.label([], [
    // The label text, for the user to read
    element.text(label_text),
    // The input, for the user to type into
    html.input([
      attribute.type_(kind),
      attribute.name(name),
      attribute.value(form.field_value(form, name)),
      case errors {
        [] -> attribute.none()
        _ -> attribute.aria_invalid("true")
      },
    ]),
    // Any errors presented below
    ..list.map(errors, fn(msg) { html.small([], [element.text(msg)]) })
  ])
}

/// Parse "n1s3on" into #(1, 3, On)
fn parse_message(msg: String) -> Result(#(Int, Int, OnOrOff), Nil) {
  let n = string.slice(msg, 0, 1)
  let node_str = string.slice(msg, 1, 1)
  let s = string.slice(msg, 2, 1)
  let solenoid_str = string.slice(msg, 3, 1)
  let state = string.drop_start(msg, 4)
  case n, int.parse(node_str), s, int.parse(solenoid_str), state {
    "n", Ok(node_number), "s", Ok(solenoid_number), "on" -> Ok(#(node_number, solenoid_number, On))
    "n", Ok(node_number), "s", Ok(solenoid_number), "off" -> Ok(#(node_number, solenoid_number, Off))
    _, _, _, _, _ -> Error(Nil)
  }
}

@external(javascript, "./frontend.js", "raise_alert")
fn alert(msg: String) -> Nil
