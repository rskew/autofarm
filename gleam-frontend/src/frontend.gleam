import formal/form
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, Some, None}
import gleam/string
import gleam/time/calendar
import gleam/time/duration
import lustre
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import lustre/effect
import lustre_websocket as ws
import shared/action
import shared/scheduled_action
import shared/day_of_week.{Monday}

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type Model {
  Model(
    state: IrrigationState,
    ws: Option(ws.WebSocket),
    minutes_on: Int,
    messages_rev: List(String),
    scheduled_action_form: form.Form(scheduled_action.ScheduledAction)
  )
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(Model(state: NotRunning, ws: None, minutes_on: 5, messages_rev: [], scheduled_action_form: scheduled_action_form()),
    ws.init("/ws", WsWrapper))
}

type Msg {
  UserClickedRunIrrigation
  UserClickedTurnOffIrrigation
  UserUpdatedOnTime(String)
  WsWrapper(ws.WebSocketEvent)
  UserSubmittedScheduledActionForm(result: Result(scheduled_action.ScheduledAction, form.Form(scheduled_action.ScheduledAction)))
}

type IrrigationState {
  NotRunning
  UserClickedRun
  Running
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg, model.ws {
    UserClickedRunIrrigation, Some(socket) -> {
      let seconds_on = model.minutes_on * 60
      let payload = "node1hi on " <> int.to_string(seconds_on)
      #(Model(..model, state: UserClickedRun), ws.send(socket, payload))
    }
    UserClickedRunIrrigation, None -> {
      let _ = alert("Disconnected from server, cannot start irrigation")
      #(model, effect.none())
    }
    UserClickedTurnOffIrrigation, Some(socket) -> {
      let payload = "node1hi off"
      #(Model(..model, state: UserClickedRun), ws.send(socket, payload))
    }
    UserClickedTurnOffIrrigation, None -> {
      let _ = alert("Disconnected from server, cannot turn off irrigation")
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
    UserSubmittedScheduledActionForm(result: Ok(the_scheduled_action)), Some(socket) -> {
      io.println("Woooo: " <> string.inspect(the_scheduled_action))
      #(model, ws.send(socket, json.to_string(scheduled_action.to_json(the_scheduled_action))))
    }
    UserSubmittedScheduledActionForm(result: Ok(the_scheduled_action)), None -> {
      io.println("No WS but form success Woooo: " <> string.inspect(the_scheduled_action))
      alert("Can't submit action when disconnected from server")
      #(model, effect.none())
    }
    UserSubmittedScheduledActionForm(result: Error(form)), _ -> {
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
      html.button([event.on_click(UserClickedTurnOffIrrigation)], [html.text("Turn off irrigation")]),
    ]),
    html.div([], [
      html.textarea([attribute.readonly(True),
                     attribute.styles([
                       #("width", "800"),
                       #("height", "400")])],
                    string.join(model.messages_rev, "\n"))
    ]),
    scheduled_action_form_view(model)
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

@external(javascript, "./frontend.js", "raise_alert")
fn alert(msg: String) -> Nil
