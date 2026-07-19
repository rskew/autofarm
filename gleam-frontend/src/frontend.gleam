import formal/form
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, Some, None}
import gleam/order
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
import lustre_http
import lustre_websocket as ws
import shared/scheduled_action
import shared/day_of_week.{Monday}
import shared/on_or_off.{type OnOrOff, On, Off}

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

// The device topology, as loaded from /devices.json at startup. The file at the
// repo root is the source of truth for which zone is wired to which node and
// solenoid; nothing here hardcodes it.
type Device {
  Device(node: Int, name: String, solenoids: List(SolenoidWiring))
}

type SolenoidWiring {
  SolenoidWiring(solenoid: Int, zone: String)
}

/// A zone's address on the wire: #(node number, solenoid number). Solenoid
/// numbers repeat across nodes, so the node is needed to key a zone uniquely.
type ZoneId =
  #(Int, Int)

/// One controllable zone, flattened out of the topology. The user picks zones
/// by name and doesn't care which node they hang off, but we keep the address
/// here so we can talk to the right node.
type ZoneState {
  ZoneState(
    node: Int,
    solenoid: Int,
    name: String,
    on_or_off: Option(OnOrOff),
    minutes_next: String,
    scheduled_off_at: Option(Timestamp),
  )
}

type Model {
  Model(
    ws: Option(ws.WebSocket),
    zones: Dict(ZoneId, ZoneState),
    load_error: Option(String),
    messages_rev: List(String),
    scheduled_action_form: form.Form(scheduled_action.ScheduledAction)
  )
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(Model(ws: None,
          zones: dict.new(),
          load_error: None,
          messages_rev: [],
          scheduled_action_form: scheduled_action_form()),
    effect.batch([ws.init("/ws", WsWrapper), fetch_devices()]))
}

fn fetch_devices() -> effect.Effect(Msg) {
  lustre_http.get(
    origin() <> "/devices.json",
    lustre_http.expect_json(devices_decoder(), DevicesLoaded),
  )
}

fn devices_decoder() -> decode.Decoder(List(Device)) {
  use devices <- decode.field("devices", decode.list(device_decoder()))
  decode.success(devices)
}

fn device_decoder() -> decode.Decoder(Device) {
  use node <- decode.field("node", decode.int)
  use name <- decode.field("name", decode.string)
  use solenoids <- decode.field("solenoids", decode.list(solenoid_decoder()))
  decode.success(Device(node: node, name: name, solenoids: solenoids))
}

fn solenoid_decoder() -> decode.Decoder(SolenoidWiring) {
  use solenoid <- decode.field("solenoid", decode.int)
  use zone <- decode.field("zone", decode.string)
  decode.success(SolenoidWiring(solenoid: solenoid, zone: zone))
}

/// Flatten the device topology into the zone list the UI controls.
fn zones_from_devices(devices: List(Device)) -> Dict(ZoneId, ZoneState) {
  devices
  |> list.flat_map(fn(device) {
    list.map(device.solenoids, fn(wiring) {
      #(#(device.node, wiring.solenoid),
        ZoneState(node: device.node,
                  solenoid: wiring.solenoid,
                  name: wiring.zone,
                  on_or_off: None,
                  minutes_next: "20",
                  scheduled_off_at: None))
    })
  })
  |> dict.from_list
}

type Msg {
  UserClickedRunIrrigation(zone_id: ZoneId)
  UserClickedTurnOffIrrigation(zone_id: ZoneId)
  UserUpdatedOnTime(zone_id: ZoneId, on_time: String)
  WsWrapper(ws.WebSocketEvent)
  DevicesLoaded(Result(List(Device), lustre_http.HttpError))
  UserSubmittedScheduledActionForm(form_result: Result(scheduled_action.ScheduledAction, form.Form(scheduled_action.ScheduledAction)))
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg, model.ws {
    DevicesLoaded(Ok(devices)), _ -> {
      #(Model(..model, zones: zones_from_devices(devices), load_error: None),
        effect.none())
    }
    DevicesLoaded(Error(error)), _ -> {
      io.println("Failed to load devices.json: " <> string.inspect(error))
      #(Model(..model, load_error: Some(string.inspect(error))), effect.none())
    }
    UserClickedRunIrrigation(zone_id), Some(socket) -> {
      case dict.get(model.zones, zone_id) {
        Ok(zone) -> {
          case int.parse(zone.minutes_next) {
            Ok(minutes_on) -> {
              let seconds_on = minutes_on * 60
              let payload = "n" <> int.to_string(zone.node)
                <> "s" <> int.to_string(zone.solenoid)
                <> "on " <> int.to_string(seconds_on)
              let scheduled_off_at = timestamp.add(timestamp.system_time(), duration.seconds(seconds_on))
              let updated_zones = dict.insert(model.zones, zone_id, ZoneState(..zone, scheduled_off_at: Some(scheduled_off_at)))
              #(Model(..model, zones: updated_zones), ws.send(socket, payload))
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
    UserClickedRunIrrigation(_zone_id), None -> {
      let _ = alert("Disconnected from server, cannot start irrigation")
      #(model, effect.none())
    }
    UserClickedTurnOffIrrigation(zone_id), Some(socket) -> {
      case dict.get(model.zones, zone_id) {
        Ok(zone) -> {
          let payload = "n" <> int.to_string(zone.node)
            <> "s" <> int.to_string(zone.solenoid)
            <> "off"
          let updated_zones = dict.insert(model.zones, zone_id, ZoneState(..zone, scheduled_off_at: None))
          #(Model(..model, zones: updated_zones), ws.send(socket, payload))
        }
        Error(_) -> {
          alert("Failed to turn off irrigation!")
          #(model, effect.none())
        }
      }
    }
    UserClickedTurnOffIrrigation(_zone_id), None -> {
      let _ = alert("Disconnected from server, cannot turn off irrigation")
      #(model, effect.none())
    }
    UserUpdatedOnTime(zone_id, on_time), _ -> {
      case dict.get(model.zones, zone_id) {
        Ok(zone) -> {
          let updated_zones = dict.insert(model.zones, zone_id,
                                          ZoneState(..zone, minutes_next: on_time))
          #(Model(..model, zones: updated_zones), effect.none())
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
        Ok(#(node_number, solenoid_number, on_or_off)) -> {
          let zone_id = #(node_number, solenoid_number)
          case dict.get(model.zones, zone_id) {
            Ok(zone) -> {
              let new_zone_state = case on_or_off {
                On -> ZoneState(..zone, on_or_off: Some(On))
                Off -> ZoneState(..zone, on_or_off: Some(Off), scheduled_off_at: None)
              }
              let updated_zones = dict.insert(model.zones, zone_id, new_zone_state)
              #(Model(..model, zones: updated_zones), effect.none())
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

/// Zones are shown as one flat list, ordered by node then solenoid so the
/// order is stable and matches the order they're declared in devices.json.
fn compare_zones(a: ZoneState, b: ZoneState) -> order.Order {
  case int.compare(a.node, b.node) {
    order.Eq -> int.compare(a.solenoid, b.solenoid)
    other -> other
  }
}

fn view(model: Model) -> Element(Msg) {
  let zones = list.sort(dict.values(model.zones), compare_zones)
  html.div([
    attribute.styles([
      #("font-family", "sans")
    ])
  ], [
    html.div([], [
      case model.ws {
        None -> html.div([
          attribute.styles([#("color", "red"),
                            #("font-size", "13px")]),
          ], [
            html.text("Disconnected from server")
          ])
        Some(_) -> html.div([
          attribute.styles([#("color", "#0d8"),
                            #("font-size", "13px")]),
          ], [
            html.text("Connected to server")
          ])
      }
    ]),
    case model.load_error, zones {
      Some(error), _ -> html.div([
        attribute.styles([#("color", "red"),
                          #("margin-top", "10px")])
        ], [
          html.text("Couldn't load devices.json: " <> error)
        ])
      None, [] -> html.div([
        attribute.styles([#("color", "#777"),
                          #("margin-top", "10px")])
        ], [
          html.text("Loading zones...")
        ])
      None, _ -> html.div([], [])
    },
    html.div([], list.map(zones, zone_view)),
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

fn zone_view(zone: ZoneState) -> Element(Msg) {
  let zone_id = #(zone.node, zone.solenoid)
  let colour = case zone.on_or_off {
    Some(On) -> "#0af"
    Some(Off) -> "#777"
    None -> "#ccc"
  }
  html.div([attribute.data("zone", zone.name),
            attribute.styles([
    #("margin-top", "10px"),
    #("margin-bottom", "10px"),
    #("border", "2px solid " <> colour)
  ])], [
    html.div([
      attribute.styles([
        #("margin", "5px"),
        #("color", "#444"),
        #("font-size", "20px"),
        #("display", "flex"),
        #("flex-direction", "row")
      ])
    ], [
      html.text(zone.name),
      case zone.on_or_off {
        None -> html.div([attribute.data("state", "unknown")], [])
        Some(On) -> html.div([
          attribute.data("state", "on"),
          attribute.styles([
            #("color", colour), #("margin-left", "10px")
          ])], [
            html.text("watering")
          ])
        Some(Off) -> html.div([
          attribute.data("state", "off"),
          attribute.styles([
            #("color", colour), #("margin-left", "10px")
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
        html.button([attribute.data("action", "on"),
                     event.on_click(UserClickedRunIrrigation(zone_id))], [html.text("ON")]),
      ]),
      html.div([attribute.styles([#("margin-top", "10px")])], [
        html.text("for "),
        html.input([
          attribute.type_("number"),
          attribute.data("field", "minutes"),
          attribute.value(zone.minutes_next),
          attribute.styles([#("width", "5ch")]),
          event.on_input(UserUpdatedOnTime(zone_id, _)),
        ]),
        html.text(" minutes"),
      ]),
      html.div([attribute.styles([
        #("margin", "10px")
      ])], [
        html.button([attribute.data("action", "off"),
                     event.on_click(UserClickedTurnOffIrrigation(zone_id))], [html.text("OFF")])
      ]),
    ])
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

@external(javascript, "./frontend.js", "origin")
fn origin() -> String
