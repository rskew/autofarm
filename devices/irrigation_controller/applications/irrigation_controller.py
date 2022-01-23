import machine
from machine import Pin

import json
import time

import aiko.mqtt
import aiko.event
from aiko.delayed_action import delayed_action

import configuration.mqtt


led = Pin(2, Pin.OUT, value=0)

class PinPeripheral():
    ACTIVE_LOW = "ACTIVE_LOW"
    ACTIVE_HIGH = "ACTIVE_HIGH"

    def __init__(self, pin_number, in_or_out, active_high_or_low):
        if active_high_or_low == ACTIVE_LOW:
            self.on_value = 0
            self.off_value = 1
        else:
            self.on_value = 1
            self.off_value = 0
        self.pin = Pin(pin_number, in_or_out, value=start_value)

    def on(self):
        self.pin.value(self.on_value)

    def off(self):
        self.pin.value(self.off_value)

#def pin_writer(pin, value):
#    def closure():
#        pin.value(value)
#    return closure

# relay control signal is active low
#poly_tunnel_pin = Pin(17, Pin.OUT, value=1)
#pumpkins_pin = Pin(4, Pin.OUT, value=1)
#top_row_pin = Pin(16, Pin.OUT, value=1)
#bore_pump_pin = Pin(5, Pin.OUT, value=1)

# relay control signal is active low
poly_tunnel_pin = PinPeripheral(17, Pin.OUT, OFF, PinPeripheral.ACTIVE_LOW)
poly_tunnel_pin.off()

pumpkins_pin    = PinPeripheral(4,  Pin.OUT, OFF, PinPeripheral.ACTIVE_LOW)
pumpkins_pin.off()

top_row_pin     = PinPeripheral(16, Pin.OUT, OFF, PinPeripheral.ACTIVE_LOW)
top_row_pin.off()

bore_pump_pin   = PinPeripheral(5,  Pin.OUT, OFF, PinPeripheral.ACTIVE_LOW)
bore_pump_pin.off()

#farm_thing_controls = {
#    "PolyTunnel": {"ON": pin_writer(poly_tunnel_pin, 0),
#                   "OFF": pin_writer(poly_tunnel_pin, 1)},
#    "Pumpkins":   {"ON": pin_writer(pumpkins_pin, 0),
#                   "OFF": pin_writer(pumpkins_pin, 1)},
#    "TopRow":     {"ON": pin_writer(top_row_pin, 0),
#                   "OFF": pin_writer(top_row_pin, 1)},
#    "BorePump":   {"ON": pin_writer(bore_pump_pin, 0),
#                   "OFF": pin_writer(bore_pump_pin, 1)},
#}

farm_devices = {
    "PolyTunnel": poly_tunnel_pin,
    "Pumpkins": pumpkins_pin,
    "TopRow": top_row_pin,
    "BorePump":  bore_pump_pin,
}

def on_farm_verb(topic, message_raw):
    '''
    Act on farm control messages defined in `../../farm-control/Main.hs`
    '''
    print(topic, message_raw)
    farmVerb = topic.split('/')[0]
    # We could switch on irrigate vs pump root topics, but no point atm
    # as they are handled the same.
    farm_thing, start_stop = topic.split('/')[1:3]
    aiko.mqtt.client.publish(
        configuration.mqtt.settings["topic_path"] + "/ack/" + topic,
        start_stop + " " + farm_thing
    )
    if start_stop == "stop":
        #farm_thing_controls[farm_thing]["OFF"]()
        farm_devices[farm_thing].off()
    elif start_stop == "start":
        #farm_thing_controls[farm_thing]["ON"]()
        message = json.loads(message_raw)
        farm_devices[farm_thing].on()
        def off_action():
            #farm_thing_controls[farm_thing]["OFF"]()
            farm_devices[farm_thing].off()
            aiko.mqtt.client.publish(
                configuration.mqtt.settings["topic_path"] + "/ack/" + topic,
                "stop " + farm_thing,
            )
        delayed_action(
            off_action,
            message["duration"] * 1000,
            farm_thing,
            message["timestamp"],
        )
    else:
        aiko.mqtt.client.publish(
            configuration.mqtt.setting["topic_path"] + "/ack/" + topic,
            "ERROR: unknown action: " + start_stop
        )

def initialise():
    #farm_thing_controls["PolyTunnel"]["OFF"]()
    #farm_thing_controls["Pumpkins"]["OFF"]()
    #farm_thing_controls["TopRow"]["OFF"]()
    #farm_thing_controls["BorePump"]["OFF"]()
    # Not sure if we need to set these here, as they are set on load at the top
    farm_devices["PolyTunnel"].off()
    farm_devices["Pumpkins"].off()
    farm_devices["TopRow"].off()
    farm_devices["BorePump"].off()

    aiko.mqtt.add_message_handler(on_farm_verb, "irrigate/#")
    aiko.mqtt.add_message_handler(on_farm_verb, "pump/#")
