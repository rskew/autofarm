import machine
from machine import Pin

import json
import time

import aiko.mqtt
import aiko.event
from aiko.delayed_action import delayed_action

import configuration.mqtt

device_name = configuration.main.settings["device_name"]

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

# relay control signal is active low
poly_tunnel_pin = PinPeripheral(17, Pin.OUT, OFF, PinPeripheral.ACTIVE_LOW)
poly_tunnel_pin.off()

pumpkins_pin    = PinPeripheral(4,  Pin.OUT, OFF, PinPeripheral.ACTIVE_LOW)
pumpkins_pin.off()

top_row_pin     = PinPeripheral(16, Pin.OUT, OFF, PinPeripheral.ACTIVE_LOW)
top_row_pin.off()

bore_pump_pin   = PinPeripheral(5,  Pin.OUT, OFF, PinPeripheral.ACTIVE_LOW)
bore_pump_pin.off()

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
    farm_thing, start_stop = topic.split('/')[2:4]
    aiko.mqtt.client.publish(
        "/".join(["irrigation_controller/out", farmVerb, farm_thing, start_stop, "ack"]),
        start_stop + " " + farm_thing
    )
    if start_stop == "stop":
        farm_devices[farm_thing].off()
    elif start_stop == "start":
        message = json.loads(message_raw)
        farm_devices[farm_thing].on()
        def off_action():
            farm_devices[farm_thing].off()
            aiko.mqtt.client.publish(
                "/".join(["irrigation_controller/out", farmVerb, farm_thing, "stop"]),
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
            configuration.mqtt.setting["topic_path"] + "/out/" + topic + "/ack",
            "ERROR: unknown action: " + start_stop
        )

def initialise():
    farm_devices["PolyTunnel"].off()
    farm_devices["Pumpkins"].off()
    farm_devices["TopRow"].off()
    farm_devices["BorePump"].off()

    aiko.mqtt.add_message_handler(on_farm_verb, "irrigation_controller/in/irrigate/#")
    aiko.mqtt.add_message_handler(on_farm_verb, "irrigation_controller/in/pump/#")
