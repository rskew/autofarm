import json
import machine
from machine import Pin
from threading import Thread
import time

import aiko.event
import aiko.mqtt
import configuration.main

parameter = configuration.main.parameter
device_name = parameter("device_name")


class FloatLevelSwitch():
    """The Float level switch is wired so that when the float
    is pointing down (water below sensor level) the two IO pins
    are connected together.
    We read the switch position by setting one pin to be high, and
    reading the value on the other pin (which has a pulldown resistor)."""
    WET = "WET"
    DRY = "DRY"

    def __init__(self, pin_a_number, pin_b_number):
        self.pin_a = Pin(pin_a_number, Pin.OUT, value=1)
        self.pin_b = Pin(pin_b_number, Pin.IN, Pin.PULL_DOWN)

    def read(self):
        if self.pin_b.value() == 1:
            return FloatLevelSwitch.DRY
        else:
            return FloatLevelSwitch.WET


low_level_switch = FloatLevelSwitch(18, 19)
high_level_switch = FloatLevelSwitch(16, 17)


def monitor_tank_level():
    while True:
        while not aiko.mqtt.is_connected():
            print("waiting for connection to MQTT server")
            time.sleep(1)
        tank_level = json.dumps({"low_level_switch": low_level_switch.read(),
                                 "high_level_switch": high_level_switch.read()})
        try:
            print("publishing tank level")
            aiko.mqtt.client.publish(
                device_name + "/tank_level",
                tank_level,
            )
            aiko.mqtt.client.publish(
                device_name + "/out",
                "hello",
            )
            time.sleep(3)
        except OSError as e:
            print("Got error: \"" + str(e) + "\" while trying to publish MQTT message")
        print("deep-sleeping for 5 seconds")
        machine.deepsleep(5_000)

def initialise():
    Thread(target=monitor_tank_level).start()
