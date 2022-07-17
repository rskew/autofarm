import esp32
import json
import machine
from machine import ADC, Pin
from threading import Thread
import time

import aiko.event
import aiko.mqtt
import configuration.main

CONNECT_TIMEOUT_SECONDS = 60
DEEPSLEEP_MINUTES = 10
DEEPSLEEP_MILLIS = DEEPSLEEP_MINUTES * 60 * 1000

parameter = configuration.main.parameter
device_name = parameter("device_name")

startup_time = time.time()


def wait_for_mqtt():
    while not aiko.mqtt.is_connected():
        if time.time() > startup_time + CONNECT_TIMEOUT_SECONDS:
            print("Could not connect to MQTT broker.")
            print("Deep-sleeping for {} millis".format(DEEPSLEEP_MILLIS))
            machine.deepsleep(DEEPSLEEP_MILLIS)
        print("waiting for connection to MQTT server")
        time.sleep(1)


def publish_with_retries(topic, message, retries=5):
    for _ in range(retries):
        wait_for_mqtt()
        try:
            aiko.mqtt.client.publish(
                topic,
                message,
            )
            time.sleep(0.1)
            break
        except OSError as e:
            print("Got error: \"" + str(e) + "\" while trying to publish MQTT message")
            time.sleep(0.5)


class BatteryMonitor():
    def __init__(self, pin):
        self.battery_voltage_adc = ADC(Pin(pin))
        self.battery_voltage_adc.atten(ADC.ATTN_11DB)   # set 3.3V Range
        self.battery_voltage_adc.width(ADC.WIDTH_12BIT)

    def read_voltage(self):
        """Battery voltage goes via 1:1 voltage divider, than into 12bit ADC"""
        return self.battery_voltage_adc.read() * 2 * 3.3 / 4095

battery_monitor = BatteryMonitor(pin=35)

class TankMonitor():
    """The Float level switch is wired so that when the float
    is pointing down (water below sensor level) the blue wire is connected to the
    black wire.
    The black wire is connected to ground, and the blue wire to the pin.
    """
    WET = "WET"
    DRY = "DRY"

    FULL = "FULL"
    EMPTY = "EMPTY"
    MIDDLE = "MIDDLE"

    def __init__(self, low_level_switch_pin, high_level_switch_pin):
        self.low_level_switch_pin = Pin(low_level_switch_pin, Pin.IN, Pin.PULL_UP)
        self.high_level_switch_pin = Pin(high_level_switch_pin, Pin.IN, Pin.PULL_UP)

    def _read_pin(self, pin):
        if pin.value() == 0:
            return TankMonitor.DRY
        else:
            return TankMonitor.WET

    def read(self):
        low_level = self._read_pin(self.low_level_switch_pin)
        high_level = self._read_pin(self.high_level_switch_pin)
        if high_level == TankMonitor.WET:
            return TankMonitor.FULL
        elif low_level == TankMonitor.WET:
            return TankMonitor.MIDDLE
        else:
            return TankMonitor.EMPTY

    def critical(self):
        return self.read() in [TankMonitor.FULL, TankMonitor.EMPTY]

    def wake_on_critical(self):
        """Use wake_on_ext0 and wake_on_ext1 to allow having two switches,
        one with each critical state."""
        esp32.wake_on_ext0(pin=self.high_level_switch_pin, level=esp32.WAKEUP_ANY_HIGH)
        esp32.wake_on_ext1(pins=[self.low_level_switch_pin], level=esp32.WAKEUP_ALL_LOW)

tank_monitor = TankMonitor(
    low_level_switch_pin=27,
    high_level_switch_pin=25,
)

def monitor_tank_level():
    start_time = time.time()
    while True:
        if tank_monitor.critical():
            print("Tank level CRITICAL")
            # If critical, retry lots of times with very important message
            retries = 50
        else:
            print("Tank level nominal")
            # If the reading is nominal, configure to wakeup if the state changes to critical.
            tank_monitor.wake_on_critical()
            retries = 5
            # If tank level is not critical and battery is low,
            # go back to deepsleep without reporting tank level to conserve power
            # If voltage is < 1V, the battery is probably not connected.
            battery_voltage = battery_monitor.read_voltage()
            if 1 < battery_voltage < 3.3:
                print("Battery critically low: {}".format(battery_voltage))
                print("Deep-sleeping for {} millis".format(DEEPSLEEP_MILLIS))
                machine.deepsleep(DEEPSLEEP_MILLIS)
            else:
                print("Battery voltage not critical: {}".format(battery_voltage))

        print("publishing tank level")
        publish_with_retries(
            device_name + "/out/tank_level",
            tank_monitor.read(),
            retries=retries,
        )
        publish_with_retries(
            device_name + "/out/battery_voltage",
            "{:.3f}".format(battery_monitor.read_voltage()),
        )
        time.sleep(3)

        print("Deep-sleeping for {} millis".format(DEEPSLEEP_MILLIS))
        machine.deepsleep(DEEPSLEEP_MILLIS)

def initialise():
    Thread(target=monitor_tank_level).start()
