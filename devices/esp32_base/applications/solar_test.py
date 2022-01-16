import machine
from threading import Thread
import time

import aiko.event
import aiko.mqtt
import configuration.main

parameter = configuration.main.parameter
device_name = parameter("device_name")

def report_upness():
    while True:
        while not aiko.mqtt.is_connected():
            print("waiting for connection to MQTT server")
            time.sleep(1)
        try:
            print("publishing upness")
            aiko.mqtt.client.publish(
                "solar_test/upness",
                device_name,
               )
            time.sleep(1)
        except OSError as e:
            print("Got error: \"" + str(e) + "\" while trying to publish MQTT message")
        print("deep-sleeping for 30 seconds")
        machine.deepsleep(30_000)

def initialise():
    Thread(target=report_upness).start()
    #aiko.event.add_timer_handler(report_upness_event, 10_000)
