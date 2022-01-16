'''
Enables reboot over mqtt
'''
import machine
import time

import aiko.mqtt
import configuration.mqtt

ACK_MESSAGE = "ack reboot"

def on_reboot_message(topic, message):
  if message != ACK_MESSAGE:
      print("Rebooting")
      aiko.mqtt.client.publish(configuration.mqtt.settings["topic_path"] + "/reboot", ACK_MESSAGE, retain=True)
      time.sleep(0.5)
      machine.reset()

def initialise(settings=configuration.mqtt.settings):
  aiko.mqtt.add_message_handler(on_reboot_message, settings["reboot_topic"])
