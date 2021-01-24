'''
Enables responding to pings over mqtt
'''
import aiko.mqtt
import configuration.mqtt

def on_ping_message(topic, payload):
  print("Received ping over MQTT")
  aiko.mqtt.client.publish(
      configuration.mqtt.settings["topic_path"] + "/ack/ping",
      "Hello!",
  )

def initialise(settings=configuration.mqtt.settings):
  aiko.mqtt.add_message_handler(on_ping_message, settings["ping_topic"])
