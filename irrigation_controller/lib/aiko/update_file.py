'''
To update applications/asdf.py, the topic will be e.g.:
    'irrigation_controller/update_file/applications/asdf.py'
'''
import shutil

import aiko.mqtt
import configuration.mqtt

def on_update_file_message(topic, payload):
  '''
  Single-file upgrade
  '''
  filename = '/'.join(topic.split('/')[2:])
  print("Updating file " + filename)
  shutil.make_directories(filename)
  with open(filename, "w") as file:
    file.write(payload)
  print("File update complete")
  aiko.mqtt.client.publish(
      configuration.mqtt.settings["topic_path"] + "/ack/" + '/'.join(topic.split('/')[1:]),
      "Successfully upgraded " + filename
  )

def initialise(settings=configuration.mqtt.settings):
  aiko.mqtt.add_message_handler(on_update_file_message, settings["update_file_topic"])
