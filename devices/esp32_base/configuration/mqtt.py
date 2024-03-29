# configuration/mqtt.py: version: 2020-12-13 18:30 v04

import configuration.main
topic_path = configuration.main.settings["device_name"]

settings = {
  # TODO reference farm server IP from repo-wide config
  "host":              "192.168.1.21", # farm server
  #"host":              "192.168.174.217", # phone hotspot
  #"host":              "192.168.1.20", # laptop
  "keepalive":         60,
  "port":              1883,
  "topic_path":        topic_path,
  "topic_subscribe":   [ topic_path+"/in/#" ],
  "update_file_topic": topic_path + "/in/update_file/#",
  "reboot_topic":      topic_path + "/in/reboot",
  "ping_topic":        topic_path + "/in/ping",

# Enable processing *INSECURE* exec() commands received via MQTT
  "mqtt_insecure_exec": False
}
