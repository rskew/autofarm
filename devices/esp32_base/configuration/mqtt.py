# configuration/mqtt.py: version: 2020-12-13 18:30 v04

upgrade_topic = "upgrade/aiko_00"
import configuration.main
topic_path = configuration.main.settings["device_name"]

settings = {
  # TODO reference farm server IP from repo-wide config
  #"host":              "192.168.1.21", # farm server
  #"host":              "192.168.174.217", # phone hotspot
  "host":              "192.168.1.20", # laptop
  "keepalive":         60,
  "port":              1883,
  "topic_path":        topic_path,
  "topic_subscribe":   [ "irrigate/#", "pump/#", topic_path+"/#", upgrade_topic ],
  "upgrade_topic":     upgrade_topic,
  "update_file_topic": topic_path + "/update_file/#",
  "reboot_topic":      topic_path + "/reboot",
  "ping_topic":        topic_path + "/ping",

# Enable processing *INSECURE* exec() commands received via MQTT
  "mqtt_insecure_exec": False
}
