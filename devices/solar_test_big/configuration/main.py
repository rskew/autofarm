# configuration/main.py: version: 2021-01-22 18:00 v05

settings = {
  "application":     "applications/solar_test",
  "denye_pins":       [12, 14],  # If touch_pins pressed, don't run "main.py"
  "logger_enabled":   False,     # Display everyone's log output
  "oled_enabled":     False,      # OLED attached
  "services_enabled": False,     # Use Aiko Services infrastructure
  "device_name":      "solar_test_big",
}

def parameter(name, settings=settings):
  if name in settings: return settings[name]
  return False
