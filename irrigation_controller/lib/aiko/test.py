# lib/aiko/test.py: version: 2020-12-13 18:30 v04
#
# Usage
# ~~~~~
# import aiko.test as test
# test.echo("(pass test.echo)")
# test.set_gpio_pin_list([19, 22])    # Set list of GPIO pin numbers
# test.get_gpio_pin_list()            # Get list of GPIO pin numbers
# test.set_gpio_pins_mode(0)          # GPIO mode: Output
# test.set_gpio_pins_mode(1)          # GPIO mode: Input pull-down
# test.set_gpio_pins_mode(2)          # GPIO mode: Input pull-up
# test.gpio_pins                      # Check GPIO pins have been initialised
# test.set_gpio_pins_value(0)         # Set all GPIO pins low
# test.set_gpio_pins_value(1)         # Set all GPIO pins high
# test.set_gpio_pin_value(index, 0)   # Set single GPIO pin[index] low
# test.set_gpio_pin_value(index, 1)   # Set single GPIO pin[index] high
# test.get_gpio_pins_value()          # Check all GPIO pin input values
# test.wait_gpio_pin_value(index, 0)  # Wait for GPIO pin value to be low
# test.wait_gpio_pin_value(index, 1)  # Wait for GPIO pin value to be high
#
# test.set_touch_pin_list([12, 14])   # Set list of touch pin numbers
# test.get_touch_pin_list()           # Get list of touch pin numbers
# test.get_touch_pins_value()         # Check all touch pin input values
# test.wait_touch_pin_pressed(index)  # Wait for touch pin to be pressed
#
# To Do
# ~~~~~
# - Include touch slider code that writes to OLED screen
#   as a callable function that starts a background thread

import machine
from time import sleep_ms

import aiko.common as common

gpio_pin_list = [
  19,  # SAO#1
  22,  #  " "
  33,  # SAO#2
  32,  #  " "
  23,  # SAO#3
  18,  #  " "
  25,  # SAO#4
  26,  #  " "
#  0,  # Left side header
   2,  #  "    "    "  "
  13,  #  "    "    "  "
# 34,  # Right side header (input only)
# 35   #  "     "    "  "  (input only)
]

touch_pin_list = [
  15,  # top left touch button (T3)
  12,  # bottom left touch button (T5)
  27,  # top right touch button (T7)
  14   # bottom right touch button (T6)
]

# 16  # left OLED button
# 17  # right OLED button

gpio_pin_mode = [
  (machine.Pin.OUT, None),
  (machine.Pin.IN, machine.Pin.PULL_DOWN),
  (machine.Pin.IN, machine.Pin.PULL_UP)
]

gpio_pins = None

def echo(message):
  print(message)

def get_gpio_pin_list():
  print("(pass get_gpio_pin_list: " + str(gpio_pin_list) + ")")

def get_gpio_pins_value():
   values = []
   for gpio_pin in gpio_pins:
     values.append(gpio_pin.value())
   print("(pass get_gpio_pins_value: " + str(values) + ")")

def get_touch_pin_list():
  print("(pass get_touch_pin_list: " + str(touch_pin_list) + ")")

def get_touch_pins_value():
  values = []
  for touch_pin_number in touch_pin_list:
    values.append(machine.TouchPad(machine.Pin(touch_pin_number)).read())
  print("(pass get_touch_pins_value: " + str(values) + ")")

def log(message):
  common.log(message)
  print("(pass log)")

def set_gpio_pin_list(_gpio_pin_list):
  global gpio_pin_list
  gpio_pin_list = _gpio_pin_list
  print("(pass set_gpio_pin_list)")

def set_touch_pin_list(_touch_pin_list):
  global touch_pin_list
  touch_pin_list = _touch_pin_list
  print("(pass set_touch_pin_list)")

def set_gpio_pins_mode(mode, silent=False):
  global gpio_pin_list, gpio_pins
  gpio_pins = []

  direction = gpio_pin_mode[mode][0]
  pull = gpio_pin_mode[mode][1]

  for gpio_pin_number in gpio_pin_list:
    if pull:
      gpio_pin = machine.Pin(gpio_pin_number, direction, pull)  # Input
    else:
      try:
        gpio_pin = machine.Pin(gpio_pin_number, direction)  # Output
      except ValueError:
        print("(fail set_gpio_pins_mode input only: " + str(gpio_pin_number) + ")")
        return
    gpio_pins.append(gpio_pin)

  if not silent:
    print("(pass set_gpio_pins_mode)")

def set_gpio_pin_value(index, value):
  gpio_pins[index].value(value)
  print("(pass set_gpio_pin_value)")

def set_gpio_pins_value(value):
  for gpio_pin in gpio_pins:
    gpio_pin.value(value)
  print("(pass set_gpio_pins_value)")

def wait_gpio_pin_value(index, value):
  mode = 2  # IN, PULL_UP
  if value:
    mode = 1  # IN, PULL_DOWN
  set_gpio_pins_mode(mode, silent=True)
  gpio_pin = gpio_pins[index]
  while gpio_pin.value() != value:
    sleep_ms(50)
  print("(pass wait_gpio_pin_value)")

def wait_touch_pin_pressed(index):
  touch_pin_number = touch_pin_list[index]
  touch_pin = machine.TouchPad(machine.Pin(touch_pin_number))
  while touch_pin.read() > 200:
    sleep_ms(50)
  print("(pass wait_touch_pin_pressed)")

print("(pass aiko.test %s)" % common.serial_id())
