from config import Config

class TankLevel:
    empty = "empty"
    middle = "middle"
    full = "full"

def now():
    import time
    return time.time()

def read_tank_level():
    import random
    tank_level = random.choice([TankLevel.empty, TankLevel.middle, TankLevel.full])
    print(f"Tank level: {tank_level}")
    return tank_level
    #from machine import Pin
    #low_level_switch_pin = Pin(Config.low_level_switch_pin, Pin.IN, Pin.PULL_UP)
    #high_level_switch_pin = Pin(Config.high_level_switch_pin, Pin.IN, Pin.PULL_UP)

    #low_level_wet = low_level_switch_pin.value() == 1
    #high_level_wet = high_level_switch_pin.value() == 1
    #if high_level_wet:
    #    return TankMonitor.FULL
    #elif low_level_wet:
    #    return TankMonitor.MIDDLE
    #else:
    #    return TankMonitor.EMPTY

def set_wake_on_tank_level_critical():
    pass
    #import esp32
    #from machine import Pin
    #low_level_switch_pin = 27
    #high_level_switch_pin = 25
    #low_level_switch_pin = Pin(Config.low_level_switch_pin, Pin.IN, Pin.PULL_UP)
    #high_level_switch_pin = Pin(Config.high_level_switch_pin, Pin.IN, Pin.PULL_UP)
    #esp32.wake_on_ext0(pin=Config.high_level_switch_pin, level=esp32.WAKEUP_ANY_HIGH)
    #esp32.wake_on_ext1(pins=[Config.low_level_switch_pin], level=esp32.WAKEUP_ALL_LOW)

def read_battery_voltage():
    import random
    voltage = random.random() * 5
    print(f"Battery voltage: {voltage}")
    return voltage
    #from machine import ADC, Pin
    #battery_voltage_adc = ADC(Pin(pin))
    #battery_voltage_adc.atten(ADC.ATTN_11DB)   # set 3.3V Range
    #battery_voltage_adc.width(ADC.WIDTH_12BIT)
    #return battery_voltage_adc.read() * 2 * 3.3 / 4095


def connect(timeout_millis):
    #import time
    #start_time = time.time()
    #import network
    #sta_if = network.WLAN(network.STA_IF)
    #if not sta_if.isconnected():
    #    sta_if.active(True)
    #    sta_if.connect(Config.wifi_ssid, Config.wifi_password)
    #    while not sta_if.isconnected():
    #        if time.time() > start_time + Config.timeout_millis:
    #            return None
    import socket
    s = socket.socket()
    s.settimeout(timeout_millis)
    s.connect(Config.manager_host_and_port)
    return s

def send(s, message):
    """
    Test against `while true; nc -l <port>; end`
    """
    if s:
        print(f"Sending message: {message}")
        message_bytes = bytearray([255, 255, 255, len(message) // 256, len(message)]) + bytearray(message, "utf-8")
        s.send(message_bytes)
    #import time
    #retries = 5
    #begin_time = time.time()
    #def wait_for_mqtt():
    #    while not aiko.mqtt.is_connected():
    #        if time.time() > begin_time + 10:
    #            print("Could not connect to MQTT broker.")
    #            print("Deep-sleeping for {} millis".format(DEEPSLEEP_MILLIS))
    #            machine.deepsleep(DEEPSLEEP_MILLIS)
    #        print("waiting for connection to MQTT server")
    #        time.sleep(1)

    #for _ in range(retries):
    #    wait_for_mqtt()
    #    try:
    #        aiko.mqtt.client.publish(
    #            topic,
    #            message,
    #        )
    #        time.sleep(0.1)
    #        break
    #    except OSError as e:
    #        print("Got error: \"" + str(e) + "\" while trying to publish MQTT message")
    #        time.sleep(0.5)

def go_sleep(millis):
    import time
    print(f"Sleeping for {millis}")
    time.sleep(millis / 1000)
    print()
    #import machine
    #machine.deepsleep(millis)

state = "initialize"

def set_state(new_state):
    global state
    state = new_state
    print(f"New state: {new_state}")

def get_state():
    global state
    return state
