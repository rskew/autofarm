state_machine_state = "initializing"
state = {}

def set_state(sms, s):
    print(f"New state: {sms}, {s}")
    global state_machine_state
    global state
    state_machine_state = sms
    state = s

def get_state():
    global state_machine_state
    global state
    print()
    print(f"Getting state: {state_machine_state}")
    return state_machine_state, state

def to_json(thing):
    import json
    return json.dumps(thing)

def now():
    import time
    return time.time() * 1000

def reboot():
    print("Rebooting")
    set_state("initializing", {})

def sleep(millis):
    import time
    time.sleep(millis / 1000)

def commit_update():
    print("Committing Update")

def deep_sleep(millis):
    print(f"Sleeping deeply for: {millis}")
    import time
    time.sleep(millis / 1000)

def read_battery_voltage():
    import random
    voltage = random.random() * 3 + 3
    reading = random.choice([voltage] * 3 + [None])
    print(f"Battery voltage: {reading}")
    return reading

def connect(
        timeout_millis,
        host,
        port,
        wifi_ssid,
        wifi_password,
    ):
    import random
    socket = random.choice(["socket"] * 4 + [None])
    print(f"Connection: {socket}")
    return socket

def close(socket):
    print("Closing socket")

def send_message(socket, message):
    print(f"Sending message {message}")

def store_file(filename, content):
    print(f"Storing file {filename} with content {content}")

class Message:
    reboot = "reboot"
    query_sensor = "query_sensor"
    do_thing_a = "do_thing_a"
    do_thing_b = "do_thing_b"
    update = "update"
    store_file = "store_file"
    commit_update = "commit_update"

def receive_message(socket):
    import random
    message = random.choice([None] * 10 + [
        Message.reboot,
        Message.query_sensor,
        Message.do_thing_a,
        Message.do_thing_b,
        Message.update,
        (Message.store_file, "<filename>", "<content>"),
        Message.commit_update,
    ])
    print(f"Received message {message}")
    return message

def read_sensor():
    import random
    reading = random.choice([random.random() * 5] * 5 + [None])
    print(f"Sensor reading: {reading}")
    return reading

def do_thing_b():
    print("Doing thing b")

def do_thing_a():
    print("Doing thing a")
