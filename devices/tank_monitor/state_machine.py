from config import Config
import lib
from lib import TankLevel

def main():
    socket = None
    lib.set_state(State.initialize)
    while True:

        if lib.get_state() == State.initialize:
            """
            INITIALIZE
            Move on to sending tank level if level is critical or battery is not empty
            """
            tank_level = lib.read_tank_level()
            battery_voltage = lib.read_battery_voltage()
            lib.set_state(tank_level)

        if lib.get_state() in [TankLevel.empty, TankLevel.full]:
            """
            LEVEL CRITICAL
            """
            socket = lib.connect(Config.connect_timeout_millis)
            lib.set_state(State.send)

        if lib.get_state() == TankLevel.middle:
            """
            LEVEL MIDDLE
            """
            # If the reading is nominal, configure to wakeup if the state changes to critical.
            lib.set_wake_on_tank_level_critical()

            # If tank level is not critical and battery is low,
            # go back to deepsleep without reporting tank level to conserve power
            # If voltage is < 1V, the battery is probably not connected.
            if 1 < battery_voltage < 3.3:
                lib.set_state(State.go_sleep)
            else:
                socket = lib.connect(Config.connect_timeout_millis)
                lib.set_state(State.send)

        elif lib.get_state() == State.send:
            """
            SEND
            """
            import json
            lib.send(socket, json.dumps({"tank_level": tank_level, "battery_voltage": battery_voltage}))
            lib.set_state(State.go_sleep)

        elif lib.get_state() == State.go_sleep:
            """
            GO SLEEP
            """
            if socket:
                socket.close()
            lib.go_sleep(Config.deep_sleep_millis)
            lib.set_state(State.initialize)

class State:
    initialize = "initialize"
    send = "send"
    go_sleep = "go_sleep"


if __name__ == "__main__":
    main()
