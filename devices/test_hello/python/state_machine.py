from config import Config
import lib_test as lib

def main():
    lib.set_state(StateMachineState.initializing, {})
    while True:
        state_machine_state, state = lib.get_state()
        if state_machine_state == StateMachineState.initializing:
            """
            INITIALIZING
            """
            maybe_battery_voltage = lib.read_battery_voltage()
            if maybe_battery_voltage != None and maybe_battery_voltage < 3.3:
                lib.set_state(StateMachineState.deep_sleep, state)
            else:
                maybe_socket = lib.connect(**Config.connection_config)
                if maybe_socket == None:
                    lib.set_state(StateMachineState.deep_sleep, state)
                else:
                    if maybe_battery_voltage != None:
                        lib.send_message(maybe_socket, lib.to_json({"battery_voltage": maybe_battery_voltage}))
                        state["time_last_send_battery_voltage"] = lib.now()
                    state["socket"] = maybe_socket
                    lib.set_state(StateMachineState.receiving, state)

        elif state_machine_state == StateMachineState.receiving:
            """
            RECEIVING
            """
            maybe_message = lib.receive_message(state["socket"])
            if lib.Message.reboot == maybe_message:
                lib.reboot()
            elif lib.Message.query_sensor == maybe_message:
                maybe_reading = lib.read_sensor()
                if not maybe_reading is None:
                    lib.send_message(state["socket"], lib.to_json({"reading": maybe_reading}))
            elif lib.Message.do_thing_a == maybe_message:
                lib.do_thing_a()
            elif lib.Message.do_thing_b == maybe_message:
                lib.do_thing_b()
            elif lib.Message.update == maybe_message:
                state["time_enter_update_state"] = lib.now()
                lib.set_state(StateMachineState.updating, state)
                lib.sleep(1000)
            elif None != maybe_message:
                print(f"Unknown message: {maybe_message}")
            elif None               == maybe_message:
                maybe_battery_voltage = lib.read_battery_voltage()
                if maybe_battery_voltage != None:
                    now = lib.now()
                    if "time_last_send_battery_voltage" in state \
                       and now > state["time_last_send_battery_voltage"] + Config.battery_voltage_send_period_millis:
                        state["time_last_send_battery_voltage"] = now
                        lib.send_message(state["socket"], lib.to_json({"battery_voltage": maybe_battery_voltage}))
                    if maybe_battery_voltage < 3.3:
                        lib.set_state(StateMachineState.deep_sleep, state)
                    else:
                        lib.set_state(StateMachineState.receiving, state)
            lib.sleep(Config.loop_sleep_millis)

        elif state_machine_state == StateMachineState.updating:
            """
            UPDATING
            """
            maybe_message = lib.receive_message(state["socket"])
            if maybe_message != None and len(maybe_message) == 3 and lib.Message.store_file == maybe_message[0]:
                lib.store_file(maybe_message[1], maybe_message[2])

            elif lib.Message.commit_update == maybe_message:
                lib.commit_update()
                lib.set_state(StateMachineState.receiving, state)

            elif None != maybe_message:
                print(f"Unknown message: {maybe_message}")

            elif None == maybe_message:
                if lib.now() > state["time_enter_update_state"] + Config.update_timeout:
                    print("Update timeout")
                    lib.set_state(StateMachineState.receiving, state)

            lib.sleep(Config.loop_sleep_millis)

        elif state_machine_state == StateMachineState.deep_sleep:
            """
            DEEP SLEEP
            """
            if state.get("socket") != None:
                lib.close(state["socket"])
            lib.deep_sleep(Config.deep_sleep_millis)
            lib.set_state(StateMachineState.initializing, {})

class StateMachineState:
    initializing = "initializing"
    receiving = "receiving"
    updating = "updating"
    deep_sleep = "deep_sleep"


if __name__ == "__main__":
    main()
