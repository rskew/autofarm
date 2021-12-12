from threading import Thread
import time

most_recent_update = {}

def delayed_action(action, wait_ms, action_name, action_id):
    '''
    Schedule an action to run once some time in the future.
    The action name and id are used to cancel older scheduled actions
    for the same name.
    Note: A timestamp makes a great action_id when creating a delayed_action.
    '''
    def action():
        global most_recent_update
        most_recent_update[action_name] = action_id
        time.sleep(wait_ms / 1000.)
        # Check we're not going to override a newer action
        if most_recent_update[action_name] == action_id:
            action()
        else:
            print("Not performing overridden action: " + action_name)
    Thread(target=action).start()
