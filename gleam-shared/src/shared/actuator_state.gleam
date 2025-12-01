import gleam/time/timestamp
import shared/action_state.{type ActionState}

pub type ActuatorState {
  ActuatorState(target_name: String, desired_state: ActionState, last_known_state: ActionState, last_heard_at: timestamp.Timestamp)
}

pub fn zero() -> ActuatorState {
  ActuatorState("", desired_state: action_state.zero(), last_known_state: action_state.zero(), last_heard_at: timestamp.from_unix_seconds(0))
}

// todo the rest and actually use it
