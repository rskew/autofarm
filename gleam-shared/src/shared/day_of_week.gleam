import gleam/dynamic/decode
import gleam/string
import gleam/result

pub type DayOfWeek {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday
  Sunday
}

pub fn decoder() -> decode.Decoder(DayOfWeek) {
  decode.new_primitive_decoder("DayOfWeek", fn(data) {
    case decode.run(data, decode.string)
         |> result.map(parse) {
      Ok(Ok(day_of_week)) -> Ok(day_of_week)
      _ -> Error(Monday)
    }
  })
}

pub fn parse(day_of_week_string) -> Result(DayOfWeek, String) {
  case string.lowercase(day_of_week_string) {
      "monday" -> Ok(Monday)
      "tuesday" -> Ok(Tuesday)
      "wednesday" -> Ok(Wednesday)
      "thursday" -> Ok(Thursday)
      "friday" -> Ok(Friday)
      "saturday" -> Ok(Saturday)
      "sunday" -> Ok(Sunday)
      other -> Error("Not a day of week: " <> other)
  }
}

pub fn to_string(day_of_week: DayOfWeek) -> String {
  case day_of_week {
    Monday -> "Monday"
    Tuesday -> "Tuesday"
    Wednesday -> "Wednesday"
    Thursday -> "Thursday"
    Friday -> "Friday"
    Saturday -> "Saturday"
    Sunday -> "Sunday"
  }
}
