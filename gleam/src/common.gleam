import gleam/dynamic.{Dynamic}
import gleam/result

pub external fn read_file(name: String) -> Result(String, Dynamic) =
  "file" "read_file"

pub fn read_and_parse(
  file_name: String,
  parse: fn(String) -> Result(a, String),
) -> Result(a, String) {
  read_file(file_name)
  |> result.replace_error("Couldn't read file")
  |> result.then(parse)
}
