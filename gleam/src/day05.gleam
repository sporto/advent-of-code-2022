import common
import gleam/string
import gleam/result
import gleam/list
import gleam/io
import gleam/queue

pub fn run() {
  part1_sample()
}

fn read_input(file: String) {
  common.read_and_parse(file, parse)
}

fn part1_sample() {
  part1("./data/05/sample")
}

fn part1(input) {
  try input = read_input(input)
  Ok(input)
}

fn parse(content: String) -> Result(String, String) {
  try res =
    string.split_once(content, "\n\n")
    |> result.replace_error("Couldn't split")

  let #(top, bottom) = res

  try stage = parse_stage(top)
  try instructions = parse_instructions(bottom)

  // io.debug(stage)
  io.debug(instructions)
  Ok("")
}

fn parse_stage(input: String) {
  let rows =
    input
    |> string.split("\n")
    |> list.reverse
    |> list.drop(1)

  try parsed_rows =
    rows
    |> list.try_map(parse_stage_row)

  let stacks =
    parsed_rows
    |> list.transpose
    |> list.map(fn(stack) {
      stack
      |> list.reverse
      |> list.filter(is_not_empty)
      |> queue.from_list
    })

  Ok(stacks)
}

fn parse_stage_row(input: String) -> Result(List(String), String) {
  input
  |> string.replace(each: "   ", with: "[.]")
  |> string.replace(each: " ", with: "")
  |> string.replace("[", "")
  |> string.replace("]", "")
  |> string.to_graphemes
  |> Ok
}

fn is_not_empty(c: String) {
  c != "."
}

fn parse_instructions(input: String) {
  input
  |> string.split("\n")
  |> list.try_map(parse_instruction)
}

fn parse_instruction(input: String) {
  case string.split(input, " ") {
    [_, move, _, from, _, to] -> Ok(move)
    _ -> Error("Invalid")
  }
}
