import common
import gleam/int
import gleam/io
import gleam/list
import gleam/map.{Map}
import gleam/queue.{Queue}
import gleam/result
import gleam/string

pub fn run() {
  part1_main()
}

fn read_input(file: String) {
  common.read_and_parse(file, parse)
}

fn part1_sample() {
  part1("./data/05/sample")
}

fn part1_main() {
  part1("./data/05/input")
}

fn part1(input) {
  try #(stage, instructions) = read_input(input)
  try out = process_part_1(stage, instructions)

  try tops =
    out
    |> map.values
    |> list.try_map(fn(q) {
      try #(head, _) =
        queue.pop_front(q)
        |> result.replace_error("Unable to pop")
      Ok(head)
    })

  tops
  |> string.join("")
  |> Ok
}

fn process_part_1(stage: Stage, instructions: List(Instruction)) {
  io.debug(stage)

  list.try_fold(
    over: instructions,
    from: stage,
    with: process_part_1_instruction,
  )
}

fn process_part_1_instruction(
  stage: Stage,
  instruction: Instruction,
) -> Result(Stage, String) {
  let move = instruction.move
  let from = instruction.from
  let to = instruction.to

  // move until move is 0
  case move {
    0 -> Ok(stage)
    _ -> {
      try next_stage = move_one(stage, from, to)
      let next_instruction = Instruction(..instruction, move: move - 1)
      process_part_1_instruction(next_stage, next_instruction)
    }
  }
}

fn move_one(stage: Stage, from: Int, to: Int) -> Result(Stage, String) {
  // get top from from
  try from_queue =
    map.get(stage, from)
    |> result.replace_error("Unable to get from queue")

  try to_queue =
    map.get(stage, to)
    |> result.replace_error("Unable to get to queue")

  try #(from_top, next_from) =
    queue.pop_front(from_queue)
    |> result.replace_error("Unable to pop element")

  let next_to = queue.push_front(to_queue, from_top)

  stage
  |> map.insert(from, next_from)
  |> map.insert(to, next_to)
  |> Ok
}

fn parse(content: String) -> Result(#(Stage, List(Instruction)), String) {
  try res =
    string.split_once(content, "\n\n")
    |> result.replace_error("Couldn't split")

  let #(top, bottom) = res

  try stage = parse_stage(top)
  try instructions = parse_instructions(bottom)

  Ok(#(stage, instructions))
}

type Stage =
  Map(Int, Queue(String))

fn parse_stage(input: String) -> Result(Stage, String) {
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
    |> list.index_map(fn(ix, element) { #(ix + 1, element) })
    |> map.from_list

  Ok(stacks)
}

fn parse_stage_row(input: String) -> Result(List(String), String) {
  input
  |> string.replace(each: "    ", with: " [.]")
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

type Instruction {
  Instruction(move: Int, from: Int, to: Int)
}

fn parse_instruction(input: String) -> Result(Instruction, String) {
  case string.split(input, " ") {
    [_, move_str, _, from_str, _, to_str] -> {
      try move =
        int.parse(move_str)
        |> result.replace_error(move_str)
      try from =
        int.parse(from_str)
        |> result.replace_error(from_str)
      try to =
        int.parse(to_str)
        |> result.replace_error(to_str)
      Ok(Instruction(move, from, to))
    }
    _ -> Error("Invalid")
  }
}
