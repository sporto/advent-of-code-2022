interface Day05
    exposes [
        run,
        part1,
        # part2,
    ]
    imports [
        Common.{ Parser },
        pf.Task.{ Task },
    ]

run : Task Str Str
run =
    part1 "day05/sample"

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

Parsed : {
    start: Stage,
    instructions: List Instruction
}

Stage: {}

Instruction : Str

parse : Common.Parser Parsed
parse = \input ->
    { start, ins } <- splitStartAndInstructions input |> Result.try
    stage <- parseStage start |> Result.try
    instructions <- parseInstructions ins |> Result.try

    Ok {
        start: stage,
        instructions: instructions,
    }

splitStartAndInstructions = \input ->
    when Str.split input "\n\n" is
        [start, ins] ->
            Ok {start: start, ins: ins}
        _ ->
            Err "Invalid input"

parseStage : Str -> Result Stage Str
parseStage = \input ->
    Ok {}

parseInstructions : Str -> Result (List Instruction) Str
parseInstructions = \input ->
    input
    |> Str.split "\n"
    |> List.mapTry parseInstruction

parseInstruction = \input ->
    Ok input

processPart1 : Parsed -> Result Str Str
processPart1 = \parsed ->
    Ok "1"