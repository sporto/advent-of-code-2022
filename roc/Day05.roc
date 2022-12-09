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

Stage: Dict Nat (List Str)

Instruction : { move: U8, from: U8, to: U8 }

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
    input
    |> Str.split "\n"
    |> List.dropLast
    |> List.mapTry parseStageRow
    |> Result.map transposeToStacks

# [N] [C]    
parseStageRow : Str -> Result (List Str) Str
parseStageRow = \input ->
    input
    # 3 spaces means an empty space
    |> Common.replace "   " "[.]"
    # Remove all spaces
    |> Common.replace " " ""
    # Remove the [ and ]
    |> Common.replace "[" ""
    |> Common.replace "]" ""
    |> Str.graphemes
    |> Ok

# We have something like
# .D.
# NC.
# ZMP
#
# We want to convert to stack
# ZN.
# MCD
# P..
transposeToStacks : List (List Str) -> Dict Nat (List Str)
transposeToStacks = \input ->
    input
    |> List.reverse
    |> Common.walkWithIndex Dict.empty (\acc, row, _ ->
        Common.walkWithIndex row acc (\innerAcc, char, charIndex ->
            Dict.update innerAcc (charIndex + 1) (\possibleValue ->
                when possibleValue is
                    Missing -> Present [char]
                    Present elems -> Present (List.append elems char)
            )
        )
    )

parseInstructions : Str -> Result (List Instruction) Str
parseInstructions = \input ->
    input
    |> Str.split "\n"
    |> List.mapTry parseInstruction

parseInstruction = \input ->
    when input |> Str.split " " is
    [_, moveStr, _, fromStr, _, toStr] ->
        move <- parseNum moveStr |> Result.try
        from <- parseNum fromStr |> Result.try
        to <- parseNum toStr |> Result.try

        Ok {
            move: move,
            from: from,
            to: to
        }
    _ -> Err "Invalid \(input)"

processPart1 : Parsed -> Result Str Str
processPart1 = \parsed ->
    # dbg parsed.start
    parsed.instructions
    |> List.walk parsed.start runInstruction
    |> List.mapTry List.last
    |> Str.joinWith ""
    |> Ok

parseNum : Str -> Result U8 Str
parseNum = \input ->
    Str.toU8 input |> Result.mapErr (\_ -> "Invalid \(input)")

runInstruction : Stage, Instruction -> Stage
runInstruction = \stage, instruction ->
    stage