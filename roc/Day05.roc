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
    part1 "day05/input"

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

Parsed : {
    start: Stage,
    instructions: List Instruction
}

Stage: Dict Nat (List Str)

Instruction : { move: Nat, from: Nat, to: Nat }

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
            when char is
                "." ->
                    innerAcc
                _ ->
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
    evaluated <- parsed.instructions
        |> List.walkTry parsed.start runInstruction
        |> Result.try

    # dbg evaluated
    # Ok (printStage evaluated)

    evaluated
    |> Dict.values
    |> List.mapTry (\l ->
        List.last l
        |> Result.mapErr (\_ -> "Empty")
    )
    |> Result.map (\r -> Str.joinWith r "")

printStage : Stage -> Str
printStage = \stage ->
    stage
    |> Dict.values
    |> List.map (\stack -> Str.joinWith stack "")
    |> Str.joinWith "\n"

parseNum : Str -> Result Nat Str
parseNum = \input ->
    Str.toNat input
    |> Result.mapErr (\_ -> "Invalid \(input)")

runInstruction : Stage, Instruction -> Result Stage Str
runInstruction = \stage, ins ->
    # dbg stage
    originList <- Dict.get stage ins.from
        |> Result.mapErr (\_ -> "Not found")
        |> Result.try

    targetList <- Dict.get stage ins.to
        |> Result.mapErr (\_ -> "Not found")
        |> Result.try

    elementsToMove = originList |> List.takeLast ins.move |> List.reverse
    nextOriginList = originList |> List.reverse |> List.drop ins.move |> List.reverse
    nextTargetList = List.concat targetList elementsToMove

    stage
    |> Dict.insert ins.from nextOriginList
    |> Dict.insert ins.to nextTargetList
    |> Ok