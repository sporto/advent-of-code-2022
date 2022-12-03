interface Day02
    exposes [
        run,
    ]
    imports [
        Common.{ Parser },
        pf.Task.{ Task },
    ]

Hand : [Rock, Paper, Scissors]

Round : {
    opp : Hand,
    me : Hand,
}

RoundPart2 : {
    opp : Hand,
    outcome : Outcome,
}

Parsed : List Round

ParsedPart2 : List RoundPart2

sample =
    "day02/sample"

file =
    "day02/input"

run : Task Str Str
run =
    part2 file

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

part2 : Str -> Task Str Str
part2 = \f ->
    input <- Common.readAndParse f parsePart2 |> Task.await
    processPart2 input |> Task.fromResult

parse : Common.Parser Parsed
parse = \input ->
    Str.split input "\n"
    |> List.mapTry parseRow

parsePart2 : Common.Parser ParsedPart2
parsePart2 = \input ->
    Str.split input "\n"
    |> List.mapTry parseRowPart2

parseRow : Str -> Result Round Str
parseRow = \input ->
    Str.split input " "
    |> List.mapTry parseHand
    |> Result.try
        (\hands ->
            when hands is
                [a, b] ->
                    Ok { opp: a, me: b }

                _ ->
                    Err "Invalid row"
        )

parseRowPart2 : Str -> Result RoundPart2 Str
parseRowPart2 = \input ->
    when Str.split input " " is
        [a, b] ->
            opp <- parseHand a |> Result.try
            outcome <- parseOutcome b |> Result.try
            Ok { opp: opp, outcome: outcome }

        _ ->
            Err "Invalid row"

parseHand : Str -> Result Hand Str
parseHand = \char ->
    when char is
        "A" -> Ok Rock
        "B" -> Ok Paper
        "C" -> Ok Scissors
        "X" -> Ok Rock
        "Y" -> Ok Paper
        "Z" -> Ok Scissors
        _ -> Err char

parseOutcome : Str -> Result Outcome Str
parseOutcome = \char ->
    when char is
        "X" -> Ok Lost
        "Y" -> Ok Draw
        "Z" -> Ok Win
        _ -> Err char

processPart1 : Parsed -> Result Str Str
processPart1 = \rounds ->
    rounds
    |> List.walk
        0
        (\acc, round ->
            acc + roundScore round
        )
    |> Num.toStr
    |> Ok

processPart2 = \rounds ->
    rounds
    |> List.map roundPart2ToPart1
    |> processPart1

roundPart2ToPart1 : RoundPart2 -> Round
roundPart2ToPart1 = \round -> {
    opp: round.opp,
    me: handForOutcome round.opp round.outcome,
}

handForOutcome : Hand, Outcome -> Hand
handForOutcome = \hand, outcome ->
    when outcome is
        Lost ->
            when hand is
                Rock ->
                    Scissors

                Paper ->
                    Rock

                Scissors ->
                    Paper

        Draw ->
            hand

        Win ->
            when hand is
                Rock ->
                    Paper

                Paper ->
                    Scissors

                Scissors ->
                    Rock

roundScore : Round -> I16
roundScore = \round ->
    scoreForShape =
        when round.me is
            Rock -> 1
            Paper -> 2
            Scissors -> 3
    scoreForOutcome =
        when getOutcome round is
            Lost -> 0
            Draw -> 3
            Win -> 6

    scoreForShape + scoreForOutcome

Outcome : [Lost, Draw, Win]

getOutcome : Round -> Outcome
getOutcome = \round ->
    when round is
        { opp: Rock, me: Scissors } -> Lost
        { opp: Rock, me: Paper } -> Win
        { opp: Paper, me: Rock } -> Lost
        { opp: Paper, me: Scissors } -> Win
        { opp: Scissors, me: Paper } -> Lost
        { opp: Scissors, me: Rock } -> Win
        _ -> Draw
