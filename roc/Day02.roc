interface Day02
    exposes [
        run,
    ]
    imports [
        Common.{ Parser },
        pf.Task.{ Task },
    ]

Hand : [ Rock, Paper, Scissors ]

Round : {
    opp: Hand,
    me: Hand,
}

Parsed : List Round

sample =
    "day02/sample"

file =
    "day02/input"

run : Task Str Str
run =
    part1 file

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

parse : Common.Parser Parsed
parse = \input ->
    Str.split input "\n"
    |> List.mapTry parseRow

parseRow : Str -> Result Round Str
parseRow = \input ->
    Str.split input " "
        |> List.mapTry parseHand
        |> Result.try (\hands ->
            when hands is
                [a, b] ->
                    Ok { opp: a, me: b}
                _ ->
                    Err "Invalid row"
        )

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

processPart1 : Parsed -> Result Str Str
processPart1 = \rounds ->
    rounds
    |> List.walk 0 (\acc, round ->
        acc + roundScore round
    )
    |> Num.toStr
    |> Ok

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
        { opp : Rock, me: Scissors } -> Lost
        { opp : Rock, me: Paper } -> Win
        { opp : Paper, me: Rock } -> Lost
        { opp : Paper, me: Scissors } -> Win
        { opp : Scissors, me: Paper } -> Lost
        { opp : Scissors, me: Rock } -> Win
        _ -> Draw
