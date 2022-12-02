interface Day01
    exposes [
        part1,
        parse,
        processPart1
    ]
    imports [
        Common.{ Parser },
        pf.Task.{ Task },
    ]

Parsed : List (List I32)


part1 : Task Str Str
part1 =
    input <- Common.readAndParse "day01/input" parse |> Task.await
    processPart1 input |> Task.fromResult


parse : Common.Parser Parsed
parse = \input ->
    Str.split input "\n\n"
    |> List.mapTry parseBlock


parseBlock : Parser (List I32)
parseBlock = \input ->
    Str.split input "\n"
    |> List.mapTry Str.toI32
    |> Result.mapErr (\_ -> "Could not parse \(input)")


processPart1 : Parsed -> Result Str Str
processPart1 = \input ->
    input
    |> List.map List.sum
    |> List.max
    |> Result.mapErr (\_ -> "Empty")
    |> Result.map Num.toStr
