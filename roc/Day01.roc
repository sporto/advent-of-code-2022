interface Day01
    exposes [
        run,
    ]
    imports [
        Common.{ Parser },
        pf.Task.{ Task },
    ]

Parsed : List (List I32)

sample =
    "day01/sample"

file =
    "day01/input"

run : Task Str Str
run =
    part2 file

part1 : Task Str Str
part1 =
    input <- Common.readAndParse file parse |> Task.await
    processPart1 input |> Task.fromResult

part2 : Str -> Task Str Str
part2 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart2 input |> Task.fromResult

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

processPart2 : Parsed -> Result Str Str
processPart2 = \input ->
    input
    |> List.map List.sum
    |> List.sortDesc
    |> List.takeFirst 3
    |> List.sum
    |> Num.toStr
    |> Ok
