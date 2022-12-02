interface Day01
    exposes [
        parse,
        process,
    ]
    imports []

Parser parsed : Str -> Result parsed Str

parse : Parser (List (List I32))
parse = \input ->
    Str.split input "\n\n"
    |> List.mapTry day01ParseBlock


day01ParseBlock : Parser (List I32)
day01ParseBlock = \input ->
    Str.split input "\n"
    |> List.mapTry Str.toI32
    |> Result.mapErr (\_ -> "Could not parse \(input)")


process : (List (List I32)) -> Result Str Str
process = \input ->
    input
    |> List.map List.sum
    |> List.max
    |> Result.mapErr (\_ -> "Empty")
    |> Result.map Num.toStr
