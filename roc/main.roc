app "advent"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
    ]
    provides [main] to pf

# https://www.roc-lang.org/builtins/Result
# https://www.roc-lang.org/packages/basic-cli/Task

main : Task {} []
main =
    result <- Task.attempt start
    when result is
        Err e ->
            {} <- Stderr.line e |> Task.await
            Process.exit 1
        Ok resp ->
            {} <- Stdout.line resp |> Task.await
            Process.exit 0


start : Task Str Str
start =
    input <- readAndParse "day01/input" day01Parse |> Task.await
    processDay1 input |> Task.fromResult


Parser parsed : Str -> Result parsed Str


readAndParse : Str, Parser parsed -> Task parsed Str
readAndParse = \filePath, parse ->
    content <- readFile filePath |> Task.await
    parse content |> Task.fromResult


readFile : Str -> Task Str Str
readFile = \filePath ->
    task =
        File.readUtf8 (Path.fromStr filePath)
    Task.attempt task \result ->
        when result is
            Err _ -> Task.fail "Could not open \(filePath)"
            Ok content -> Task.succeed content

# Day 01
day01Parse : Parser (List (List I32))
day01Parse = \input ->
    Str.split input "\n\n"
    |> List.mapTry day01ParseBlock


day01ParseBlock : Parser (List I32)
day01ParseBlock = \input ->
    Str.split input "\n"
    |> List.mapTry Str.toI32
    |> Result.mapErr (\_ -> "Could not parse \(input)")


processDay1 : (List (List I32)) -> Result Str Str
processDay1 = \input ->
    input
    |> List.map List.sum
    |> List.max
    |> Result.mapErr (\_ -> "Empty")
    |> Result.map Num.toStr
