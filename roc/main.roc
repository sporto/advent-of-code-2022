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
    input <- readAndParse "day01/sample" day01Parse |> Task.await
    result = processDay1 input
    Task.succeed result


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


# parseInput : Parser (List (List I16))
# parseInput = \content ->
#     day01Parse content


day01Parse : Parser (List (List I16))
day01Parse = \input ->
    Str.split input "\n\n"
    |> List.mapTry day01ParseBlock


day01ParseBlock : Parser (List I16)
day01ParseBlock = \input ->
    Str.split input "\n"
    |> List.mapTry Str.toI16
    |> Result.mapErr (\_ -> "Could not parse \(input)")


processDay1 : (List (List I16)) -> Str
processDay1 = \_ ->
    "Hello"
