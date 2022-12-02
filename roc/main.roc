app "advent"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        Day01,
        Common.{ Parser },
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
    input <- readAndParse "day01/input" Day01.parse |> Task.await
    Day01.process input |> Task.fromResult


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
