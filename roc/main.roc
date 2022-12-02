app "advent"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        Day02,
    ]
    provides [main] to pf

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
    Day02.run

