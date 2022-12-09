app "advent"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.2/3bKbbmgtIfOyC6FviJ9o8F8xqKutmXgjCJx3bMfVTSo.tar.br" }
    imports [
        pf.Process,
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        Day05,
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
    Day05.run