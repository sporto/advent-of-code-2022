app "advent"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [pf.Stdout, pf.Stdin, pf.Task.{ await }]
    provides [main] to pf

main =
    _ <- await (Stdout.line "Type something press Enter:")
    text <- await Stdin.line

    Stdout.line "You just entered: \(text)"

read = \filepath ->
    File.readUtf8 (Path.fromStr filepath)