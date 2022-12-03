interface Common
    exposes [
        Parser,
        readAndParse,
    ]
    imports [
        pf.Task.{ Task },
        pf.File,
        pf.Path,
    ]

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
