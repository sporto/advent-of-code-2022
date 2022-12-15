interface Day07
    exposes [
        run,
        part1,
        # part2,
    ]
    imports [
        Common.{ Parser },
        pf.Task.{ Task },
    ]

run : Task Str Str
run =
    part1 "../data/day07/sample"

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

parse : Str -> Result (List Block) Str
parse = \input ->
    input
    |> Str.split "$ "
    |> List.drop 1
    |> List.mapTry parseBlock

Block : {cmd : Command, out: Str}

Command :
    [
    ChangeDir Str,
    ListDir,
    ]

cmdToStr : Command -> Str
cmdToStr = \cmd ->
    when cmd is
        ChangeDir arg ->
            "cd \(arg)"
        ListDir ->
            "ls"

parseBlock : Str -> Result Block Str
parseBlock = \input ->
    lines = input
        |> Str.split "\n"

    out = lines |> List.drop 1 |> Str.joinWith "\n"
    cmdStr <- lines |> List.first |> Result.mapErr (\_ -> "Empty") |> Result.try
    cmd <- parseCommand cmdStr |> Result.try

    Ok { cmd : cmd, out: out}

parseCommand : Str -> Result Command Str
parseCommand = \input ->
    if Str.startsWith input "ls" then
        Ok ListDir
    else if Str.startsWith input "cd" then
        Ok (ChangeDir (Common.replace input "cd " ""))
    else
        Err input


processPart1 : List Block -> Result Str Str
processPart1 = \input ->
    input
    |> List.map (\block -> block.cmd |> cmdToStr)
    |> Str.joinWith "\n"
    |> Ok