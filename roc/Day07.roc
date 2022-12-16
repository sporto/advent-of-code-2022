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

parse : Str -> Result (List Command) Str
parse = \input ->
    input
    |> Str.split "$ "
    |> List.drop 1
    |> List.mapTry parseCommandBlock

Command :
    [
    ChangeDir Str,
    ListDir (List LsLine),
    ]

LsLine :
    [
    File Nat Str,
    Dir Str,
    ]

lsLineToStr : LsLine -> Str
lsLineToStr = \lsLine ->
    when lsLine is
        File size name ->
            sizeStr = Num.toStr size
            "file \(sizeStr) \(name)"
        Dir name ->
            "dir \(name)"

commandToStr : Command -> Str
commandToStr = \cmd ->
    when cmd is
        ChangeDir arg ->
            "cd \(arg)"
        ListDir out ->
            outLines = lsOutToStr out
            "ls \n \(outLines)"

lsOutToStr = \out ->
    out
    |> List.map lsLineToStr
    |> Str.joinWith "\n"

parseCommandBlock : Str -> Result Command Str
parseCommandBlock = \input ->
    lines = input
        |> Str.split "\n"

    cmdStr <- lines |> List.first |> Result.mapErr (\_ -> "Empty") |> Result.try

    if Str.startsWith cmdStr "ls" then
        output <- lines |> List.drop 1 |> parseLsOutput |> Result.try
        Ok (ListDir output)
    else if Str.startsWith cmdStr "cd" then
        Ok (ChangeDir (Common.replace cmdStr "cd " ""))
    else
        Err "Invalid \(input)"

parseLsOutput : List Str -> Result (List LsLine) Str
parseLsOutput = \input ->
    input
    |> List.keepIf (\line -> !(Str.isEmpty line))
    |> List.mapTry parseLsLine

parseLsLine : Str -> Result LsLine Str
parseLsLine = \input ->
    if Str.isEmpty input then
        Err "Input is empty"
    else if Str.startsWith input "dir" then
        Ok (Dir (Common.replace input "dir " ""))
    else
        when Str.split input " " is
        [sizeStr, name] ->
            size <- Str.toNat sizeStr |> Result.mapErr (\_ -> "NaN") |> Result.try
            Ok (File size name)
        _ -> Err "Invalid LsLine \(input)"

processPart1 : List Command -> Result Str Str
processPart1 = \input ->
    input
    |> List.walk {fs: newFileSystem, currentPath: []} fillFileSystem
    |> .fs
    |> printFileSystem
    # |> List.map commandToStr
    # |> Str.joinWith "\n"
    |> Ok

# Key is the path
FileSystem : Dict (List Str) (List LsLine) # (List {size :Nat , name :Str})

newFileSystem : FileSystem
newFileSystem =
    Dict.single [] []

fillFileSystem : { fs: FileSystem, currentPath: List Str }, Command -> { fs: FileSystem, currentPath: List Str }
fillFileSystem = \ { fs: fileSystem,  currentPath: currentPath }, command ->
    when command is
        ChangeDir dir ->
            when dir is
                "/" ->
                    {
                        fs: fileSystem,
                        currentPath: []
                    }
                ".." ->
                    {
                        fs: fileSystem,
                        currentPath: List.dropFirst currentPath
                    }
                _ ->
                    {
                        fs: fileSystem,
                        currentPath: List.append currentPath dir
                    }
        ListDir list ->
            {
                fs: fillDirectory fileSystem currentPath list,
                currentPath: currentPath
            }

fillDirectory : FileSystem, List Str, List LsLine -> FileSystem
fillDirectory = \fileSystem, currentPath, list ->
    Dict.insert fileSystem currentPath list
    # List.walk list fileSystem (\fs, entry ->
        # Dict.set fs currentPath en
        # Dict.update fs currentPath (\possibleValue ->
            # when possibleValue is
            #     Missing -> Present list
            #     Present existing -> Present (List.concat )
        # )
    # )

printFileSystem : FileSystem -> Str
printFileSystem = \fileSystem ->
    printDirectory [] fileSystem

printDirectory : List Str, FileSystem -> Str
printDirectory = \path, fileSystem ->
    level = List.len path
    indentation = makeIndent level
    when Dict.get fileSystem path is
        Ok content ->
            fullPath = Str.joinWith path "/"
            files = printDirectoryFiles (level + 1) content
            subDirs = printDirectorySubDirs path fileSystem content
            "\(indentation)- /\(fullPath) (dir)\n\(files)\n\(subDirs)"
        Err _ -> ""

makeIndent : Nat -> Str
makeIndent = \level ->
    List.repeat "    " level
        |> Str.joinWith ""

printDirectoryFiles : Nat, List LsLine -> Str
printDirectoryFiles = \level, content ->
    indentation = makeIndent level
    content
        |> List.keepOks (\entry ->
            when entry is
                File size name ->
                    Ok { size: size, name: name }
                _ ->
                    Err "Not a file"
        )
        |> List.map
            (\{size, name} ->
                Str.concat
                    (makeIndent level)
                    "- \(name) (file)"
            )
            |> Str.joinWith ("\n")

printDirectorySubDirs = \containerPath, fileSystem, content ->
    content
        |> List.keepOks (\entry ->
            when entry is
                Dir name ->
                    Ok name
                _ ->
                    Err "Not a dir"
            )
        |> List.map (\subDirName ->
            subDirPath = (List.append containerPath subDirName)
            printDirectory subDirPath fileSystem
        ) |> Str.joinWith "\n"