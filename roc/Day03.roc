interface Day03
    exposes [
        run,
    ]
    imports [
        Common.{ Parser },
        pf.Task.{ Task },
    ]

sample =
    "day03/sample"

file =
    "day03/input"

run : Task Str Str
run =
    part1 file

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

Parsed : List Rucksack

Rucksack : {
    left : List Str,
    right : List Str,
}

parse : Common.Parser Parsed
parse = \input ->
    Str.split input "\n"
    |> List.mapTry parseRow

parseRow : Str -> Result Rucksack Str
parseRow = \input ->
    chars = Str.graphemes input
    half = (List.len chars |> Num.toFrac) / 2 |> Num.round
    left = List.takeFirst chars half
    right = List.takeLast chars half

    Ok {
        left: left,
        right: right,
    }

processPart1 : Parsed -> Result Str Str
processPart1 = \sacks ->
    sacks
    |> List.map findCommonChars
    |> List.map assignPriority
    |> List.sum
    |> Num.toStr
    |> Ok

findCommonChars : Rucksack -> Str
findCommonChars = \rucksack ->
    Set.intersection
        (Set.fromList rucksack.left)
        (Set.fromList rucksack.right)
    |> Set.toList
    |> Str.joinWith ""

assignPriority : Str -> U64
assignPriority = \chars ->
    chars
    |> Str.toScalars
    |> List.map assignCharPriority
    |> List.sum
    

assignCharPriority : U32 -> U64
assignCharPriority = \char ->
    code = if char >= 'a'
        then char - 'a' + 1
        else char - 'A' + 27
    Num.toU64 code

