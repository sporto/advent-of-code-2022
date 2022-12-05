interface Day03
    exposes [
        run,
        part1,
        part2,
    ]
    imports [
        Common.{ Parser },
        pf.Task.{ Task },
    ]

run : Task Str Str
run =
    part2 "day03/sample"

# part : Str -> Task Str Str
# part = \file ->
#     if 1 == 1 then
#         part2 file
#     else
#         part1 file

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

part2 : Str -> Task Str Str
part2 = \f ->
    input <- Common.readAndParse f parsePart2 |> Task.await
    processPart2 input |> Task.fromResult

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

assignPriority : Str -> Nat
assignPriority = \chars ->
    chars
    |> Str.toScalars
    |> List.map assignCharPriority
    |> List.sum

assignCharPriority : U32 -> Nat
assignCharPriority = \char ->
    code = if char >= 'a'
        then char - 'a' + 1
        else char - 'A' + 27
    Num.toNat code

# ===

parsePart2 : Common.Parser (List Str)
parsePart2 = \input ->
    Str.split input "\n" |> Ok

processPart2 : List Str -> Result Str Str
processPart2 = \sacks ->
    groups = sacks
        |> Common.chunks 3

    groups
    |> List.map processGroup
    |> List.sum
    |> Num.toStr
    |> Ok

# vJrwpWtwJgWrhcsFMMfFFhFp
# jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
# PmmdzqPrVvPwwTWBwg
# We need to find which char is in all three
# And then count how many of those
processGroup : List Str -> Nat
processGroup = \group ->
    dictAll =
        group
        |> Str.joinWith ""
        |> Str.graphemes
        |> Common.countElements

    sets =
        group
        |> List.map Str.graphemes
        |> List.map Set.fromList

    when sets is
        [ a, b, c ] ->
            Set.intersection a b
            |> Set.intersection c
            |> Set.toList
            |> Str.joinWith ""
            |> assignPriority
        _ -> 0
