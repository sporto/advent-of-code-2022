interface Day04
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
    part1 "day04/input"

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

Ranges : {
    left : Range,
    right : Range,
}

Range : {
    start : U8,
    end : U8,
}

Parsed : List Ranges

parse : Common.Parser Parsed
parse = \input ->
    Str.split input "\n"
    |> List.mapTry parseRow

parseRow : Str -> Result Ranges Str
parseRow = \input ->
    Str.split input ","
    |> List.mapTry parseRange
    |> Result.try
        (\list ->
            when list is
                [a, b] ->
                    Ok { left: a, right: b }

                _ ->
                    Err "Invalid row"
        )

parseRange : Str -> Result Range Str
parseRange = \input ->
    Str.split input "-"
    |> List.mapTry parseNum
    |> Result.try
        (\list ->
            when list is
                [a, b] -> Ok { start: a, end: b }
                _ -> Err "Invalid"
        )

parseNum : Str -> Result U8 Str
parseNum = \input ->
    Str.toU8 input |> Result.mapErr (\_ -> "Invalid \(input)")

processPart1 : Parsed -> Result Str Str
processPart1 = \pairs ->
    contains = pairs
        |> List.map fullyContains

    # dbg contains

    contains
    |> List.countIf (\b -> b)
    |> Num.toStr
    |> Ok

# rangesHasOverlap : Ranges -> Bool
# rangesHasOverlap = \ranges ->
#     int = Set.intersection
#             (expandRange ranges.left |> Set.fromList)
#             (expandRange ranges.right |> Set.fromList)
#         |> Set.len

#     int > 0

fullyContains : Ranges -> Bool
fullyContains = \ranges ->
    leftList = expandRange ranges.left
    # dbg leftList
    rightList = expandRange ranges.right
    # dbg rightList
    leftSet = leftList |> Set.fromList
    rightSet = rightList |> Set.fromList
    intersection = Set.intersection leftSet rightSet |> Set.toList
    # dbg intersection
    intersection == leftList || intersection == rightList


expandRange : Range -> List U8
expandRange = \range ->
    List.range range.start (range.end + 1)