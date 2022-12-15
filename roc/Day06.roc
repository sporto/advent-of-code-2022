interface Day06
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
    part2 "../data/day06/input"

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

part2 : Str -> Task Str Str
part2 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart2 input |> Task.fromResult

processPart1 = \input ->
    findMarker 4 input
    |> Num.toStr
    |> Ok

processPart2 = \input ->
    findMarker 14 input
    |> Num.toStr
    |> Ok

parse : Common.Parser Str
parse = \input ->
    Ok input

findMarker = \size, input ->
    input
    |> Str.graphemes
    |> doFindMaker [] size

doFindMaker : List Str, List Str, Nat -> Nat
doFindMaker = \remainder, collected, size ->
    rest = List.drop remainder size
    head = List.takeFirst remainder size
    len = Set.fromList head |> Set.len

    if len == size then
        List.len collected + size
    else
        when head is
            [a, ..] ->
                restOfHead = List.drop head 1
                doFindMaker
                    (List.concat restOfHead rest)
                    (List.append collected a)
                    size
            _ ->
                0


#findMessageMarker


expect findMarker 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == 7
expect findMarker 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" == 5
expect findMarker 4 "nppdvjthqldpwncqszvftbrmjlhg" == 6
expect findMarker 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 10
expect findMarker 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 11

expect
    actual = findMarker 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    actual == 19

expect findMarker 14 "bvwbjplbgvbhsrlpgdmjqwftvncz" == 23
expect findMarker 14 "nppdvjthqldpwncqszvftbrmjlhg" == 23
expect findMarker 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 29
expect findMarker 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 26