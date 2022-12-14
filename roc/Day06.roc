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
    part1 "../data/day06/input"

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

processPart1 = \input ->
    findMarker input
    |> Num.toStr
    |> Ok

parse : Common.Parser Str
parse = \input ->
    Ok input

findMarker = \input ->
    input
    |> Str.graphemes
    |> doFindMaker []

doFindMaker : List Str, List Str -> Nat
doFindMaker = \remainder, collected ->
    rest = List.drop remainder 4

    when remainder is
    [a, b, c, d, ..] ->
        len = Set.fromList [a, b, c, d] |> Set.len
        if len == 4 then
            List.len collected + 4
        else
            doFindMaker
                (List.concat [b, c, d] rest)
                (List.append collected a)

    _ -> 0


expect findMarker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == 7
expect findMarker "bvwbjplbgvbhsrlpgdmjqwftvncz" == 5
expect findMarker "nppdvjthqldpwncqszvftbrmjlhg" == 6
expect findMarker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 10
expect findMarker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 11