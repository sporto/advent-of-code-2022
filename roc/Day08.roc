interface Day08
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
    part1 "../data/day08/sample"

part1 : Str -> Task Str Str
part1 = \f ->
    input <- Common.readAndParse f parse |> Task.await
    processPart1 input |> Task.fromResult

Coor : {x: Nat, y: Nat}

Matrix : Dict Coor Nat

parse : Str -> Result Matrix Str
parse = \input ->
    input
    |> Str.split "\n"
    |> List.mapTry parseRow
    |> Result.map toMatrix

parseRow = \input ->
    input
    |> Str.graphemes
    |> List.mapTry parseNum

parseNum : Str -> Result Nat Str
parseNum = \input ->
    Str.toNat input |> Result.mapErr (\_ -> "Invalid \(input)")

toMatrix : List (List Nat) -> Matrix
toMatrix = \list ->
    Common.walkWithIndex list Dict.empty (\acc, row, rowIx ->
        Common.walkWithIndex row acc (\innerAcc, cell, cellIx ->
            Dict.insert innerAcc { x: cellIx, y: rowIx } cell
        )
    )

MatrixWithVis : Dict Coor { height: Nat, visible: d}

processPart1 = \matrix ->
    Dict.walk matrix Dict.empty (\state, k, v ->
        state
    )
    |> Dict.values
    |> List.sum
    |> Num.toStr
    |> Ok

# part1WalkRow : Matrix -> Matrix, List Nat, Nat -> Matrix
# part1WalkRow = \matrix ->
#     \acc, row, ix ->
#         Common.walkWithIndex row [] (part1WalkCell matrix)

# part1WalkCell = \matrix ->
#     \acc, cell, ix ->
#         []