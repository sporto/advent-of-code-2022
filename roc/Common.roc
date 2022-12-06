interface Common
    exposes [
        Parser,
        boolToNum,
        chunks,
        countElements,
        readAndParse,
        replace,
        walkWithIndex,
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

boolToNum : Bool -> Nat
boolToNum = \bool ->
    if bool then
        1
    else
        0

# https://github.com/elm-community/list-extra/blob/5a083cf0400260537adef75f96fbd48bfcedc7c0/src/List/Extra.elm#L2089
chunks : List a, Nat -> List (List a)
chunks = \list, chunkSize ->
    doChunks list chunkSize []

doChunks : List a, Nat, List (List a) -> List (List a)
doChunks = \remainder, chunkSize, acc ->
    when List.takeFirst remainder chunkSize is
        [] -> acc
        taken ->
            doChunks
                (List.drop remainder chunkSize)
                chunkSize
                (List.append acc taken)

countElements : List a -> Dict a Nat | a has Eq
countElements = \list ->
    insert : Dict a Nat, a -> Dict a Nat
    insert = \acc, ele ->
        Dict.update
            acc
            ele
            (\possibleValue ->
                when possibleValue is
                    Missing -> Present 1
                    Present value -> Present (value + 1)
            )

    List.walk list Dict.empty insert

# expected =
#     Dict.empty |> Dict.insert "a" 2 |> Dict.insert "b" 1
# expect countElements (Str.graphemes "aba") == expected
expect countElements (Str.graphemes "aba") == (Dict.empty |> Dict.insert "a" 2 |> Dict.insert "b" 1)
# expect countElements "aba" == (Dict.empty |> Dict.insert "a" 2 |> Dict.insert "b" 1)

replace : Str, Str, Str -> Str
replace = \word, target, replacement ->
    when Str.replaceEach word target replacement is
        Ok replaced -> replaced
        Err _ -> word

walkWithIndex : List elem, state, (state, elem, Nat -> state) -> state
walkWithIndex = \list, state, fn ->
    List.mapWithIndex list (\e, ix ->
        {element: e, index: ix}
    )
    |> List.walk state (\acc, pair ->
        fn acc pair.element pair.index
    )