module Reduce exposing
    ( Reduce
    , custom, combine, flip, collect, wrap
    , fAdd, fDiv, fMul, fPow, fSub, iAdd, iDiv, iMul, iPow, iSub, array, list, max, min, string, or, and
    , arrayLeft, arrayRight, stringLeft, listLeft, maybe, result
    )

{-| Simple, extensible reducers.


# Reducers

@docs Reduce


# Manipulations

@docs custom, combine, flip, collect, wrap


# Primitive Reducers

@docs fAdd, fDiv, fMul, fPow, fSub, iAdd, iDiv, iMul, iPow, iSub, array, list, max, min, string, or, and


# Composite Reducers

@docs arrayLeft, arrayRight, stringLeft, listLeft, maybe, result

-}

import Array exposing (Array)
import Filter exposing (Filter)
import Fold exposing (Fold)
import Sort exposing (Sorter)
import Split exposing (Splitter)


{-| Combines 2 of the same thing. Equivalent to a `Fold` where the accumulator and input share the same type.
-}
type alias Reduce a =
    Fold a a


type alias Internals a =
    a -> a -> a


{-| Reduce over a collection. The initial state is split off of the collection.
The accumulator is the reducer's left input.
-}
collect : Splitter b a -> (Reduce b -> Fold a b) -> Reduce b -> a -> Maybe b
collect splitter folder reducer acc =
    Split.take (Fold.merge <| folder reducer) splitter acc


{-| Combine two values with a reducer.
-}
combine : Reduce a -> a -> a -> a
combine =
    Fold.merge


{-| Flip the left and right inputs to a reducer.
-}
flip : Reduce a -> Reduce a
flip reducer =
    custom <| flip_ <| combine reducer


flip_ : Internals a -> Internals a
flip_ reducer leftv rightv =
    reducer rightv leftv



-- START REVISIONS


{-| Apply an operation to the right input of a reducer.
-}
wrap : (a -> a) -> Reduce a -> Reduce a
wrap =
    Fold.wrap


{-| Apply an operation to the left input of a reducer
-}
left : (a -> a) -> Reduce a -> Reduce a
left mapper reducer =
    custom <| left_ mapper (combine reducer)


left_ : (a -> a) -> Internals a -> Internals a
left_ mapper reducer leftv rightv =
    reducer (mapper leftv) rightv


{-| Apply an operation to the right input of a reducer
-}
right : (a -> a) -> Reduce a -> Reduce a
right mapper reducer =
    custom <| right_ mapper (combine reducer)


right_ : (a -> a) -> Internals a -> Internals a
right_ mapper reducer leftv rightv =
    reducer leftv (mapper rightv)


{-| Apply an operation to the result of a reducer.
-}
last : (a -> a) -> Reduce a -> Reduce a
last mapper reducer =
    custom <| \leftv rightv -> mapper <| combine reducer leftv rightv



-- END REVISIONS


{-| Construct a reducer from the given function, where the first and second arguments are the left and right inputs respectively.
-}
custom : (a -> a -> a) -> Reduce a
custom =
    Fold.custom


{-| Pass the filter if one filter passes.
-}
or : Reduce (Filter a)
or =
    custom Filter.or


{-| Pass the filter if both filters pass.
-}
and : Reduce (Filter a)
and =
    custom Filter.and


{-| Put the left integer to the power of the right integer.
-}
iPow : Reduce Int
iPow =
    custom (^)


{-| Put the left float to the power of the right float.
-}
fPow : Reduce Float
fPow =
    custom (^)


{-| Divide the left integer by the right integer.
-}
iDiv : Reduce Int
iDiv =
    custom (//)


{-| Divide the left float by the right float.
-}
fDiv : Reduce Float
fDiv =
    custom (/)


{-| Multiply the right integer by the left integer.
-}
iMul : Reduce Int
iMul =
    custom (*)


{-| Multiply the right float by the left float.
-}
fMul : Reduce Int
fMul =
    custom (*)


{-| Add the right int to the left int.
-}
iAdd : Reduce Int
iAdd =
    custom (+)


{-| Subtract the right int from the left int.
-}
iSub : Reduce Int
iSub =
    custom (-)


{-| Add the right float to the left float.
-}
fAdd : Reduce Float
fAdd =
    custom (+)


{-| Subtract the right float from the left float.
-}
fSub : Reduce Float
fSub =
    custom (-)


{-| Append the right list to the left list.
-}
list : Reduce (List a)
list =
    custom (++)


{-| Append the right array to the left array.
-}
array : Reduce (Array a)
array =
    custom Array.append


{-| Append the right string to the left string.
-}
string : Reduce String
string =
    custom (++)


{-| Keep all `Just` values, reducing if both are present.
-}
maybe : Reduce a -> Reduce (Maybe a)
maybe reducer =
    custom <| maybe_ (combine reducer)


maybe_ : Internals a -> Internals (Maybe a)
maybe_ reducer maybeLeft maybeRight =
    case ( maybeLeft, maybeRight ) of
        ( Just leftv, Just rightv ) ->
            Just <| reducer leftv rightv

        ( Just leftv, Nothing ) ->
            Just leftv

        ( Nothing, Just rightv ) ->
            Just rightv

        ( Nothing, Nothing ) ->
            Nothing


result_ : Internals e -> Internals a -> Internals (Result e a)
result_ err ok resultLeft resultRight =
    case ( resultLeft, resultRight ) of
        ( Ok leftv, Ok rightv ) ->
            Ok <| ok leftv rightv

        ( Ok leftv, Err _ ) ->
            Ok leftv

        ( Err _, Ok rightv ) ->
            Ok rightv

        ( Err leftv, Err rightv ) ->
            Err (err leftv rightv)


resultError_ : Internals e -> Internals a -> Internals (Result e a)
resultError_ err ok resultLeft resultRight =
    case ( resultLeft, resultRight ) of
        ( Ok leftv, Ok rightv ) ->
            Ok <| ok leftv rightv

        ( Ok _, Err rightv ) ->
            Err rightv

        ( Err leftv, Ok _ ) ->
            Err leftv

        ( Err leftv, Err rightv ) ->
            Err (err leftv rightv)


{-| Keep all `Ok` values, reducing if both are present.
-}
result : Reduce e -> Reduce a -> Reduce (Result e a)
result err ok =
    custom <| result_ (combine err) (combine ok)


{-| Keep all `Err` values, reducing if both are present.
-}
resultError : Reduce e -> Reduce a -> Reduce (Result e a)
resultError err ok =
    custom <| resultError_ (combine err) (combine ok)


{-| Keep the larger input, according to the given sorter.
-}
max : Sorter k -> Reduce k
max sorter =
    custom <| max_ sorter


max_ : Sorter k -> Internals k
max_ sorter leftv rightv =
    case Sort.order sorter leftv rightv of
        LT ->
            rightv

        _ ->
            leftv


{-| Keep the smaller input, according to the given sorter.
-}
min : Sorter k -> Reduce k
min sorter =
    custom <| min_ sorter


min_ : Sorter k -> Internals k
min_ sorter leftv rightv =
    case Sort.order sorter leftv rightv of
        GT ->
            rightv

        _ ->
            leftv


{-| Reduce a string from the left.
-}
stringLeft : Reduce Char -> String -> Maybe Char
stringLeft =
    collect Split.stringHead Fold.stringLeft


{-| Reduce a list from the left.
-}
listLeft : Reduce a -> List a -> Maybe a
listLeft =
    collect Split.listHead Fold.listLeft


{-| Reduce an array from the left.
-}
arrayLeft : Reduce a -> Array a -> Maybe a
arrayLeft =
    collect Split.arrayStart Fold.arrayLeft


{-| Reduce an array from the right.
-}
arrayRight : Reduce a -> Array a -> Maybe a
arrayRight =
    collect Split.arrayEnd Fold.arrayRight
