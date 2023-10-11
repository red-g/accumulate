module Fold exposing
    ( Fold
    , wrap, custom, merge
    , list, string
    , arrayLeft, arrayRight, listLeft, listRight, maybe, result, resultError, stringLeft, stringRight
    , failed, passed
    )

{-| Simple, extensible `Fold`s.


# Basics

@docs Fold


# Transformations

@docs wrap, custom, merge


# Primitive Folds

@docs list, string


# Composite Folds

@docs arrayLeft, arrayRight, listLeft, listRight, maybe, result, resultError, stringLeft, stringRight

-}

import Array exposing (Array)
import Filter exposing (Filter)


{-| Uses an input `a` to accumulate over `b`.
-}
type Fold a b
    = Fold (Internals a b)


type alias Internals a b =
    b -> a -> b


{-| Apply a transformation to input of a `Fold`.
-}
wrap : (c -> a) -> Fold a b -> Fold c b
wrap unwrapper folder =
    custom <| wrap_ unwrapper (merge folder)


wrap_ : (c -> a) -> Internals a b -> Internals c b
wrap_ unwrapper folder acc value =
    folder acc (unwrapper value)


{-| If the input is `Just`, fold over it.

merge (maybe list) [ 2, 3 ] (Just 1)
-- [ 1, 2, 3, 4 ]

merge (maybe string) "hello" Nothing
-- "hello"

-}
maybe : Fold a b -> Fold (Maybe a) b
maybe folder =
    custom <| maybe_ (merge folder)


maybe_ : Internals a b -> Internals (Maybe a) b
maybe_ folder acc maybeValue =
    case maybeValue of
        Just value ->
            folder acc value

        Nothing ->
            acc


{-| Evaluate your fold for a given accumulator and input.
-}
merge : Fold a b -> b -> a -> b
merge (Fold folder) acc value =
    folder acc value


{-| Construct a `Fold` that uses `a` to accumulate over `b`.
-}
custom : (b -> a -> b) -> Fold a b
custom =
    Fold


{-| Fold `a` onto `b` if `Result e a` is `Ok`.
-}
result : Fold a b -> Fold (Result e a) b
result folder =
    custom <| result_ (merge folder)


result_ : Internals a b -> Internals (Result e a) b
result_ folder acc resultValue =
    case resultValue of
        Ok value ->
            folder acc value

        Err _ ->
            acc


{-| Fold 'e' onto 'b' if `Result e a` is `Err`.
-}
resultError : Fold e b -> Fold (Result e b) b
resultError folder =
    custom <| resultError_ (merge folder)


resultError_ : Internals e b -> Internals (Result e a) b
resultError_ folder acc resultValue =
    case resultValue of
        Ok _ ->
            acc

        Err error ->
            folder acc error


{-| Apply a fold only if the filter passes.
-}
passed : Filter a -> Fold a b -> Fold a b
passed filter fold =
    custom <| passed_ filter <| merge fold


passed_ : Filter a -> Internals a b -> Internals a b
passed_ filter folder acc value =
    case Filter.test filter value of
        Filter.Pass ->
            folder acc value

        Filter.Fail ->
            acc


{-| Apply a fold only if the filter fails.
-}
failed : Filter a -> Fold a b -> Fold a b
failed filter fold =
    custom <| failed_ filter <| merge fold


failed_ : Filter a -> Internals a b -> Internals a b
failed_ filter folder acc value =
    case Filter.test filter value of
        Filter.Pass ->
            folder acc value

        Filter.Fail ->
            acc


{-| Starting from the left, fold each element of the list onto `b`.
-}
listLeft : Fold a b -> Fold (List a) b
listLeft folder =
    custom <| listLeft_ (merge folder)


listLeft_ : Internals a b -> Internals (List a) b
listLeft_ folder =
    List.foldl (reorder folder)


{-| Starting from the right, fold each element of the list onto 'b'.
-}
listRight : Fold a b -> Fold (List a) b
listRight folder =
    custom <| listRight_ (merge folder)


listRight_ : Internals a b -> Internals (List a) b
listRight_ folder =
    List.foldr (reorder folder)


{-| Starting from the left, fold each element of the array onto 'b'.
-}
arrayLeft : Fold a b -> Fold (Array a) b
arrayLeft folder =
    custom <| arrayLeft_ (merge folder)


arrayLeft_ : Internals a b -> Internals (Array a) b
arrayLeft_ folder =
    Array.foldl (reorder folder)


{-| Starting from the right, fold each element of the array onto `b`.
-}
arrayRight : Fold a b -> Fold (Array a) b
arrayRight folder =
    custom <| arrayRight_ (merge folder)


arrayRight_ : Internals a b -> Internals (Array a) b
arrayRight_ folder =
    Array.foldr (reorder folder)


stringLeft_ : Internals Char b -> Internals String b
stringLeft_ folder =
    String.foldl (reorder folder)


{-| Starting from the left, fold each character in the string onto `b`.
-}
stringLeft : Fold Char b -> Fold String b
stringLeft folder =
    custom <| stringLeft_ (merge folder)


stringRight_ : Internals Char b -> Internals String b
stringRight_ folder =
    String.foldr (reorder folder)


{-| Starting from the right, fold each character in the string onto `b`.
-}
stringRight : Fold Char b -> Fold String b
stringRight folder =
    custom <| stringRight_ (merge folder)


reorder : Internals a b -> a -> b -> b
reorder folder value acc =
    folder acc value


{-| Cons a character onto the string.
-}
string : Fold Char String
string =
    custom string_


string_ : Internals Char String
string_ s c =
    String.cons c s


{-| Cons an element onto the list.
-}
list : Fold a (List a)
list =
    custom list_


list_ : Internals a (List a)
list_ l i =
    i :: l
