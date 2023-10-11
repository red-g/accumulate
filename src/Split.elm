module Split exposing
    ( Splitter
    , custom, take
    , arrayEnd, arrayStart, listHead, stringHead
    )

{-| Extract elements from collections.


# Splitters

@docs Splitter


# Manipulations

@docs custom, take


# Primitives

@docs arrayEnd, arrayStart, listHead, stringHead

-}

import Array exposing (Array)


{-| Tries to split an element `a` off of `b`.
-}
type Splitter a b
    = Splitter (b -> Maybe ( a, b ))


{-| Try to split an item off a collection, applying the given function on success.
-}
take : (a -> b -> c) -> Splitter a b -> b -> Maybe c
take func (Splitter splitter) collection =
    Maybe.map (\( h, r ) -> func h r) <| splitter collection


{-| Create a splitter that tries to take an element `a` off of `b`.
-}
custom : (b -> Maybe ( a, b )) -> Splitter a b
custom =
    Splitter


{-| Split off the head of a list.
-}
listHead : Splitter a (List a)
listHead =
    custom listHead_


{-| Split off the start of an array.
-}
arrayStart : Splitter a (Array a)
arrayStart =
    custom arrayStart_


arrayStart_ : Array a -> Maybe ( a, Array a )
arrayStart_ a =
    Maybe.map (\t -> ( t, Array.slice 0 -1 a )) <| Array.get 0 a


{-| Split off the end of an array.
-}
arrayEnd : Splitter a (Array a)
arrayEnd =
    custom arrayEnd_


arrayEnd_ : Array a -> Maybe ( a, Array a )
arrayEnd_ a =
    Maybe.map (\h -> ( h, Array.slice 1 (Array.length a) a )) <| Array.get 0 a


{-| Split off the head of a string.
-}
stringHead : Splitter Char String
stringHead =
    custom String.uncons


listHead_ : List a -> Maybe ( a, List a )
listHead_ l =
    case l of
        h :: r ->
            Just ( h, r )

        _ ->
            Nothing
