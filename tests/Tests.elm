module Tests exposing (suite)

import Dict exposing (Dict)
import Expect
import Filter exposing (Filter)
import Fold exposing (Fold)
import Fuzz exposing (Fuzzer)
import Reduce exposing (Reduce)
import Test exposing (Test, fuzz, test)


suite : Test
suite =
    Test.concat [ mergeValuesTest, cattingFiltersTest ]


andReduce : Reduce (Filter a)
andReduce =
    Reduce.custom Filter.and


orReduce : Reduce (Filter a)
orReduce =
    Reduce.custom Filter.or


maybeOr : Reduce (Maybe (Filter a))
maybeOr =
    Reduce.maybe orReduce


maybeAnd : Reduce (Maybe (Filter a))
maybeAnd =
    Reduce.maybe andReduce


cattingFiltersTest : Test
cattingFiltersTest =
    Test.describe "Testing Filter Concatenation"
        [ test "Two Just Filters" <| \_ -> Expect.equal (Just Filter.Fail) <| Maybe.map (\f -> Filter.test f ()) (Reduce.combine maybeAnd (Just Filter.pass) (Just Filter.fail))
        , test "One Just Filter" <| \_ -> Expect.equal (Just Filter.Pass) <| Maybe.map (\f -> Filter.test f ()) (Reduce.combine maybeAnd (Just Filter.pass) Nothing)
        , test "Zero Just Filters" <| \_ -> Expect.equal Nothing (Reduce.combine maybeAnd Nothing Nothing)
        ]


intervalFuzzer : Int -> Fuzzer { lo : Int, hi : Int }
intervalFuzzer variability =
    Fuzz.map2 (\lo dif -> { lo = lo, hi = lo + dif }) (Fuzz.intRange 1 variability) (Fuzz.intRange 1 variability)


mergeValuesTest : Test
mergeValuesTest =
    fuzz (intervalFuzzer 50) "Merges values that are in both dicts" <|
        \{ lo, hi } ->
            let
                delta =
                    hi - lo

                t =
                    delta // 3

                leftKeys =
                    List.range lo (lo + 2 * t)

                rightKeys =
                    List.range (lo + t) hi

                values =
                    List.range 0 hi

                left =
                    Dict.fromList <| List.map2 Tuple.pair leftKeys values

                right =
                    Dict.fromList <| List.map2 Tuple.pair rightKeys values

                expected =
                    List.map2 (+) (List.range t (2 * t)) (List.range 0 t)

                keys =
                    List.range lo hi
            in
            mergeValues (+) left right keys
                |> Expect.equal expected


getMerge : (a -> b -> c) -> Dict comparable a -> Dict comparable b -> comparable -> Maybe c
getMerge merger left right key =
    case ( Dict.get key left, Dict.get key right ) of
        ( Just l, Just r ) ->
            Just <| merger l r

        _ ->
            Nothing


valueMerger : (a -> b -> c) -> Dict comparable a -> Dict comparable b -> Fold (List comparable) (List c)
valueMerger merger left right =
    Fold.listRight <| Fold.wrap (getMerge merger left right) <| Fold.maybe Fold.list


mergeValues : (a -> b -> c) -> Dict comparable a -> Dict comparable b -> List comparable -> List c
mergeValues merger left right keys =
    Fold.merge (valueMerger merger left right) [] keys
