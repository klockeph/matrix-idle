module Bars exposing (..)

import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import Random
import Svg.Styled as Svg


kMIN_BAR =
    5


kMAX_BAR =
    100


kMIN_BARS =
    4


kMAX_BARS =
    17


type AlgorithmType
    = BubbleSort
    | InsertionSort
    | QuickSort


type alias Algorithm =
    { algo : AlgorithmType, name : String, unlocked : Bool, active : Bool, price : Int }



-- TODO: fix prices!


kBubbleSort =
    { algo = BubbleSort, name = "BubbleSort", unlocked = False, active = False, price = 10 }


kInsertionSort =
    { algo = InsertionSort, name = "InsertionSort", unlocked = False, active = False, price = 10 }


kQuickSort =
    { algo = QuickSort, name = "QuickSort", unlocked = False, active = False, price = 30 }


kBARS =
    [ 10, 20, 50, 40 ]


kAlgorithms =
    [ kBubbleSort, kInsertionSort, kQuickSort ]


kAlgorithmDict =
    Dict.fromList (List.map (\x -> ( x.algo, x )) kAlgorithms)


barsSorted : Array Int -> Bool
barsSorted a =
    barsSortedL (Array.toList a)


barsSortedL : List Int -> Bool
barsSortedL bars =
    case bars of
        b :: b2 :: bs ->
            b <= b2 && barsSortedL (b2 :: bs)

        _ ->
            True


randomBars : Int -> Random.Generator (List Int)
randomBars len =
    Random.list len (Random.int kMIN_BAR kMAX_BAR)


splitList : Int -> List a -> Result String ( List a, a, List a )
splitList i l =
    let
        firsts : List a
        firsts =
            List.take i l

        lasts : List a
        lasts =
            List.drop i l
    in
    case lasts of
        x :: xs ->
            Ok ( firsts, x, xs )

        _ ->
            Err ("Can't split list at index " ++ String.fromInt i)


swapBarsL : List Int -> Int -> Int -> List Int
swapBarsL bars i1 i2 =
    let
        ( firsts, e1, tmp ) =
            Result.withDefault ( [], 0, [] ) (splitList i1 bars)

        ( mids, e2, lasts ) =
            Result.withDefault ( [], 0, [] ) (splitList (i2 - i1 - 1) tmp)
    in
    firsts ++ [ e2 ] ++ mids ++ [ e1 ] ++ lasts


swapBars : Array Int -> Int -> Int -> Array Int
swapBars bars i1 i2 =
    let
        a1 =
            Maybe.withDefault 0 (Array.get i1 bars)

        a2 =
            Maybe.withDefault 0 (Array.get i2 bars)
    in
    Array.set i1 a2 (Array.set i2 a1 bars)


getBarPos width padding idx =
    String.fromInt (idx * (width + padding) + padding)


getBarHeight : Int -> Int -> Int
getBarHeight max_height bar =
    round ((toFloat max_height / (kMAX_BAR + 1)) * toFloat bar)
