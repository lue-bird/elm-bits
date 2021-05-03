module Util exposing
    ( last
    , removeLast
    )

{-| Functions that should not be exposed but are needed in multiple places.
-}

import Arr exposing (Arr)
import LinearDirection exposing (LinearDirection(..))
import MinArr
import NNats exposing (..)
import Nat exposing (In, Min)
import TypeNats exposing (..)


{-| Last element in a `List`.

    last [ 1, 3, 5, 0 ]
    --> Just 0

    last []
    --> Nothing

-}
last : List element -> Maybe element
last list =
    case list of
        [ one ] ->
            Just one

        [] ->
            Nothing

        _ :: more ->
            last more


removeLast : Arr (In min max) element -> Arr (Min Nat0) element
removeLast arr =
    arr
        |> Arr.lowerMinLength nat0
        |> MinArr.isLengthAtLeast nat1
            { min = nat0 }
            { less = MinArr.value
            , equalOrGreater =
                MinArr.drop nat1 LastToFirst
            }