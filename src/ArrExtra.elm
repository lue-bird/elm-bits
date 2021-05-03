module ArrExtra exposing (groupPaddingLeft)

{-| -}

import Arr exposing (Arr)
import Array
import Array.LinearDirection as Array
import LinearDirection exposing (LinearDirection(..))
import MinArr
import NNats exposing (..)
import Nat exposing (ArgIn, In, Min, Nat)
import TypeNats exposing (..)
import Typed exposing (val)


groupPaddingLeft :
    Nat (ArgIn (Nat1Plus minGroupSizeMinus1) maxGroupSize maybeN)
    -> element
    -> Arr (In min max) element
    -> Arr (Min Nat0) (Arr (In (Nat1Plus minGroupSizeMinus1) maxGroupSize) element)
groupPaddingLeft groupSize defaultElement bitArr =
    let
        { groups, less } =
            Arr.groupsOf groupSize LastToFirst bitArr
    in
    if Array.isEmpty (less |> Arr.toArray) then
        groups |> MinArr.value

    else
        less
            |> Arr.resize LastToFirst groupSize defaultElement
            |> Arr.from1
            |> MinArr.extend groups nat0
            |> Arr.lowerMinLength nat0
