module ArrExtra exposing (groupPaddingLeft)

import Arr exposing (Arr)
import Array
import Array.LinearDirection as Array
import LinearDirection exposing (LinearDirection(..))
import MinArr
import NNats exposing (..)
import Nat exposing (ArgIn, In, Min, Nat)
import TypeNats exposing (..)


groupPaddingLeft :
    Nat (ArgIn (Nat1Plus minGroupSizeMinus1) maxGroupSize maybeN)
    -> element
    -> Arr (In min max) element
    -> Arr (Min Nat0) (Arr (In (Nat1Plus minGroupSizeMinus1) maxGroupSize) element)
groupPaddingLeft groupSize defaultElement bitArr =
    let
        { groups, remaining } =
            Arr.groupsOf groupSize LastToFirst bitArr
    in
    if Array.isEmpty (remaining |> Arr.toArray) then
        groups |> MinArr.value

    else
        remaining
            |> Arr.resize LastToFirst groupSize defaultElement
            |> Arr.from1
            |> MinArr.append nat0 groups
            |> Arr.lowerMinLength nat0
