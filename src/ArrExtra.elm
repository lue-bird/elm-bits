module ArrExtra exposing (groupPaddingLeft)

import Arr exposing (Arr)
import Array
import LinearDirection exposing (LinearDirection(..))
import MinArr
import Nat exposing (ArgIn, In, Min, Nat)
import Nats exposing (..)


groupPaddingLeft :
    Nat (ArgIn (Nat1Plus minGroupSizeMinus1) maxGroupSize ifN_)
    -> element
    -> Arr (In minLength_ maxLength_) element
    -> Arr (Min Nat0) (Arr (In (Nat1Plus minGroupSizeMinus1) maxGroupSize) element)
groupPaddingLeft groupSize defaultElement bitArr =
    let
        { groups, remaining } =
            Arr.groupsOf groupSize LastToFirst bitArr
    in
    if Array.isEmpty (remaining |> Arr.toArray) then
        groups |> Arr.toMin

    else
        remaining
            |> Arr.resize LastToFirst groupSize defaultElement
            |> Arr.from1
            |> MinArr.append nat0 groups
            |> Arr.lowerMinLength nat0
