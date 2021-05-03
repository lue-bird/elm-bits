module Lue.Byte exposing (serialize)

{-| 8 `Bit`s.

    Arr.from8 I O O O O I O I

is a byte.


## extra

@docs serialize

&emsp;

`Lue.`, the github username is used to avoid overlaps in module names.

-}

import Arr exposing (Arr)
import Common
import LinearDirection exposing (LinearDirection(..))
import Lue.Bit exposing (Bit(..))
import NNats exposing (..)
import Nat exposing (In, Only)
import Serialize
import TypeNats exposing (..)
import Typed exposing (val)


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize a byte.
-}
serialize : Serialize.Codec String (Arr (Only Nat8) Bit)
serialize =
    let
        fromInt : Int -> Arr (In Nat8 (Nat8Plus a)) Bit
        fromInt int =
            Arr.nats nat8
                |> Arr.reverse
                |> Arr.map
                    (\power ->
                        case int // (2 ^ val power) of
                            0 ->
                                O

                            _ ->
                                I
                    )
    in
    Serialize.byte
        |> Serialize.map fromInt
            (Arr.restoreMaxLength nat8
                >> Common.bitsToNat
                >> val
            )
