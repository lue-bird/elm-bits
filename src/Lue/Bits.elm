module Lue.Bits exposing
    ( random
    , toNat, toBytes, padToByte
    , serialize
    )

{-| Working with an [`Arr`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/) of `Bit`s.


## create

@docs random


## transform

@docs toNat, toBytes, padToByte


## extra

@docs serialize

&emsp;

`Lue.` is just the github username to avoid overlaps in module names.

-}

import Arr exposing (Arr)
import ArrExtra as Arr
import Array exposing (Array)
import Common
import LinearDirection exposing (LinearDirection(..))
import Lue.Bit as Bit exposing (Bit(..))
import NNats exposing (..)
import Nat exposing (ArgIn, In, Min, Nat)
import Random
import Serialize exposing (Codec)
import TypeNats exposing (..)
import Typed exposing (val)


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) for an `Arr` of `Bit`s.
-}
serialize :
    Nat (ArgIn min max maybeN)
    -> Codec String (Arr (In min max) Bit)
serialize length =
    Arr.serialize length Bit.serialize


{-| A `Random.Generator` for an `Arr` of `Bit`s.

To use it effectively, you will need some [extra bits of randomness](https://package.elm-lang.org/packages/NoRedInk/elm-random-pcg-extended/latest/).

-}
random : Nat (ArgIn min max maybeN) -> Random.Generator (Arr (In min max) Bit)
random bitCount =
    Arr.random bitCount Bit.random


{-| Convert an `Arr` of up to 53 `Bit`s to a `Nat`.

Bits from the [53](https://package.elm-lang.org/packages/elm-community/basics-extra/4.1.0/Basics-Extra#maxSafeInteger)th index become mathematically unsound so they are avoided.

    Bits.toNat (Arr.from8 I O O O I O I O)
    --> Nat 138

-}
toNat : Arr (In min Nat53) Bit -> Nat (Min Nat0)
toNat bits =
    Common.bitsToNat bits


{-| Group an `Arr` of `Bit`s into `Arr`s of size 8.

    Bits.toBytes (Arr.from1 O)
    --> Arr.from1 (Arr.from8 O O O O O O O O)

    Arr.from3 I I I
        |> Arr.extendOnly nat8 (Arr.from8 O I I I O I O O)
        |> Arr.extendOnly nat8 (Arr.from8 O I I I O I O O)
        |> Arr.extendOnly nat8 (Arr.from8 O I I I O I O O)
        |> Bits.toBytes
    --> (Arr.from8 O O O O O I I I
    -->     |> Arr.push (Arr.from8 O I I I O I O O)
    -->     |> Arr.push (Arr.from8 O I I I O I O O)
    -->     |> Arr.push (Arr.from8 O I I I O I O O)
    --> )

`O`s at the start are kept.

-}
toBytes :
    Arr (In min max) Bit
    -> Arr (Min Nat0) (Arr (In Nat8 (Nat8Plus a)) Bit)
toBytes =
    Arr.groupPaddingLeft nat8 O


{-| Fill `O`s to the begin of the `Arr` until it has 8 elements.

    Arr.from3 I I I |> Bits.toBytes
    --> Arr.from8 O O O O O I I I

-}
padToByte : Arr (In min Nat8) Bit -> Arr (In Nat8 (Nat8Plus a)) Bit
padToByte bitArr =
    Arr.resize LastToFirst nat8 O bitArr
