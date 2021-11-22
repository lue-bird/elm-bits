module Lue.Bits exposing
    ( random
    , toNat, toBytes, padToByte
    , serialize
    )

{-| Working with an [`Arr`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/) of `Bit`s. To `import as Bits`


## create

@docs random


## transform

@docs toNat, toBytes, padToByte

TODO: toInt etc.


## extra

@docs serialize

&emsp;

`Lue.` is just the github username to avoid overlaps in module names.

-}

import Arr exposing (Arr)
import ArrExtra as Arr
import Common
import InArr
import LinearDirection exposing (LinearDirection(..))
import Lue.Bit as Bit exposing (Bit(..))
import Nat exposing (ArgIn, In, Min, Nat)
import Nats exposing (..)
import Random
import Serialize exposing (Codec)


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) for an `Arr` of `Bit`s.

To get a `Codec` with a string error:

import Serialize exposing (Codec)

    bit128 :
        Codec
            String
            (Arr (In Nat128 (Nat128Plus a_)) Bit)
    bit128 =
        Bits.serialize nat128 Arr.errorToString

Use the `serialize` functions in `InArr` and `MinArr` to serialize `Arr`s with a length in a range.

-}
serialize :
    Nat (ArgIn minLength maxLength ifN_)
    -> (InArr.Error -> error)
    -> Codec error (Arr (In minLength maxLength) Bit)
serialize length toSerializeError =
    InArr.serialize length toSerializeError Bit.serialize


{-| A `Random.Generator` for an `Arr` of `Bit`s.

To use it effectively, you will need some [extra bits of randomness](https://package.elm-lang.org/packages/NoRedInk/elm-random-pcg-extended/latest/).

-}
random :
    Nat (ArgIn minAmount maxAmount ifN_)
    -> Random.Generator (Arr (In minAmount maxAmount) Bit)
random bitCount =
    Arr.random bitCount Bit.random


{-| Convert an `Arr` of up to 53 `Bit`s to a `Nat`.

Bits from the [53](https://package.elm-lang.org/packages/elm-community/basics-extra/4.1.0/Basics-Extra#maxSafeInteger)th index become mathematically unsound so they are avoided.

    Bits.toNat (Arr.from8 I O O O I O I O)
    --> Nat 138

-}
toNat : Arr (In minLength_ Nat53) Bit -> Nat (Min Nat0)
toNat bits =
    Common.bitsToNat bits


{-| Group an `Arr` of `Bit`s into `Arr`s of size 8.

    Bits.toBytes (Arr.from1 O)
    --> Arr.from1 (Arr.from8 O O O O O O O O)

    Arr.from3 I I I
        |> InArr.append nat8 (Arr.from8 O I I I O I O O)
        |> InArr.append nat8 (Arr.from8 O I I I O I O O)
        |> InArr.append nat8 (Arr.from8 O I I I O I O O)
        |> Bits.toBytes
    --> (Arr.from8 O O O O O I I I
    -->     |> Arr.push (Arr.from8 O I I I O I O O)
    -->     |> Arr.push (Arr.from8 O I I I O I O O)
    -->     |> Arr.push (Arr.from8 O I I I O I O O)
    --> )

`O`s at the start are kept.

-}
toBytes :
    Arr (In minLength_ maxLength_) Bit
    -> Arr (Min Nat0) (Arr (In Nat8 (Nat8Plus a_)) Bit)
toBytes =
    Arr.groupPaddingLeft nat8 O


{-| Fill `O`s to the begin of the `Arr` until it has 8 elements.

    Arr.from3 I I I |> Bits.padToByte
    --> Arr.from8 O O O O O I I I

-}
padToByte :
    Arr (In min_ Nat8) Bit
    -> Arr (In Nat8 (Nat8Plus a_)) Bit
padToByte bitArr =
    Arr.resize LastToFirst nat8 O bitArr
