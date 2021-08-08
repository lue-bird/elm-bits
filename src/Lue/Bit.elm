module Lue.Bit exposing
    ( Bit(..)
    , random
    , toNat
    , serialize
    )

{-| To `import as Bit exposing (Bit(..))`

@docs Bit


## create

@docs random


## shape

@docs toNat


## extra

@docs serialize

&emsp;

`Lue.` is just the github username to avoid overlaps in module names.

-}

import InNat
import Nat exposing (In, Nat)
import Nats exposing (..)
import Random
import Serialize


{-| One of 2 states.

  - 1, on: `I`
  - 0, off: `O`

-}
type Bit
    = I
    | O


{-| Convert `O` to zero, `I` to one. `Nat (In Nat0 (Nat1Plus a))` means that the result will be between 0 & 1.

    toInt bit =
        bit |> Bit.toNat |> val

`val` is from [`Typed`](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/Typed).

-}
toNat : Bit -> Nat (In Nat0 (Nat1Plus a_))
toNat =
    \bit ->
        case bit of
            O ->
                nat0 |> InNat.value

            I ->
                nat1 |> Nat.lowerMin nat0


{-| `Random.Generator` for either `I` or `O`.
-}
random : Random.Generator Bit
random =
    Random.uniform I [ O ]


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize 1 `Bit`.
-}
serialize : Serialize.Codec error_ Bit
serialize =
    Serialize.enum O [ I ]
