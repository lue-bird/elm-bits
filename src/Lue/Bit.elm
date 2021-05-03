module Lue.Bit exposing
    ( Bit(..)
    , generate
    , to0or1
    , serialize
    )

{-|

@docs Bit


## create

@docs generate


## shape

@docs to0or1


## extra

@docs serialize

&emsp;

`Lue.` is just the github username to avoid overlaps in module names.

-}

import InNat
import NNats exposing (..)
import Nat exposing (In, Nat)
import Random
import Serialize
import TypeNats exposing (..)


{-| One of 2 states.

  - 1, on: `I`
  - 0, off: `O`

-}
type Bit
    = I
    | O


{-| Convert `O` to zero, `I` to one.
-}
to0or1 : Bit -> Nat (In Nat0 (Nat1Plus a))
to0or1 =
    \bit ->
        case bit of
            O ->
                nat0 |> InNat.value

            I ->
                nat1 |> Nat.lowerMin nat0


{-| `Random.Generator` for either `I` or `O`.
-}
generate : Random.Generator Bit
generate =
    Random.uniform I [ O ]


{-| A [`Codec`](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/) to serialize 1 `Bit`.
-}
serialize : Serialize.Codec error Bit
serialize =
    Serialize.enum O [ I ]
