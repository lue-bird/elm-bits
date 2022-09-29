module Bit exposing
    ( Bit(..)
    , random
    , fromN, toN
    , opposite
    )

{-| A primitive: the single bit

@docs Bit


## create

@docs random

just like `random`, its pretty easy to implement all sorts of generators etc. for a [`Bit`](#Bit)

    bitFuzz : Fuzzer Bit
    bitFuzz =
        Fuzz.oneOf (List.map Fuzz.constant [ O, I ])


## `N`

@docs fromN, toN


## alter

@docs opposite

I'll be happy to add or merge more stuff!
Just open an issue or a PR

-}

import N exposing (In, N, N1, To, Up, Up0, Up1, n0, n1)
import Random


{-| One of 2 states:

  - 1, on: `I`
  - 0, off: `O`

This right here is really the core of this library.
Representing bits safely is now quite readable!

    ArraySized.l8 O I I I O O I O
    --: ArraySized (In (Up8 minX_) (Up8 maxX_)) Bit

for an example of using this to represent an `Id`,
visit the [readme](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/)

-}
type Bit
    = I
    | O


{-| Uniform `Random.Generator` for either [`I`](#Bit) or [`O`](#Bit)
-}
random : Random.Generator Bit
random =
    Random.uniform I [ O ]


{-| `n0` → [`O`](#Bit), `n1` → [`I`](#Bit)
-}
fromN : N (In min_ (Up maxTo1_ To N1)) -> Bit
fromN =
    \n ->
        case n |> N.toInt of
            0 ->
                O

            -- 1
            _ ->
                I


{-| Switch `O` ↔ `I`. Often called the NOT operator
-}
opposite : Bit -> Bit
opposite =
    \bit ->
        case bit of
            O ->
                I

            I ->
                O


{-| Convert `O` to `n0`, `I` to `1`.
`N (In (Up0 minX_) (Up1 maxX_))` means that the result will be between 0 & 1

    toInt =
        Bit.toN >> N.toInt

-}
toN : Bit -> N (In (Up0 minX_) (Up1 maxX_))
toN =
    \bit ->
        case bit of
            O ->
                n0 |> N.maxTo n1

            I ->
                n1 |> N.minTo n0
