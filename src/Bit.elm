module Bit exposing (Bit(..), opposite)

{-| A primitive: the single bit

@docs Bit, opposite

I'll happily add or merge more stuff!
Just open an issue or a PR

-}


{-| One of 2 states:

  - 1, on: `I`
  - 0, off: `O`

This right here is really the core of this library.
Representing bits safely is now quite readable!

    [ O, I, I, I, O, O, I, O ]
    --: List Bit

    randomBit : Random.Generator Bit
    randomBit =
        Random.uniform I [ O ]

    fuzz : Fuzz.Fuzzer Bit
    fuzz =
        Fuzz.oneOfValues [ O, I ]

for an example of using this to represent an `Id`,
visit the [readme](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/)

-}
type Bit
    = I
    | O


{-| Switch `O` ↔ `I`. Often called the not, negate or flip operator
-}
opposite : Bit -> Bit
opposite =
    \bit ->
        case bit of
            O ->
                I

            I ->
                O
