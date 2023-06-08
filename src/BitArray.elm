module BitArray exposing
    ( fromN, toN
    , fromIntSigned, toIntSigned
    , toChunksOf
    )

{-| [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/)
of [`Bit`](Bit#Bit)s

Use what's available in `ArraySized` to create and analyze bits!

Use the helpers here in `Bits`
when you know the exact number of bits, like in an id


## `N`

@docs fromN, toN


## `Int` signed

@docs fromIntSigned, toIntSigned


## alter

@docs toChunksOf

You can additionally use the operations of `ArraySized`, for example

    -- left shift
    ArraySized.attach Up (ArraySized.repeat O n)

    -- left right
    ArraySized.attach Down (ArraySized.repeat O n)

    -- complement
    ArraySized.map Bit.opposite

    ...

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit(..))
import Bitwise
import Emptiable
import Linear exposing (Direction(..))
import N exposing (Add1, Exactly, In, Min, N, On, To, Up, Up0, n0, n1, n2)


{-| Pad the `ArraySized` of [`Bit`](Bit#Bit)s to chunks of a given length

    import ArraySized
    import Linear exposing (Direction(..))
    import N exposing (n8)
    import Bit exposing (Bit(..))

    ArraySized.one O
        |> BitArray.toChunksOf n8
        |> ArraySized.map ArraySized.toList
        |> ArraySized.toList
    --> [ [ O, O, O, O, O, O, O, O ] ]

    ArraySized.l3 I I I
        |> ArraySized.attach Up (ArraySized.l8 O I I I O I O O)
        |> ArraySized.attach Up (ArraySized.l8 O I I I O I O O)
        |> ArraySized.attach Up (ArraySized.l8 O I I I O I O O)
        |> BitArray.toChunksOf n8
        |> ArraySized.map ArraySized.toList
        |> ArraySized.toList
    --> [ [ O, O, O, O, O, I, I, I ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> ]

[`O`](Bit#Bit)s at the beginning aren't removed from the original `ArraySized`

-}
toChunksOf :
    N (Exactly (On (Add1 chunkLengthMinus1)))
    ->
        (ArraySized
            Bit
            (In (On min_) (Up maxX To maxPlusX))
         ->
            ArraySized
                (ArraySized
                    Bit
                    (Exactly (On (Add1 chunkLengthMinus1)))
                )
                (In (Up0 minX_) (Up maxX To (Add1 maxPlusX)))
        )
toChunksOf chunkBitLength =
    \arraySized ->
        let
            chunked =
                arraySized
                    |> ArraySized.toChunksOf Down chunkBitLength
        in
        case chunked.remainder |> ArraySized.hasAtLeast n1 of
            Err _ ->
                chunked.chunks |> ArraySized.maxAdd n1

            Ok remainder ->
                chunked.chunks
                    |> ArraySized.insert ( Up, n1 )
                        (remainder
                            |> ArraySized.maxAdd n1
                            |> ArraySized.toSize Down chunkBitLength (\_ -> O)
                        )
                    |> ArraySized.minTo n0



-- N


{-| Convert the [natural number](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)
to a given number of bits.

    import N exposing (n4, n14)
    import Bit exposing (Bit(..))
    import ArraySized

    n14 |> BitArray.fromN n4 |> ArraySized.toList
    --> [ I, I, I, O ]

The `N` is always clamped to `<= 2 ^ bitCount - 1`

    import N exposing (n0)
    import N.Local exposing (n32)

    2 ^ 53 - 1
        |> N.intToAtLeast n0
        |> BitArray.fromN n32
        |> BitArray.toN
        |> N.toInt
    --> 2 ^ 32 - 1

-}
fromN :
    N (In (Up minX To minPlusX) max)
    ->
        (N range_
         -> ArraySized Bit (In (Up minX To minPlusX) max)
        )
fromN bitCount =
    \n ->
        if n |> nHasAtMostBits bitCount then
            ArraySized.n1To bitCount
                |> ArraySized.reverse
                |> ArraySized.map
                    (\bitIndex ->
                        n |> nBitAt bitIndex
                    )

        else
            ArraySized.repeat I bitCount


nHasAtMostBits : N bitCountRange_ -> (N range_ -> Bool)
nHasAtMostBits bitCount =
    \n ->
        (n |> N.toInt |> Bitwise.shiftRightBy (bitCount |> N.toInt)) == 0


nBitAt : N (In indexMin_ indexMax_) -> (N range_ -> Bit)
nBitAt indexFrom1 =
    \n ->
        (n |> N.toInt |> Bitwise.shiftRightBy ((indexFrom1 |> N.toInt) - 1))
            |> N.intModBy n2
            |> Bit.fromN


{-| Convert <= 32 bits into an unsigned integer: `N (Min (Up0 nX_))`

    import N
    import Bit exposing (Bit(..))
    import ArraySized

    ArraySized.l6 I O O I O I
        |> BitArray.toN
        |> N.toInt
    --> 37

-}
toN :
    ArraySized Bit (In min_ max_)
    -> N (Min (Up0 nX_))
toN =
    \bits ->
        bits
            |> ArraySized.foldFrom
                { power = 0, total = 0 }
                Down
                (\bit soFar ->
                    { power = soFar.power + 1
                    , total =
                        soFar.total
                            |> (case bit of
                                    O ->
                                        identity

                                    I ->
                                        \x -> x + (2 ^ soFar.power)
                               )
                    }
                )
            |> .total
            |> N.intToAtLeast n0



-- `Int` signed


{-| Clamp a signed `Int`, decoding a given bit size `n`
ranging from `-(2 ^ (n - 1))` to `2 ^ (n - 1) - 1`.
For example, bit size 5 ranges from -16 to 15

    import ArraySized
    import Bit exposing (Bit(..))
    import N exposing (n5, n6)

    -6 |> BitArray.fromIntSigned n5 |> ArraySized.toList
    --> [ I, I, O, I, O ]

    10 |> BitArray.fromIntSigned n6 |> ArraySized.toList
    --> [ O, O, I, O, I, O ]

-}
fromIntSigned :
    N (Exactly (On (Add1 bitSizeFrom1)))
    ->
        (Int
         -> ArraySized Bit (Exactly (On (Add1 bitSizeFrom1)))
        )
fromIntSigned bitSizeAvailable =
    \int ->
        case int |> N.intIsAtLeast n0 of
            Ok intAtLeast0 ->
                intAtLeast0
                    |> fromN (bitSizeAvailable |> N.subtract n1)
                    |> ArraySized.insert ( Up, n1 ) O

            Err negativeInt ->
                negativeInt
                    + 1
                    |> N.intToAbsolute
                    |> fromN (bitSizeAvailable |> N.subtract n1)
                    |> ArraySized.insert ( Up, n1 ) O
                    |> ArraySized.map Bit.opposite


{-| Create signed `Int` from a bit array's in 2's complement encoding
using its length as the `Int`'s bit count.

So given bit count `n`,
decodes in the range `-(2 ^ (n - 1))` → `2 ^ (n - 1) - 1`.
For example, bit size 5 ranges from -16 to 15

    import ArraySized
    import Bit exposing (Bit(..))
    import N exposing (n5, n6)

    ArraySized.l5 O I O I O |> BitArray.toIntSigned
    --> 10

    ArraySized.l8 I O I O I O I O |> BitArray.toIntSigned
    --> -86

    ArraySized.l11 O O O O O I O I O I O |> BitArray.toIntSigned
    --> 42

-}
toIntSigned : ArraySized Bit (In min_ max_) -> Int
toIntSigned =
    \bits ->
        case bits |> ArraySized.hasAtLeast n1 of
            Err _ ->
                0

            Ok bitsAtLeast1 ->
                case bitsAtLeast1 |> ArraySized.element ( Up, n1 ) of
                    Bit.O ->
                        bitsAtLeast1 |> toN |> N.toInt

                    Bit.I ->
                        (bitsAtLeast1
                            |> ArraySized.map Bit.opposite
                            |> ArraySized.removeMin ( Up, n1 )
                            |> toN
                            |> N.toInt
                            |> negate
                        )
                            - 1
