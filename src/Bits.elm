module Bits exposing
    ( fromN, toN
    , fromIntSigned, toIntSigned
    , padToLength, atMost, unpad
    , toChunksOf
    )

{-| [`ArraySized`](https://package.elm-lang.org/packages/lue-bird/elm-typesafe-array/latest/)
of [`Bit`](Bit#Bit)s

Use what's available in `ArraySized` to create and analyze bits!

Use the helpers here in `Bits`
when you know the exact number of bits, like in an id


## `N`

@docs fromN, toN


## `Int`

@docs fromIntSigned, toIntSigned


## alter

@docs padToLength, atMost, unpad
@docs toChunksOf

You can additionally use the operations of `ArraySized`, for example

    -- left shift
    ArraySized.glue Up (ArraySized.repeat O n...)

    -- complement
    ArraySized.map Bit.opposite

    ...

-}

import ArraySized exposing (ArraySized)
import Bit exposing (Bit(..))
import Bitwise
import Linear exposing (Direction(..))
import N exposing (Add1, Exactly, Fixed, In, Min, N, To, Up, Up0, n0, n1)
import N.Local exposing (N31, N32, Up32, n31, n32)


{-| Increase the capacity to a given length
by padding with [`O`](Bit#Bit)s to the beginning

Short for

    ArraySized.padToLength Down (ArraySized.repeat O)

-}
padToLength :
    N (In (Fixed paddedMin) (Up maxX To paddedMaxPlusX))
    ->
        (ArraySized
            Bit
            (In
                (Up paddingMaxPlusX_ To paddedMaxPlusX)
                (Up paddingMin_ To paddedMin)
            )
         ->
            ArraySized
                Bit
                (In (Fixed paddedMin) (Up maxX To paddedMaxPlusX))
        )
padToLength paddedLength =
    \bitsNotFullyPadded ->
        bitsNotFullyPadded
            |> ArraySized.padToLength
                Down
                (ArraySized.repeat O)
                paddedLength


{-| If the `ArraySized` has a [`I`](Bit#Bit) earlier than a given number of [`Bit`](Bit#Bit)s up,
returns only [`I`](Bit#Bit)s up to the given length

    import N exposing (n0, n16)
    import ArraySized

    2 ^ 25
        |> N.intAtLeast n0
        |> Bits.fromN
        |> Bits.atMost n16
        |> ArraySized.maxTo n16
        |> Bits.toN
        |> N.toInt
    --> 2 ^ 16 - 1

Its length after that will be `Exactly` the given desired length.
Some operations like glueing or `Bits.toN` will stop working because of this.
In that case, just

    |> Bits.atMost n
    |> ArraySized.minTo n
    |> ArraySized.maxTo n

To give the `ArraySized`'s _type_ more information

-}
atMost :
    N (Exactly newLength)
    ->
        (ArraySized Bit (In (Fixed min_) (Fixed max_))
         -> ArraySized Bit (Exactly newLength)
        )
atMost =
    \bitSizeAvailable bits ->
        case bits |> unpad |> ArraySized.hasAtMost bitSizeAvailable of
            Ok hasAtMostBitSizeAvailable ->
                hasAtMostBitSizeAvailable
                    |> ArraySized.minTo n0
                    |> padToLength bitSizeAvailable

            Err _ ->
                ArraySized.repeat I bitSizeAvailable


{-| Remove all [`O`](Bit#Bit) from the beginning until the first [`I`](Bit#Bit) is reached

    import N exposing (n4, n14)
    import Bit exposing (Bit(..))
    import ArraySized

    n14 |> Bits.fromN |> Bits.unpad |> ArraySized.toList
    --> [ I, I, I, O ]

-}
unpad :
    ArraySized Bit (In (Fixed min_) (Fixed max))
    -> ArraySized Bit (In (Up0 minX_) (Fixed max))
unpad =
    \arraySized ->
        let
            padOLength : N (In (Up x To x) (Fixed max))
            padOLength =
                arraySized
                    |> ArraySized.foldFrom
                        { padOLength = n0 |> N.maxToInfinity
                        , found = () |> Err
                        }
                        Up
                        (\bit soFar ->
                            case soFar.found of
                                Ok () ->
                                    soFar

                                Err () ->
                                    case bit of
                                        Bit.I ->
                                            { padOLength = soFar.padOLength
                                            , found = () |> Ok
                                            }

                                        Bit.O ->
                                            { padOLength =
                                                soFar.padOLength
                                                    |> N.addMin n1
                                                    |> N.minTo n0
                                            , found = () |> Err
                                            }
                        )
                    |> .padOLength
                    |> N.minTo n0
                    |> N.atMost (arraySized |> ArraySized.length |> N.minTo n0)
                    |> N.minTo n0
        in
        arraySized
            |> ArraySized.dropOverMin ( Up, padOLength )
            |> ArraySized.minTo n0


{-| Pad the `ArraySized` of [`Bit`](Bit#Bit)s to chunks of a given length

    import ArraySized
    import Linear exposing (Direction(..))
    import N exposing (n8)
    import Bit exposing (Bit(..))

    ArraySized.l1 O
        |> Bits.toChunksOf n8
        |> ArraySized.map ArraySized.toList
        |> ArraySized.toList
    --> [ [ O, O, O, O, O, O, O, O ] ]

    ArraySized.l3 I I I
        |> ArraySized.glue Up (ArraySized.l8 O I I I O I O O)
        |> ArraySized.glue Up (ArraySized.l8 O I I I O I O O)
        |> ArraySized.glue Up (ArraySized.l8 O I I I O I O O)
        |> Bits.toChunksOf n8
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
    N (Exactly (Add1 chunkLengthMinus1))
    ->
        (ArraySized
            Bit
            (In (Fixed min_) (Up maxX To maxPlusX))
         ->
            ArraySized
                (ArraySized
                    Bit
                    (Exactly (Add1 chunkLengthMinus1))
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
                chunked.chunks |> ArraySized.maxUp n1

            Ok remainder ->
                chunked.chunks
                    |> ArraySized.insert ( Up, n0 )
                        (remainder
                            |> ArraySized.maxUp n1
                            |> padToLength chunkBitLength
                        )
                    |> ArraySized.minTo n0



-- N


{-| Convert the unsigned integer to <= 32 bits

Combine with [`atMost`](#atMost) to clamp to a lower bit size

    import N exposing (n4, n14)
    import Bit exposing (Bit(..))
    import ArraySized

    n14 |> Bits.fromN |> Bits.atMost n4 |> ArraySized.toList
    --> [ I, I, I, O ]

The `N` is always clamped to `<=  2 ^ 32 - 1`

    import N exposing (n0)

    2 ^ 53 - 1
        |> N.intAtLeast n0
        |> Bits.fromN
        |> Bits.toN
        |> N.toInt
    --> 2 ^ 32 - 1

-}
fromN :
    N range_
    -> ArraySized Bit (In (Up32 minX_) (Up32 maxX_))
fromN =
    \n ->
        if (n |> N.toInt) >= (2 ^ 32) then
            ArraySized.repeat I n32

        else
            ArraySized.upTo n31
                |> ArraySized.minTo n32
                |> ArraySized.reverse
                |> ArraySized.map
                    (\bitIndex ->
                        n |> nBitAt bitIndex
                    )
                |> ArraySized.maxTo n32


nBitAt : N (In indexMin_ (Up indexMaxTo31_ To N31)) -> (N range_ -> Bit)
nBitAt index =
    \n ->
        case
            (n
                |> N.toInt
            )
                // (1 |> Bitwise.shiftLeftBy (index |> N.toInt))
                |> remainderBy 2
        of
            0 ->
                Bit.O

            _ ->
                Bit.I


{-| Convert <= 32 bits into an unsigned integer: `N (Min (Up0 nX_))`

    import N
    import Bit exposing (Bit(..))
    import ArraySized

    ArraySized.l6 I O O I O I
        |> Bits.toN
        |> N.toInt
    --> 37

-}
toN :
    ArraySized Bit (In min_ (Up maxTo32_ To N32))
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
            |> N.atLeastInt n0



-- `Int` signed


{-| Clamp a signed `Int`, decoding a given bit size `n`
ranging from `-(2 ^ (n - 1))` to `2 ^ (n - 1) - 1`.
For example, bit size 5 ranges from -16 to 15

    import ArraySized
    import Bit exposing (Bit(..))
    import N exposing (n5, n6)

    -6 |> Bits.fromIntSigned n5 |> ArraySized.toList
    --> [ O, I, O, I, O ]

    10 |> Bits.fromIntSigned n6 |> ArraySized.toList
    --> [ I, O, I, O, I, O ]

-}
fromIntSigned :
    N (Exactly bitSize)
    ->
        (Int
         -> ArraySized Bit (Exactly bitSize)
        )
fromIntSigned bitSizeAvailable =
    \int ->
        if int >= (1 |> Bitwise.shiftLeftBy (bitSizeAvailable |> N.toInt)) then
            ArraySized.repeat I bitSizeAvailable

        else
            int
                + (1 |> Bitwise.shiftLeftBy ((bitSizeAvailable |> N.toInt) - 1))
                |> N.atLeastInt n0
                |> fromN
                |> atMost bitSizeAvailable


{-| Create signed `Int`
encoded in its **length as the `Int`'s bit size**

Encoding as follows:
given bit size `n`,
encode in the range `-(2 ^ (n - 1))` → `2 ^ (n - 1) - 1`.
For example, bit size 5 ranges from -16 to 15

    import ArraySized
    import Bit exposing (Bit(..))
    import N exposing (n5, n6)

    ArraySized.l5 O I O I O |> Bits.toIntSigned
    -- 5-bit `Int`
    --> -6

    ArraySized.l6 I O I O I O |> Bits.toIntSigned
    -- 6-bit `Int`
    --> 10

[`padToLength`](#padToLength) to convert it to a higher-bit representation

    import ArraySized
    import Bit exposing (Bit(..))
    import N exposing (n8)

    ArraySized.l6 I O I O I O
        |> Bits.padToLength n8
        |> Bits.toIntSigned
    -- 8-bit `Int`
    --> -128 + 42

-}
toIntSigned :
    ArraySized Bit (In min_ (Up maxTo32 To N32))
    -> Int
toIntSigned =
    \bits ->
        (bits |> toN |> N.toInt)
            - (1
                |> Bitwise.shiftLeftBy
                    ((bits
                        |> ArraySized.length
                        |> N.toInt
                     )
                        - 1
                    )
              )