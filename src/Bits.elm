module Bits exposing
    ( fromIntUnsigned, toIntUnsigned
    , fromIntSigned, toIntSigned
    )

{-| Convert from and to a `List` of [`Bit`](Bit#Bit)s

@docs fromIntUnsigned, toIntUnsigned
@docs fromIntSigned, toIntSigned

You can also use all the operations that work on `List`, for example

    -- complement
    List.map Bit.opposite

    -- left shift
    (_ ++ List.repeat n Bit.O) |> List.take x

    -- left right
    (List.repeat n Bit.O ++ _) |> List.take x

    ...

-}

import Bit exposing (Bit)
import Bitwise


{-| Convert the unsigned `Int`
to a given number of bits (`<= 32`).

    import Bit exposing (Bit(..))
    import Bits

    14 |> Bits.fromIntUnsigned 4
    --> [ I, I, I, O ]

The `Int` is always clamped to `<= 2 ^ bitCount - 1`

    import Bits

    (2 ^ 53 - 1)
        |> Bits.fromIntUnsigned 32
        |> Bits.toIntUnsigned
    --> 2 ^ 32 - 1

-}
fromIntUnsigned : Int -> (Int -> List Bit)
fromIntUnsigned bitCount intUnsigned =
    if intUnsigned |> intUnsignedHasAtMostBits bitCount then
        List.range 0 (bitCount - 1)
            |> List.reverse
            |> List.map
                (\bitIndex -> intUnsigned |> intUnsignedBitAt bitIndex)

    else
        List.repeat bitCount Bit.I


intUnsignedHasAtMostBits : Int -> (Int -> Bool)
intUnsignedHasAtMostBits maximumBitCount =
    \intUnsigned ->
        intUnsigned < 2 * (2 ^ maximumBitCount)


intUnsignedBitAt : Int -> (Int -> Bit)
intUnsignedBitAt indexFrom0 =
    \intUnsigned ->
        (intUnsigned |> Bitwise.shiftRightBy indexFrom0)
            |> bitFromRemainderBy2


bitFromRemainderBy2 : Int -> Bit
bitFromRemainderBy2 =
    \int ->
        case int |> Basics.remainderBy 2 of
            0 ->
                Bit.O

            -- 1
            _ ->
                Bit.I


{-| Convert ≤ 32 bits into a natural `Int` ≥ 0

    import Bit exposing (Bit(..))
    import Bits

    [ I, O, O, I, O, I ]
        |> Bits.toIntUnsigned
    --> 37

-}
toIntUnsigned : List Bit -> Int
toIntUnsigned =
    \bits ->
        bits
            |> List.foldr
                (\bit soFar ->
                    { power = soFar.power + 1
                    , total =
                        case bit of
                            Bit.O ->
                                soFar.total

                            Bit.I ->
                                soFar.total + (2 ^ soFar.power)
                    }
                )
                { power = 0, total = 0 }
            |> .total


{-| Encode with a given bit size `n` (≤ 32)
ranging from `-(2 ^ (n - 1))` to `2 ^ (n - 1) - 1`.
For example, bit size 5 ranges from -16 to 15

    import Bit exposing (Bit(..))
    import Bits

    -6 |> Bits.fromIntSigned 5
    --> [ I, I, O, I, O ]

    10 |> Bits.fromIntSigned 6
    --> [ O, O, I, O, I, O ]

-}
fromIntSigned : Int -> (Int -> List Bit)
fromIntSigned bitSizeAvailable intSigned =
    if intSigned >= 0 then
        intSigned
            |> fromIntUnsigned (bitSizeAvailable - 1)
            |> (::) Bit.O

    else
        (intSigned + 1 |> Basics.abs)
            |> fromIntUnsigned (bitSizeAvailable - 1)
            |> List.map Bit.opposite
            |> (::) Bit.I


{-| Create signed `Int` from a bit array's in 2's complement encoding
using its length (≤ 32) as the `Int`'s bit count.

So given bit count `n`,
decodes in the range `-(2 ^ (n - 1))` → `2 ^ (n - 1) - 1`.
For example, bit size 5 ranges from -16 to 15

    import Bit exposing (Bit(..))
    import Bits

    [ O, I, O, I, O ] |> Bits.toIntSigned
    --> 10

    [ I, O, I, O, I, O, I, O ] |> Bits.toIntSigned
    --> -86

    [ O, O, O, O, O, I, O, I, O, I, O ]
        |> Bits.toIntSigned
    --> 42

-}
toIntSigned : List Bit -> Int
toIntSigned =
    \bits ->
        case bits of
            [] ->
                0

            firstBit :: secondBitUp ->
                case firstBit of
                    Bit.O ->
                        secondBitUp |> toIntUnsigned

                    Bit.I ->
                        (secondBitUp
                            |> List.map Bit.opposite
                            |> toIntUnsigned
                            |> negate
                        )
                            - 1
