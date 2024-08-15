module Bits exposing
    ( fromIntUnsigned, fromIntSigned, fromBytes
    , toIntUnsigned, toIntSigned, toIntUnsigned8s, toIntUnsigned16s, toIntUnsigned32s
    )

{-| Convert from and to a `List` of [`Bit`](Bit#Bit)s

@docs fromIntUnsigned, fromIntSigned, fromBytes

If you have a need for UTF-8 characters and Floats, feel free to [open an issue](https://github.com/lue-bird/elm-bits/issues/new) or PR.


## transform

@docs toIntUnsigned, toIntSigned, toIntUnsigned8s, toIntUnsigned16s, toIntUnsigned32s

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
import Bytes exposing (Bytes)
import Bytes.Decode


{-| Convert the unsigned `Int`
to a given number of bits.

    import Bit exposing (Bit(..))
    import Bits

    14 |> Bits.fromIntUnsigned 4
    --> [ I, I, I, O ]

-}
fromIntUnsigned : Int -> (Int -> List Bit)
fromIntUnsigned bitCount intUnsigned =
    fromIntUnsignedOnto [] bitCount intUnsigned


fromIntUnsignedOnto : List Bit -> Int -> (Int -> List Bit)
fromIntUnsignedOnto soFar bitCount intUnsigned =
    if bitCount <= 0 then
        soFar

    else
        fromIntUnsignedOnto
            ((case Basics.remainderBy 2 intUnsigned of
                0 ->
                    Bit.O

                -- 1
                _ ->
                    Bit.I
             )
                :: soFar
            )
            (bitCount - 1)
            (intUnsigned // 2)


{-| Convert ≤ 32 bits into a natural `Int` ≥ 0

    import Bit exposing (Bit(..))
    import Bits

    [ I, O, O, I, O, I ]
        |> Bits.toIntUnsigned
    --> 37

-}
toIntUnsigned : List Bit -> Int
toIntUnsigned =
    \bits -> toIntUnsignedOnto 0 bits


toIntUnsignedOnto : Int -> List Bit -> Int
toIntUnsignedOnto soFar bits =
    case bits of
        [] ->
            soFar

        headBit :: tailBits ->
            toIntUnsignedOnto
                ((soFar * 2)
                    + (case headBit of
                        Bit.O ->
                            0

                        Bit.I ->
                            1
                      )
                )
                tailBits


{-| Encode a signed `Int` with a given bit size `n`
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
        Bit.O :: (intSigned |> fromIntUnsigned (bitSizeAvailable - 1))

    else
        Bit.I
            :: ((intSigned + 1 |> Basics.abs)
                    |> fromIntUnsigned (bitSizeAvailable - 1)
                    |> List.map Bit.opposite
               )


{-| Create signed `Int` from a bit array's in 2's complement encoding
using its length as the `Int`'s bit count.

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


{-| Convert from [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)
to a list of individual bits.
-}
fromBytes : Bytes -> List Bit
fromBytes =
    \bytes ->
        bytes
            |> Bytes.Decode.decode
                (bitListBytesDecoderOfByteWidth (bytes |> Bytes.width))
            |> Maybe.withDefault []


bitListBytesDecoderOfByteWidth : Int -> Bytes.Decode.Decoder (List Bit)
bitListBytesDecoderOfByteWidth length =
    Bytes.Decode.loop { byteCountLeft = length, reverse = [] }
        (\state ->
            if state.byteCountLeft <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse state.reverse))

            else
                Bytes.Decode.map
                    (\unsignedInt8 ->
                        Bytes.Decode.Loop
                            { byteCountLeft = state.byteCountLeft - 1
                            , reverse = unsignedInt8 :: state.reverse
                            }
                    )
                    Bytes.Decode.unsignedInt8
        )
        |> Bytes.Decode.map
            (\intUnsigned8List ->
                intUnsigned8List
                    |> List.concatMap (\unsignedInt8 -> unsignedInt8 |> fromIntUnsigned 8)
            )


{-| Convert to a list of `Int`s in range 0..255
from a list of individual bits.

    import Bit exposing (Bit(..))
    import Bits

    [ O, O, I, O, O, I, O, I, {- 8 bits -} O, O, I, O, O, I, I, I ]
        |> Bits.toIntUnsigned8s
    --> [ 37, 39 ]

From there it's easy to convert to e.g. [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)
using `List.map Bytes.Encode.unsignedInt8 |> Bytes.Encode.sequence`

You should make sure in your code that, the given bits are a multiple of 8.
If there are remaining bits, `toIntUnsigned8s` treats your input as if it had enough 0s padded to the right.

-}
toIntUnsigned8s : List Bit -> List Int
toIntUnsigned8s =
    \bits -> bits |> toIntUnsigned8sReverseOnto [] |> List.reverse


toIntUnsigned8sReverseOnto : List Int -> List Bit -> List Int
toIntUnsigned8sReverseOnto soFar bits =
    case bits of
        [] ->
            soFar

        bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8Up ->
            toIntUnsigned8sReverseOnto
                (toIntUnsigned [ bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7 ]
                    :: soFar
                )
                bit8Up

        upTo7Bits ->
            toIntUnsigned (padRightToAtLeast 8 upTo7Bits) :: soFar


{-| Convert to a list of `Int`s in range 0..65535
from a list of individual bits.

    import Bit exposing (Bit(..))
    import Bits

    [ O, O, O, O, O, O, O, O, O, O, I, O, O, I, O, I, {- 16 bits -} O, O, O, O, O, O, O, O, O, O, I, O, O, I, I, I ]
        |> Bits.toIntUnsigned16s
    --> [ 37, 39 ]

From there it's easy to convert to e.g. [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)
using `List.map Bytes.Encode.unsignedInt16 |> Bytes.Encode.sequence`

You should make sure in your code that, the given bits are a multiple of 16.
If there are remaining bits, `toIntUnsigned16s` treats your input as if it had enough 0s padded to the right.

-}
toIntUnsigned16s : List Bit -> List Int
toIntUnsigned16s =
    \bits -> bits |> toIntUnsigned16sReverseOnto [] |> List.reverse


toIntUnsigned16sReverseOnto : List Int -> List Bit -> List Int
toIntUnsigned16sReverseOnto soFar bits =
    case bits of
        [] ->
            soFar

        bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8 :: bit9 :: bit10 :: bit11 :: bit12 :: bit13 :: bit14 :: bit15 :: bit16Up ->
            toIntUnsigned16sReverseOnto
                (toIntUnsigned [ bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8, bit9, bit10, bit11, bit12, bit13, bit14, bit15 ]
                    :: soFar
                )
                bit16Up

        upTo15Bits ->
            toIntUnsigned (padRightToAtLeast 16 upTo15Bits) :: soFar


{-| Convert to a list of `Int`s in range 0..4294967295
from a list of individual bits.

    import Bit exposing (Bit(..))
    import Bits

    [ O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, I, O, O, I, O, I, {- 32 bits -} O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, I, O, O, I, I, I ]
        |> Bits.toIntUnsigned32s
    --> [ 37, 39 ]

From there it's easy to convert to e.g. [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)
using `List.map Bytes.Encode.unsignedInt32 |> Bytes.Encode.sequence`

You should make sure in your code that, the given bits are a multiple of 32.
If there are remaining bits, `toIntUnsigned32s` treats your input as if it had enough 0s padded to the right.

-}
toIntUnsigned32s : List Bit -> List Int
toIntUnsigned32s =
    \bits -> bits |> toIntUnsigned32sReverseOnto [] |> List.reverse


toIntUnsigned32sReverseOnto : List Int -> List Bit -> List Int
toIntUnsigned32sReverseOnto soFar bits =
    case bits of
        [] ->
            soFar

        bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8 :: bit9 :: bit10 :: bit11 :: bit12 :: bit13 :: bit14 :: bit15 :: bit16 :: bit17 :: bit18 :: bit19 :: bit20 :: bit21 :: bit22 :: bit23 :: bit24 :: bit25 :: bit26 :: bit27 :: bit28 :: bit29 :: bit30 :: bit31 :: bit32Up ->
            toIntUnsigned32sReverseOnto
                (toIntUnsigned [ bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7, bit8, bit9, bit10, bit11, bit12, bit13, bit14, bit15, bit16, bit17, bit18, bit19, bit20, bit21, bit22, bit23, bit24, bit25, bit26, bit27, bit28, bit29, bit30, bit31 ]
                    :: soFar
                )
                bit32Up

        upTo31Bits ->
            toIntUnsigned (padRightToAtLeast 32 upTo31Bits) :: soFar


padRightToAtLeast : Int -> List Bit -> List Bit
padRightToAtLeast newMinimumLength list =
    list ++ List.repeat (newMinimumLength - (list |> List.length)) Bit.O
