module Benchmarks exposing (benchmarks)

import Benchmark
import Benchmark.Alternative
import Bit exposing (Bit)
import Bits
import Bitwise


benchmarks : Benchmark.Benchmark
benchmarks =
    Benchmark.describe "elm-bits"
        [ Benchmark.Alternative.rank "bits from Int unsigned"
            (\fromIntUnsigned -> List.map (fromIntUnsigned 32) exampleListOfIntUnsigneds)
            [ ( "TCO recursive using bitwise mask and shift", fromIntUnsignedRecursiveWithTCOUsingMaskAndShift )
            , ( "TCO recursive using bitwise mask and //", fromIntUnsignedRecursiveWithTCOUsingMask )
            , ( "TCO recursive using remainderBy and //", fromIntUnsignedRecursiveWithTCO )
            , ( "non-TCO recursive |> List.reverse", fromIntUnsignedUsingNonTCORecursionReverse )
            , ( "List.range", fromIntUnsignedUsingListRange )
            ]
        , Benchmark.Alternative.rank "bits to Int unsigned"
            (\toIntUnsigned -> List.map toIntUnsigned exampleListOfBits32)
            [ ( "foldr", toIntUnsignedUsingFoldr )
            , ( "recursive TCO using 2 *", toIntUnsignedUsingRecursiveUsingMultiplyWithTCO )
            , ( "recursive TCO using bitwise shift", toIntUnsignedUsingRecursiveUsingBitwiseShiftWithTCO )
            , ( "recursive without TCO using 2 ^", toIntUnsignedUsingRecursiveWithoutTCOUsingPower )
            , ( "recursive without TCO using bitwise shift", toIntUnsignedUsingRecursiveWithoutTCOUsingBitwiseShift )
            , ( "recursive TCO using 2 * and nested cases", toIntUnsignedUsingRecursiveUsingMultiplyWithTCOAndNestedCases )
            ]
        , Benchmark.Alternative.rank "bits to List of Int unsigned 8s"
            (\toIntUnsigned8s -> List.map toIntUnsigned8s exampleListOfBitsLong)
            [ ( "recursive 8-uncons |> List.reverse", toIntUnsigned8sRecursive8UnconsThenReverse )
            , ( "recursive List.take/List.drop |> List.reverse", toIntUnsigned8sRecursiveTakeDropThenReverse )
            ]
        ]


exampleListOfBits32 : List (List Bit)
exampleListOfBits32 =
    (List.range 0 16
        |> List.map
            (\n ->
                List.repeat n Bit.O
                    ++ List.repeat n Bit.I
            )
    )
        ++ (List.range 0 16
                |> List.map
                    (\n ->
                        List.repeat n Bit.I
                            ++ List.repeat n Bit.O
                    )
           )


exampleListOfBitsLong : List (List Bit)
exampleListOfBitsLong =
    (List.repeat 2000 Bit.O
        ++ List.repeat 2000 Bit.I
        ++ List.repeat 2000 Bit.I
        ++ List.repeat 2000 Bit.O
    )
        :: exampleListOfBits32


exampleListOfIntUnsigneds : List Int
exampleListOfIntUnsigneds =
    [ 0, 1, 5, 2 ^ 17 + 3274, 2 ^ 32 - 1, 392974, 38284, 244 ]


fromIntUnsignedRecursiveWithTCOUsingMaskAndShift : Int -> (Int -> List Bit)
fromIntUnsignedRecursiveWithTCOUsingMaskAndShift bitCount intUnsigned =
    fromIntUnsignedRecursiveWithTCOUsingMaskOnto [] bitCount intUnsigned


fromIntUnsignedRecursiveWithTCOUsingMask : Int -> (Int -> List Bit)
fromIntUnsignedRecursiveWithTCOUsingMask bitCount intUnsigned =
    fromIntUnsignedRecursiveWithTCOUsingMaskOnto [] bitCount intUnsigned


fromIntUnsignedRecursiveWithTCOUsingMaskOnto : List Bit -> Int -> (Int -> List Bit)
fromIntUnsignedRecursiveWithTCOUsingMaskOnto soFar bitCount intUnsigned =
    if bitCount <= 0 then
        soFar

    else
        fromIntUnsignedRecursiveWithTCOUsingMaskOnto
            (bitFromRemainderBy2UsingMask intUnsigned :: soFar)
            (bitCount - 1)
            (intUnsigned // 2)


fromIntUnsignedRecursiveWithTCO : Int -> (Int -> List Bit)
fromIntUnsignedRecursiveWithTCO bitCount intUnsigned =
    fromIntUnsignedRecursiveWithTCOOnto [] bitCount intUnsigned


fromIntUnsignedRecursiveWithTCOOnto : List Bit -> Int -> (Int -> List Bit)
fromIntUnsignedRecursiveWithTCOOnto soFar bitCount intUnsigned =
    if bitCount <= 0 then
        soFar

    else
        fromIntUnsignedRecursiveWithTCOOnto
            (bitFromRemainderBy2 intUnsigned :: soFar)
            (bitCount - 1)
            (intUnsigned // 2)


fromIntUnsignedUsingNonTCORecursionReverse : Int -> (Int -> List Bit)
fromIntUnsignedUsingNonTCORecursionReverse bitCount intUnsigned =
    fromIntUnsignedReverse bitCount intUnsigned |> List.reverse


fromIntUnsignedReverse : Int -> (Int -> List Bit)
fromIntUnsignedReverse bitCount intUnsigned =
    if bitCount <= 0 then
        []

    else
        (intUnsigned |> bitFromRemainderBy2)
            :: (intUnsigned // 2 |> fromIntUnsignedReverse (bitCount - 1))


fromIntUnsignedUsingListRange : Int -> (Int -> List Bit)
fromIntUnsignedUsingListRange bitCount intUnsigned =
    List.range 0 (bitCount - 1)
        |> List.reverse
        |> List.map
            (\bitIndex -> intUnsigned |> intUnsignedBitAt bitIndex)


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


bitFromRemainderBy2UsingMask : Int -> Bit
bitFromRemainderBy2UsingMask =
    \int ->
        case int |> Bitwise.and 1 of
            0 ->
                Bit.O

            -- 1
            _ ->
                Bit.I



--


toIntUnsignedUsingFoldr : List Bit -> Int
toIntUnsignedUsingFoldr =
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


toIntUnsignedUsingRecursiveUsingBitwiseShiftWithTCO : List Bit -> Int
toIntUnsignedUsingRecursiveUsingBitwiseShiftWithTCO bits =
    toIntUnsignedUsingRecursiveWithTCOUsingMultiplyOnto 0 bits


toIntUnsignedUsingRecursiveUsingMultiplyWithTCOAndNestedCases : List Bit -> Int
toIntUnsignedUsingRecursiveUsingMultiplyWithTCOAndNestedCases bits =
    toIntUnsignedUsingRecursiveWithTCOUsingMultiplyAndNestedCasesOnto 0 bits


toIntUnsignedUsingRecursiveWithTCOUsingMultiplyAndNestedCasesOnto : Int -> List Bit -> Int
toIntUnsignedUsingRecursiveWithTCOUsingMultiplyAndNestedCasesOnto soFar bits =
    case bits of
        [] ->
            soFar

        Bit.O :: tailBits ->
            toIntUnsignedUsingRecursiveWithTCOUsingMultiplyAndNestedCasesOnto
                (soFar * 2)
                tailBits

        Bit.I :: tailBits ->
            toIntUnsignedUsingRecursiveWithTCOUsingMultiplyAndNestedCasesOnto
                ((soFar * 2) + 1)
                tailBits


toIntUnsignedUsingRecursiveUsingMultiplyWithTCO : List Bit -> Int
toIntUnsignedUsingRecursiveUsingMultiplyWithTCO bits =
    toIntUnsignedUsingRecursiveWithTCOUsingMultiplyOnto 0 bits


toIntUnsignedUsingRecursiveWithTCOUsingMultiplyOnto : Int -> List Bit -> Int
toIntUnsignedUsingRecursiveWithTCOUsingMultiplyOnto soFar bits =
    case bits of
        [] ->
            soFar

        headBit :: tailBits ->
            toIntUnsignedUsingRecursiveWithTCOUsingMultiplyOnto
                ((soFar * 2)
                    + (case headBit of
                        Bit.O ->
                            0

                        Bit.I ->
                            1
                      )
                )
                tailBits


toIntUnsignedUsingRecursiveWithoutTCOUsingPower : List Bit -> Int
toIntUnsignedUsingRecursiveWithoutTCOUsingPower bits =
    toIntUnsignedAndBitCountUsingRecursiveWithoutTCOUsingPower bits |> .int


toIntUnsignedAndBitCountUsingRecursiveWithoutTCOUsingPower : List Bit -> { int : Int, bitCount : Int }
toIntUnsignedAndBitCountUsingRecursiveWithoutTCOUsingPower bits =
    case bits of
        [] ->
            { int = 0, bitCount = 0 }

        headBit :: tailBits ->
            let
                tailResult : { int : Int, bitCount : Int }
                tailResult =
                    toIntUnsignedAndBitCountUsingRecursiveWithoutTCOUsingPower tailBits
            in
            { int =
                case headBit of
                    Bit.O ->
                        tailResult.int

                    Bit.I ->
                        2 ^ tailResult.bitCount + tailResult.int
            , bitCount = tailResult.bitCount + 1
            }


toIntUnsignedUsingRecursiveWithoutTCOUsingBitwiseShift : List Bit -> Int
toIntUnsignedUsingRecursiveWithoutTCOUsingBitwiseShift bits =
    toIntUnsignedAndBitCountUsingRecursiveWithoutTCOUsingBitwiseShift bits |> .int


toIntUnsignedAndBitCountUsingRecursiveWithoutTCOUsingBitwiseShift : List Bit -> { int : Int, bitCount : Int }
toIntUnsignedAndBitCountUsingRecursiveWithoutTCOUsingBitwiseShift bits =
    case bits of
        [] ->
            { int = 0, bitCount = 0 }

        headBit :: tailBits ->
            let
                tailResult : { int : Int, bitCount : Int }
                tailResult =
                    toIntUnsignedAndBitCountUsingRecursiveWithoutTCOUsingBitwiseShift tailBits
            in
            { int =
                case headBit of
                    Bit.O ->
                        tailResult.int

                    Bit.I ->
                        Bitwise.shiftLeftBy tailResult.bitCount 1 + tailResult.int
            , bitCount = tailResult.bitCount + 1
            }



--


toIntUnsigned8sRecursive8UnconsThenReverse : List Bit -> List Int
toIntUnsigned8sRecursive8UnconsThenReverse =
    \bits -> bits |> toIntUnsigned8sRecursive8UnconsReverseOnto [] |> List.reverse


toIntUnsigned8sRecursive8UnconsReverseOnto : List Int -> List Bit -> List Int
toIntUnsigned8sRecursive8UnconsReverseOnto soFar bits =
    case bits of
        [] ->
            soFar

        bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8Up ->
            toIntUnsigned8sRecursive8UnconsReverseOnto
                (Bits.toIntUnsigned [ bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7 ]
                    :: soFar
                )
                bit8Up

        upTo7Bits ->
            Bits.toIntUnsigned upTo7Bits :: soFar


toIntUnsigned8sRecursiveTakeDropThenReverse : List Bit -> List Int
toIntUnsigned8sRecursiveTakeDropThenReverse =
    \bits ->
        bits
            |> toIntUnsigned8sRecursiveTakeDropReverseOnto [] 8
            |> List.reverse


toIntUnsigned8sRecursiveTakeDropReverseOnto : List Int -> Int -> List Bit -> List Int
toIntUnsigned8sRecursiveTakeDropReverseOnto soFar chunkSize list =
    case List.drop chunkSize list of
        [] ->
            case list of
                [] ->
                    soFar

                head :: tail ->
                    Bits.toIntUnsigned (head :: tail) :: soFar

        remainingAfterChunkHead :: remainingAfterChunkTail ->
            toIntUnsigned8sRecursiveTakeDropReverseOnto (Bits.toIntUnsigned (List.take chunkSize list) :: soFar)
                chunkSize
                (remainingAfterChunkHead :: remainingAfterChunkTail)
