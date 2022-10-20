module Tests exposing (suite)

import ArraySized exposing (ArraySized)
import Bit exposing (Bit(..))
import Bits
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Linear exposing (Direction(..))
import N exposing (n0, n3, n8)
import N.Local exposing (n20, n32)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "elm-bits"
        [ bitsTest
        ]


bitsTest : Test
bitsTest =
    describe "Bits"
        [ test "padToLength"
            (\() ->
                ArraySized.l4 I O I I
                    |> Bits.padToLength n8
                    |> expectEqualArraySized
                        (ArraySized.l8 O O O O I O I I)
            )
        , describe "toChunksOf"
            [ test "no bits is no bytes"
                (\() ->
                    ArraySized.empty
                        |> Bits.toChunksOf n8
                        |> expectEqualArraySized ArraySized.empty
                )
            , test "4 bits is a 4bit byte"
                (\() ->
                    ArraySized.l4 O I I I
                        |> Bits.toChunksOf n8
                        |> ArraySized.map ArraySized.toList
                        |> expectEqualArraySized
                            (ArraySized.l1
                                (ArraySized.l4 O I I I
                                    |> Bits.padToLength n8
                                    |> ArraySized.toList
                                )
                            )
                )
            , test "27 bits is 3 8bit bytes & a 3bit byte"
                (\() ->
                    ArraySized.l3 I I I
                        |> ArraySized.glue Up
                            (ArraySized.l8 O I I I O I O O)
                        |> ArraySized.glue Up
                            (ArraySized.l8 O I I I O I O O)
                        |> ArraySized.glue Up
                            (ArraySized.l8 O I I I O I O O)
                        |> Bits.toChunksOf n8
                        |> ArraySized.map ArraySized.toList
                        |> expectEqualArraySized
                            (ArraySized.l1 (ArraySized.l3 I I I |> Bits.padToLength n8)
                                |> ArraySized.glue Up
                                    (ArraySized.repeat (ArraySized.l8 O I I I O I O O) n3)
                                |> ArraySized.map ArraySized.toList
                            )
                )
            ]
        , describe "N"
            [ Test.fuzz
                (Fuzz.map
                    (ArraySized.fromArray
                        >> ArraySized.take ( Up, n32 |> N.minTo n0 )
                    )
                    (Fuzz.array Bit.fuzz)
                )
                "toN >> fromN → padToLength n32"
                (\bits ->
                    bits
                        |> Bits.toN
                        |> Bits.fromN
                        |> expectEqualArraySized
                            (bits
                                |> Bits.padToLength n32
                            )
                )
            , Test.describe "toN"
                [ test "138"
                    (\() ->
                        ArraySized.l8 I O O O I O I O
                            |> Bits.toN
                            |> N.toInt
                            |> Expect.equal 138
                    )
                , test "1"
                    (\() ->
                        ArraySized.l8 O O O O O O O I
                            |> ArraySized.maxTo n32
                            |> Bits.toN
                            |> N.toInt
                            |> Expect.equal 1
                    )
                , test "116"
                    (\() ->
                        ArraySized.l8 O I I I O I O O
                            |> ArraySized.maxTo n32
                            |> Bits.toN
                            |> N.toInt
                            |> Expect.equal 116
                    )
                ]
            ]
        , Test.fuzz
            (Fuzz.constant
                (\bits -> { bits = bits })
                |> Fuzz.andMap
                    (Fuzz.map
                        (ArraySized.fromArray
                            >> Bits.atMost n20
                        )
                        (Fuzz.array Bit.fuzz)
                    )
            )
            "toIntSigned >> fromIntSigned <original length> → identity"
            (\{ bits } ->
                bits
                    |> ArraySized.maxTo n20
                    |> Bits.toIntSigned
                    |> Bits.fromIntSigned (bits |> ArraySized.length)
                    |> expectEqualArraySized
                        bits
            )
        ]


expectEqualArraySized :
    ArraySized element length0_
    -> ArraySized element length1_
    -> Expectation
expectEqualArraySized expectedArr actualArr =
    actualArr
        |> ArraySized.toList
        |> Expect.equalLists
            (expectedArr |> ArraySized.toList)
