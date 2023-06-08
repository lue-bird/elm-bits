module Tests exposing (suite)

import ArraySized exposing (ArraySized)
import Bit exposing (Bit(..))
import BitArray
import Expect exposing (Expectation)
import Fuzz
import Linear exposing (Direction(..))
import N exposing (n0, n3, n5, n8)
import N.Local exposing (n31, n32)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "elm-bits"
        [ bitsTest
        ]


bitsTest : Test
bitsTest =
    describe "Bits"
        [ describe "toChunksOf"
            [ test "no bits is no bytes"
                (\() ->
                    ArraySized.empty
                        |> BitArray.toChunksOf n8
                        |> expectEqualArraySized ArraySized.empty
                )
            , test "4 bits is a 4bit byte"
                (\() ->
                    ArraySized.l4 O I I I
                        |> BitArray.toChunksOf n8
                        |> ArraySized.map ArraySized.toList
                        |> expectEqualArraySized
                            (ArraySized.one
                                (ArraySized.l4 O I I I
                                    |> ArraySized.toSize Down n8 (\_ -> O)
                                    |> ArraySized.toList
                                )
                            )
                )
            , test "27 bits is 3 8bit bytes & a 3bit byte"
                (\() ->
                    ArraySized.l3 I I I
                        |> ArraySized.attach Up
                            (ArraySized.l8 O I I I O I O O)
                        |> ArraySized.attach Up
                            (ArraySized.l8 O I I I O I O O)
                        |> ArraySized.attach Up
                            (ArraySized.l8 O I I I O I O O)
                        |> BitArray.toChunksOf n8
                        |> ArraySized.map ArraySized.toList
                        |> expectEqualArraySized
                            (ArraySized.one
                                (ArraySized.l3 I I I
                                    |> ArraySized.toSize Down n8 (\_ -> O)
                                )
                                |> ArraySized.attach Up
                                    (ArraySized.repeat (ArraySized.l8 O I I I O I O O) n3)
                                |> ArraySized.map ArraySized.toList
                            )
                )
            ]
        , Test.describe "toN"
            [ test "138"
                (\() ->
                    ArraySized.l8 I O O O I O I O
                        |> BitArray.toN
                        |> N.toInt
                        |> Expect.equal 138
                )
            , test "1"
                (\() ->
                    ArraySized.l8 O O O O O O O I
                        |> ArraySized.maxTo n32
                        |> BitArray.toN
                        |> N.toInt
                        |> Expect.equal 1
                )
            , test "116"
                (\() ->
                    ArraySized.l8 O I I I O I O O
                        |> ArraySized.maxTo n32
                        |> BitArray.toN
                        |> N.toInt
                        |> Expect.equal 116
                )
            ]
        , Test.fuzz (N.inFuzzUniform ( n0, n31 ))
            "a |> fromN |> toN == a"
            (\n ->
                n
                    |> BitArray.fromN n5
                    |> BitArray.toN
                    |> N.toInt
                    |> Expect.equal (n |> N.toInt)
            )
        , Test.describe "fromInt"
            [ test "-73"
                (\() ->
                    -73
                        |> BitArray.fromIntSigned n8
                        |> expectEqualArraySized
                            (ArraySized.l8 I O I I O I I I)
                )
            , test "-128"
                (\() ->
                    -128
                        |> BitArray.fromIntSigned n8
                        |> expectEqualArraySized
                            (ArraySized.l8 I O O O O O O O)
                )
            , Test.fuzz (Fuzz.intRange 0 127)
                "natural"
                (\naturalInt ->
                    naturalInt
                        |> BitArray.fromIntSigned n8
                        |> expectEqualArraySized
                            (naturalInt |> N.intToAtLeast n0 |> BitArray.fromN n8)
                )
            ]
        , Test.fuzz (Fuzz.intRange -128 127)
            "a |> fromInt |> toInt == a"
            (\naturalInt ->
                naturalInt
                    |> BitArray.fromIntSigned n8
                    |> BitArray.toIntSigned
                    |> Expect.equal naturalInt
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
