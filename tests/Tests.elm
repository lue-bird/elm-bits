module Tests exposing (suite)

import Arr exposing (Arr)
import Expect exposing (Expectation)
import InArr
import Lue.Bit exposing (Bit(..))
import Lue.Bits as Bits
import Nat exposing (In)
import Nats exposing (..)
import Test exposing (Test, describe, test)
import Typed exposing (val)


byte116 : Arr (In Nat8 (Nat8Plus a_)) Bit
byte116 =
    Arr.from7 I I I O I O O |> Bits.padToByte


byte138 : Arr (In Nat8 (Nat8Plus a_)) Bit
byte138 =
    Arr.from8 I O O O I O I O


suite : Test
suite =
    describe "elm-bits package"
        [ byteTest
        , bytesTest
        , bitsTest
        , test "remainderBy 0 doesn't crash elm"
            (\() ->
                isNaN (toFloat (remainderBy 0 10))
                    |> Expect.equal True
            )
        ]


byteTest : Test
byteTest =
    describe "byte"
        [ test "pad 0s to byte"
            (\() ->
                Arr.from4 I O I I
                    |> Bits.padToByte
                    |> expectEqualArrs
                        (Arr.from8 O O O O I O I I)
            )
        , test "toNat"
            (\() ->
                Expect.all
                    [ \() ->
                        Bits.toNat (Arr.from8 O O O O O O O I)
                            |> val
                            |> Expect.equal 1
                    , \() ->
                        Bits.toNat byte138
                            |> val
                            |> Expect.equal 138
                    , \() ->
                        Bits.toNat byte116
                            |> val
                            |> Expect.equal 116
                    ]
                    ()
            )
        ]


bytesTest : Test
bytesTest =
    describe "bytes"
        [ describe "fromBits"
            [ test "no bits is no bytes"
                (\() ->
                    Bits.toBytes Arr.empty
                        |> expectEqualArrs Arr.empty
                )
            , test "4 bits is a 4bit byte"
                (\() ->
                    Bits.toBytes (Arr.from4 O I I I)
                        |> Arr.map Arr.toList
                        |> expectEqualArrs
                            (Arr.from1
                                (Arr.from4 O I I I
                                    |> Bits.padToByte
                                    |> Arr.toList
                                )
                            )
                )
            , test "27 bits is 3 8bit bytes & a 3bit byte"
                (\() ->
                    Bits.toBytes
                        (Arr.from3 I I I
                            |> InArr.append nat24
                                (Arr.from8 O I I I O I O O
                                    |> InArr.append nat8
                                        (Arr.from8 O I I I O I O O)
                                    |> InArr.append nat8
                                        (Arr.from8 O I I I O I O O)
                                )
                        )
                        |> Arr.map Arr.toList
                        |> expectEqualArrs
                            (Arr.from1 (Arr.from3 I I I |> Bits.padToByte)
                                |> InArr.append nat3
                                    (Arr.repeat nat3 (Arr.from8 O I I I O I O O))
                                |> Arr.map Arr.toList
                            )
                )
            ]
        ]


bitsTest : Test
bitsTest =
    describe "bits"
        [ describe "toNat"
            [ test "gets translated correctly"
                (\() ->
                    Bits.toNat byte138
                        |> val
                        |> Expect.equal 138
                )
            ]
        ]


expectEqualArrs :
    Arr length0_ element
    -> Arr length1_ element
    -> Expectation
expectEqualArrs expectedArr actualArr =
    actualArr
        |> Arr.toList
        |> Expect.equalLists
            (expectedArr |> Arr.toList)
