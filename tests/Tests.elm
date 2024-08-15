module Tests exposing (suite)

import Bit
import Bits
import Expect
import Fuzz
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Bits"
        [ Test.describe "toIntUnsigned"
            [ Test.test "138"
                (\() ->
                    [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O ]
                        |> Bits.toIntUnsigned
                        |> Expect.equal 138
                )
            , Test.test "1"
                (\() ->
                    [ Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I ]
                        |> Bits.toIntUnsigned
                        |> Expect.equal 1
                )
            , Test.test "116"
                (\() ->
                    [ Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]
                        |> Bits.toIntUnsigned
                        |> Expect.equal 116
                )
            ]
        , Test.fuzz (Fuzz.uniformInt 31)
            "a |> fromIntUnsigned |> toIntUnsigned == a, 5 bit"
            (\n ->
                n
                    |> Bits.fromIntUnsigned 5
                    |> Bits.toIntUnsigned
                    |> Expect.equal n
            )
        , Test.fuzz (Fuzz.uniformInt (2 ^ 32 - 1))
            "a |> fromIntUnsigned |> toIntUnsigned == a, 32 bit"
            (\n ->
                n
                    |> Bits.fromIntUnsigned 32
                    |> Bits.toIntUnsigned
                    |> Expect.equal n
            )
        , Test.describe "fromIntSigned"
            [ Test.test "-73"
                (\() ->
                    -73
                        |> Bits.fromIntSigned 8
                        |> Expect.equalLists
                            [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]
                )
            , Test.test "-128"
                (\() ->
                    -128
                        |> Bits.fromIntSigned 8
                        |> Expect.equalLists
                            [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O ]
                )
            , Test.fuzz (Fuzz.uniformInt 127)
                "Bits.fromIntSigned = Bit.O :: Bits.fromIntUnsigned, base 7"
                (\naturalInt ->
                    naturalInt
                        |> Bits.fromIntSigned 8
                        |> Expect.equalLists
                            (Bit.O :: (naturalInt |> Bits.fromIntUnsigned 7))
                )
            , Test.fuzz (Fuzz.uniformInt (2 ^ 32 - 1))
                "Bits.fromIntSigned = Bit.O :: Bits.fromIntUnsigned, base 32"
                (\naturalInt ->
                    naturalInt
                        |> Bits.fromIntSigned 32
                        |> Expect.equalLists
                            (Bit.O :: (naturalInt |> Bits.fromIntUnsigned 31))
                )
            ]
        , Test.fuzz (Fuzz.intRange -128 127)
            "a |> fromIntSigned |> toIntSigned == a, base 8"
            (\naturalInt ->
                naturalInt
                    |> Bits.fromIntSigned 8
                    |> Bits.toIntSigned
                    |> Expect.equal naturalInt
            )
        , Test.fuzz (Fuzz.intRange (-2 ^ 31) (2 ^ 31 - 1))
            "a |> fromIntSigned |> toIntSigned == a, base 32"
            (\naturalInt ->
                naturalInt
                    |> Bits.fromIntSigned 32
                    |> Bits.toIntSigned
                    |> Expect.equal naturalInt
            )
        , Test.fuzz (Fuzz.list (Fuzz.intRange 0 255))
            "ints |> List.concatMap (Bits.fromIntUnsigned 8) |> Bits.toIntUnsigned8s = ints"
            (\ints ->
                ints
                    |> List.concatMap (Bits.fromIntUnsigned 8)
                    |> Bits.toIntUnsigned8s
                    |> Expect.equalLists ints
            )
        , Test.fuzz (Fuzz.list (Fuzz.intRange 0 (2 ^ 16 - 1)))
            "ints |> List.concatMap (Bits.fromIntUnsigned 16) |> Bits.toIntUnsigned16s = ints"
            (\ints ->
                ints
                    |> List.concatMap (Bits.fromIntUnsigned 16)
                    |> Bits.toIntUnsigned16s
                    |> Expect.equalLists ints
            )
        , Test.fuzz (Fuzz.list (Fuzz.intRange 0 (2 ^ 32 - 1)))
            "ints |> List.concatMap (Bits.fromIntUnsigned 32) |> Bits.toIntUnsigned32s = ints"
            (\ints ->
                ints
                    |> List.concatMap (Bits.fromIntUnsigned 32)
                    |> Bits.toIntUnsigned32s
                    |> Expect.equalLists ints
            )
        , Test.fuzz
            (Fuzz.intRange 0 20
                |> Fuzz.map (\n -> n * 8 + 6)
                |> Fuzz.andThen
                    (\bitCount ->
                        Fuzz.listOfLength bitCount
                            (Fuzz.oneOfValues [ Bit.O, Bit.I ])
                    )
            )
            "Bits.toIntUnsigned8s pads right"
            (\bits ->
                bits
                    |> Bits.toIntUnsigned8s
                    |> Expect.equalLists
                        (bits ++ [ Bit.O, Bit.O ] |> Bits.toIntUnsigned8s)
            )
        , Test.fuzz
            (Fuzz.intRange 0 20
                |> Fuzz.map (\n -> n * 16 + 6)
                |> Fuzz.andThen
                    (\bitCount ->
                        Fuzz.listOfLength bitCount
                            (Fuzz.oneOfValues [ Bit.O, Bit.I ])
                    )
            )
            "Bits.toIntUnsigned16s pads right"
            (\bits ->
                bits
                    |> Bits.toIntUnsigned16s
                    |> Expect.equalLists
                        (bits ++ [ Bit.O, Bit.O ] |> Bits.toIntUnsigned16s)
            )
        , Test.fuzz
            (Fuzz.intRange 0 20
                |> Fuzz.map (\n -> n * 8 + 6)
                |> Fuzz.andThen
                    (\bitCount ->
                        Fuzz.listOfLength bitCount
                            (Fuzz.oneOfValues [ Bit.O, Bit.I ])
                    )
            )
            "Bits.toIntUnsigned32s pads right"
            (\bits ->
                bits
                    |> Bits.toIntUnsigned32s
                    |> Expect.equalLists
                        (bits ++ [ Bit.O, Bit.O ] |> Bits.toIntUnsigned32s)
            )
        ]
