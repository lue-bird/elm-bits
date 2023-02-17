module Zombie exposing (bytesDecode, bytesEncode)

{-| Stuff that didn't make it into the published package

@docs bytesDecode, bytesEncode

-}

-- bytes

import Bytes
import Bytes.Decode
import Bytes.Encode


{-| [`Bytes.Encode.Encoder`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/Bytes-Encode#Encoder)
from an `ArraySized` of arbitrary length
-}
bytesEncode :
    Bytes.Endianness
    ->
        (ArraySized (In min_ (Up maxX_ To maxPlusX_)) Bit
         -> Bytes.Encode.Encoder
        )
bytesEncode endianness =
    \bits ->
        Bytes.Encode.sequence
            ((bits
                |> ArraySized.length
                |> N.divideBy n8
                |> N.toInt
                |> (case
                        bits
                            |> ArraySized.length
                            |> N.remainderBy n8
                            |> N.isAtLeast n1
                    of
                        Err _ ->
                            identity

                        Ok _ ->
                            \n -> n + 1
                   )
                |> Bytes.Encode.unsignedInt32 endianness
             )
                :: (let
                        chunked =
                            bits
                                |> ArraySized.maxUp n1
                                |> ArraySized.toChunksOf Down n8
                    in
                    chunked.chunks
                        |> ArraySized.insert ( Up, n0 )
                            (chunked.remainder
                                |> padToAtLeast n8
                            )
                        |> ArraySized.map
                            (\byte ->
                                byte
                                    |> toN
                                    |> N.toInt
                                    |> Bytes.Encode.unsignedInt8
                            )
                        |> ArraySized.toList
                   )
            )


{-| [`Bytes.Encode.Decoder`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/Bytes-Decode#Decoder)
of an `ArraySized` of arbitrary length
-}
bytesDecode :
    Bytes.Endianness
    -> Bytes.Decode.Decoder (ArraySized (Min (Up0 x_)) Bit)
bytesDecode endianness =
    Bytes.Decode.unsignedInt32 endianness
        |> Bytes.Decode.andThen
            (\lengthInt ->
                Bytes.Decode.loop
                    { remainingByteCount = lengthInt |> N.intToAtLeast n0
                    , soFar = []
                    }
                    arraySizedOfLengthBytesDecodeStep
            )


arraySizedOfLengthBytesDecodeStep :
    { remainingByteCount : N (In remainingByteCountMin_ remainingByteCountMax)
    , soFar : List Bit
    }
    ->
        Bytes.Decode.Decoder
            (Bytes.Decode.Step
                { remainingByteCount :
                    N
                        (In
                            (Up remainingByteCountAfterX To remainingByteCountAfterX)
                            remainingByteCountMax
                        )
                , soFar : List Bit
                }
                (ArraySized (Min (Up0 x_)) Bit)
            )
arraySizedOfLengthBytesDecodeStep =
    \{ remainingByteCount, soFar } ->
        case remainingByteCount |> N.isAtLeast n1 of
            Err _ ->
                soFar
                    |> ArraySized.fromList
                    |> Bytes.Decode.Done
                    |> Bytes.Decode.succeed

            Ok remainingByteCountAtLeast1 ->
                Bytes.Decode.map
                    (\byte ->
                        { remainingByteCount =
                            remainingByteCountAtLeast1
                                |> N.subtractMin n1
                                |> N.minTo n0
                        , soFar =
                            soFar
                                ++ (byte
                                        |> N.intToAtLeast n0
                                        |> fromN
                                        |> padToAtLeast n32
                                        |> ArraySized.dropOverMin ( Down, n8 )
                                        |> ArraySized.toList
                                   )
                        }
                            |> Bytes.Decode.Loop
                    )
                    Bytes.Decode.unsignedInt8
