module Bit.Convert exposing (fromChar, toChar)

import Bit exposing (Bit)


toChar : Bit -> Char
toChar =
    \bit ->
        case bit of
            Bit.O ->
                '0'

            Bit.I ->
                '1'


fromChar : Char -> Result Char Bit
fromChar =
    \char ->
        case char of
            '0' ->
                Bit.O |> Ok

            '1' ->
                Bit.I |> Ok

            non01 ->
                non01 |> Err
