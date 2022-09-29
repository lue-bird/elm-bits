module Bit.Convert exposing (fromChar, toChar)

import Bit exposing (Bit(..))


toChar : Bit -> Char
toChar =
    \bit ->
        case bit of
            O ->
                '0'

            I ->
                '1'


fromChar : Char -> Result Char Bit
fromChar =
    \char ->
        case char of
            '0' ->
                O |> Ok

            '1' ->
                I |> Ok

            non01 ->
                non01 |> Err
