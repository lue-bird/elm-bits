module Bits.Represent exposing
    ( as01String
    , as09avChar
    , as09avString
    , asColor
    , asHexChar
    , asHexString
    , asReadableWord
    , asReadableWordsString
    , asRecognizableCollage
    , asShape
    , asShortUnicodeString
    )

{-| Ideas on how to represent `Bit`s.
-}

import Arr exposing (Arr)
import ArrExtra as Arr
import Array
import Collage exposing (Collage)
import Collage.Layout
import Color exposing (Color)
import LinearDirection exposing (LinearDirection(..))
import Lue.Bit as Bit exposing (Bit(..))
import Lue.Bits as Bits
import NNats exposing (..)
import Nat exposing (In)
import Toop
import TypeNats exposing (..)
import Typed exposing (val)


{-| Convert to a string: `O`s are shown as `0`, `1`s as `I`s.

    as01String (Arr.from4 I O I O)
    --> "1010"

-}
as01String : Arr (In min_ max_) Bit -> String
as01String bits =
    Arr.map (Bit.toNat >> val >> String.fromInt) bits
        |> Arr.fold FirstToLast (++) ""


{-| Convert a `List` of `Bit`s to a

  - short
  - Not human readable
  - Not recognizable

`String` from unicode `Char`s.

-}
asShortUnicodeString : Arr (In min_ max_) Bit -> String
asShortUnicodeString bits =
    Arr.groupPaddingLeft nat20 O bits
        |> Arr.map
            (Bits.toNat >> val >> Char.fromCode)
        |> Arr.toArray
        |> Array.toList
        |> String.fromList


{-| Four bits represented as a hex `Char` (0-9 then a-f).
-}
asHexChar : Arr (In min_ Nat4) Bit -> Char
asHexChar hexBits =
    let
        paddedHexBits =
            hexBits |> Arr.resize LastToFirst nat4 O

        at index =
            Arr.at index FirstToLast paddedHexBits
    in
    case Toop.T4 (at nat0) (at nat1) (at nat2) (at nat3) of
        Toop.T4 O O O O ->
            '0'

        Toop.T4 O O O I ->
            '1'

        Toop.T4 O O I O ->
            '2'

        Toop.T4 O O I I ->
            '3'

        Toop.T4 O I O O ->
            '4'

        Toop.T4 O I O I ->
            '5'

        Toop.T4 O I I O ->
            '6'

        Toop.T4 O I I I ->
            '7'

        Toop.T4 I O O O ->
            '8'

        Toop.T4 I O O I ->
            '9'

        Toop.T4 I O I O ->
            'a'

        Toop.T4 I O I I ->
            'b'

        Toop.T4 I I O O ->
            'c'

        Toop.T4 I I O I ->
            'd'

        Toop.T4 I I I O ->
            'e'

        Toop.T4 I I I I ->
            'f'


asHexString : Arr (In min_ max_) Bit -> String
asHexString bits =
    Arr.groupPaddingLeft nat4 O bits
        |> Arr.map asHexChar
        |> Arr.toArray
        |> Array.toList
        |> String.fromList


as09avChar : Arr (In min_ Nat5) Bit -> Char
as09avChar bits =
    let
        paddedBits =
            bits |> Arr.resize LastToFirst nat4 O

        at index =
            Arr.at index FirstToLast paddedBits
    in
    case at nat0 of
        O ->
            asHexChar (Arr.take nat4 FirstToLast paddedBits)

        I ->
            case Toop.T4 (at nat0) (at nat1) (at nat2) (at nat3) of
                Toop.T4 O O O O ->
                    'g'

                Toop.T4 O O O I ->
                    'h'

                Toop.T4 O O I O ->
                    'i'

                Toop.T4 O O I I ->
                    'j'

                Toop.T4 O I O O ->
                    'k'

                Toop.T4 O I O I ->
                    'l'

                Toop.T4 O I I O ->
                    'm'

                Toop.T4 O I I I ->
                    'n'

                Toop.T4 I O O O ->
                    'o'

                Toop.T4 I O O I ->
                    'p'

                Toop.T4 I O I O ->
                    'q'

                Toop.T4 I O I I ->
                    'r'

                Toop.T4 I I O O ->
                    's'

                Toop.T4 I I O I ->
                    't'

                Toop.T4 I I I O ->
                    'u'

                Toop.T4 I I I I ->
                    'v'


as09avString : Arr (In min_ max_) Bit -> String
as09avString bits =
    Arr.groupPaddingLeft nat5 O bits
        |> Arr.map as09avChar
        |> Arr.toArray
        |> Array.toList
        |> String.fromList


{-| Four bits represented in a `Char` of multiple uniquely identifiable symbols
-}
asFirstLetterInWord : Arr (In min_ Nat4) Bit -> Char
asFirstLetterInWord bits =
    let
        paddedBits =
            bits |> Arr.resize LastToFirst nat4 O

        at index =
            Arr.at index FirstToLast paddedBits
    in
    case Toop.T4 (at nat0) (at nat1) (at nat2) (at nat3) of
        Toop.T4 O O O O ->
            'b'

        Toop.T4 O O O I ->
            'd'

        Toop.T4 O O I O ->
            'f'

        Toop.T4 O O I I ->
            'g'

        Toop.T4 O I O O ->
            'h'

        Toop.T4 O I O I ->
            'k'

        Toop.T4 O I I O ->
            'l'

        Toop.T4 O I I I ->
            'm'

        Toop.T4 I O O O ->
            'n'

        Toop.T4 I O O I ->
            'p'

        Toop.T4 I O I O ->
            'r'

        Toop.T4 I O I I ->
            's'

        Toop.T4 I I O O ->
            't'

        Toop.T4 I I O I ->
            'w'

        Toop.T4 I I I O ->
            'x'

        Toop.T4 I I I I ->
            'y'


asThirdLetterInWord : Arr (In min_ Nat4) Bit -> Char
asThirdLetterInWord bits =
    let
        paddedBits =
            bits |> Arr.resize LastToFirst nat4 O

        at index =
            Arr.at index FirstToLast paddedBits
    in
    case Toop.T4 (at nat0) (at nat1) (at nat2) (at nat3) of
        Toop.T4 O O O O ->
            'b'

        Toop.T4 O O O I ->
            'd'

        Toop.T4 O O I O ->
            'f'

        Toop.T4 O O I I ->
            'g'

        Toop.T4 O I O O ->
            'k'

        Toop.T4 O I O I ->
            'l'

        Toop.T4 O I I O ->
            'm'

        Toop.T4 O I I I ->
            'n'

        Toop.T4 I O O O ->
            'p'

        Toop.T4 I O O I ->
            'r'

        Toop.T4 I O I O ->
            's'

        Toop.T4 I O I I ->
            't'

        Toop.T4 I I O O ->
            'w'

        Toop.T4 I I O I ->
            'x'

        Toop.T4 I I I O ->
            'y'

        Toop.T4 I I I I ->
            ','


asVocal : Arr (In min_ Nat2) Bit -> Char
asVocal bits =
    let
        paddedBits =
            bits |> Arr.resize LastToFirst nat2 O

        at index =
            Arr.at index FirstToLast paddedBits
    in
    case ( at nat0, at nat1 ) of
        ( O, O ) ->
            'a'

        ( O, I ) ->
            'i'

        ( I, O ) ->
            'o'

        ( I, I ) ->
            'u'


asReadableWord : Arr (In min_ Nat10) Bit -> String
asReadableWord bits =
    let
        paddedBits =
            bits |> Arr.resize LastToFirst nat10 O

        at index =
            Arr.at index FirstToLast paddedBits
    in
    String.fromList
        [ asFirstLetterInWord
            (Arr.take nat4 FirstToLast paddedBits)
        , asVocal (Arr.from2 (at nat4) (at nat5))
        , asThirdLetterInWord (Arr.take nat4 LastToFirst paddedBits)
        ]


asReadableWordsString : Arr (In min max) Bit -> String
asReadableWordsString bits =
    Arr.groupPaddingLeft nat10 O bits
        |> Arr.map asReadableWord
        |> Arr.toArray
        |> Array.toList
        |> String.join " "


asColor : Arr (In min_ Nat6) Bit -> Color
asColor bits =
    let
        paddedBits =
            bits |> Arr.resize LastToFirst nat6 O

        at index =
            Arr.at index FirstToLast paddedBits

        component componentBits =
            1 / 8 + (Bits.toNat componentBits |> val |> toFloat) / 4
    in
    Color.rgb
        (component (Arr.from2 (at nat0) (at nat1)))
        (component (Arr.from2 (at nat2) (at nat3)))
        (component (Arr.from2 (at nat4) (at nat5)))


asShape : Arr (In min_ Nat3) Bit -> Collage.Shape
asShape bits =
    let
        paddedBits =
            bits |> Arr.resize LastToFirst nat3 O

        at index =
            Arr.at index FirstToLast paddedBits
    in
    case ( at nat0, at nat1, at nat2 ) of
        ( O, O, O ) ->
            Collage.circle 1

        ( O, O, I ) ->
            Collage.rectangle 0.5 1.5

        ( O, I, O ) ->
            Collage.rectangle 1.5 0.5

        _ ->
            {- 3 to 8 -}
            Collage.ngon
                (Bits.toNat paddedBits |> val)
                1


asCollage : Arr (In min_ Nat11) Bit -> Collage msg
asCollage bits =
    let
        paddedBits =
            bits |> Arr.resize LastToFirst nat11 O

        at index =
            Arr.at index FirstToLast paddedBits

        colorShape =
            case ( at nat3, at nat4 ) of
                ( O, O ) ->
                    Collage.filled

                ( O, I ) ->
                    Collage.outlined << Collage.solid 0.5

                ( I, O ) ->
                    Collage.outlined << Collage.dash 0.5

                ( I, I ) ->
                    Collage.outlined << Collage.dot 0.5
    in
    colorShape
        (Collage.uniform
            (asColor (Arr.take nat6 LastToFirst paddedBits))
        )
        (asShape (Arr.take nat3 FirstToLast paddedBits))


asRecognizableCollage : Arr (In min_ max_) Bit -> Collage msg_
asRecognizableCollage bits =
    let
        collages =
            Arr.groupPaddingLeft nat11 O bits
                |> Arr.map asCollage
                |> Arr.toArray
                |> Array.toList
    in
    collages
        |> List.indexedMap
            (\i collage ->
                let
                    collageCount =
                        List.length collages

                    part =
                        toFloat i / toFloat collageCount

                    rotation =
                        turns part

                    scale =
                        32 / (toFloat collageCount ^ 0.8)

                    radius =
                        16
                in
                collage
                    |> Collage.shiftX (radius * cos rotation)
                    |> Collage.shiftY (radius * sin rotation)
                    |> Collage.rotate rotation
                    |> Collage.scale scale
            )
        |> Collage.Layout.stack
