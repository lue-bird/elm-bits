module Bits.Convert exposing
    ( to01String
    , to09avString
    , toHexString
    , toProquintSequence
    , toReadableWordsString
    , toRecognizableCollage
    , toUnicodeString
    )

{-| Ideas on how to represent `Bit`s
-}

import Bit exposing (Bit)
import Bit.Convert
import Bits
import Collage exposing (Collage)
import Collage.Layout
import Color exposing (Color)
import List
import Proquint
import Toop


{-| Convert to a string: `O`s are shown as `0`, `1`s as `I`s.

    as01String (List.l4 I O I O)
    --> "1010"

-}
to01String : List Bit -> String
to01String =
    \bits ->
        bits
            |> List.map Bit.Convert.toChar
            |> String.fromList


listToChunksOf : Int -> List a -> List (List a)
listToChunksOf chunkSize list =
    listToChunksOfReverseOnto [] chunkSize list
        |> List.reverse


listToChunksOfReverseOnto : List (List a) -> Int -> List a -> List (List a)
listToChunksOfReverseOnto soFar chunkSize list =
    case List.drop chunkSize list of
        [] ->
            case list of
                [] ->
                    soFar

                head :: tail ->
                    (head :: tail) :: soFar

        remainingAfterChunkHead :: remainingAfterChunkTail ->
            listToChunksOfReverseOnto (List.take chunkSize list :: soFar)
                chunkSize
                (remainingAfterChunkHead :: remainingAfterChunkTail)


{-| Convert a `List` of `Bit`s to a

  - short
  - not human readable
  - not recognizable

`String` from unicode `Char`s.

-}
toUnicodeString : List Bit -> String
toUnicodeString =
    \bits ->
        bits
            |> listToChunksOf 20
            |> List.map
                (\bits20 ->
                    bits20
                        |> Bits.toIntUnsigned
                        |> Char.fromCode
                )
            |> String.fromList


toProquintSequence : List Bit -> String
toProquintSequence =
    \bits ->
        bits
            |> listToChunksOf 32
            |> List.map
                (\chunk ->
                    case chunk |> Bits.toIntUnsigned |> Proquint.fromInt of
                        Nothing ->
                            "ERROR using Proquint.fromInt"

                        Just proquint ->
                            proquint |> Proquint.toString
                )
            |> String.join ", "


{-| Four bits represented as a hex `Char` (0-9 then a-f)
-}
toHexChar : Toop.T4 Bit Bit Bit Bit -> Char
toHexChar =
    \bits ->
        case bits of
            Toop.T4 Bit.O Bit.O Bit.O Bit.O ->
                '0'

            Toop.T4 Bit.O Bit.O Bit.O Bit.I ->
                '1'

            Toop.T4 Bit.O Bit.O Bit.I Bit.O ->
                '2'

            Toop.T4 Bit.O Bit.O Bit.I Bit.I ->
                '3'

            Toop.T4 Bit.O Bit.I Bit.O Bit.O ->
                '4'

            Toop.T4 Bit.O Bit.I Bit.O Bit.I ->
                '5'

            Toop.T4 Bit.O Bit.I Bit.I Bit.O ->
                '6'

            Toop.T4 Bit.O Bit.I Bit.I Bit.I ->
                '7'

            Toop.T4 Bit.I Bit.O Bit.O Bit.O ->
                '8'

            Toop.T4 Bit.I Bit.O Bit.O Bit.I ->
                '9'

            Toop.T4 Bit.I Bit.O Bit.I Bit.O ->
                'a'

            Toop.T4 Bit.I Bit.O Bit.I Bit.I ->
                'b'

            Toop.T4 Bit.I Bit.I Bit.O Bit.O ->
                'c'

            Toop.T4 Bit.I Bit.I Bit.O Bit.I ->
                'd'

            Toop.T4 Bit.I Bit.I Bit.I Bit.O ->
                'e'

            Toop.T4 Bit.I Bit.I Bit.I Bit.I ->
                'f'


toHexString : List Bit -> String
toHexString =
    \bits ->
        bits |> bitsToChunksOf4 |> List.map toHexChar |> String.fromList


bitsToChunksOf4 : List Bit -> List (Toop.T4 Bit Bit Bit Bit)
bitsToChunksOf4 =
    \bits ->
        case bits of
            [] ->
                []

            bit0 :: [] ->
                List.singleton (Toop.T4 Bit.O Bit.O Bit.O bit0)

            bit0 :: bit1 :: [] ->
                List.singleton (Toop.T4 Bit.O Bit.O bit0 bit1)

            bit0 :: bit1 :: bit2 :: [] ->
                List.singleton (Toop.T4 Bit.O bit0 bit1 bit2)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4Up ->
                Toop.T4 bit0 bit1 bit2 bit3 :: bitsToChunksOf4 bit4Up


to09avString : List Bit -> String
to09avString =
    \bits ->
        bits
            |> bitsToChunksOf5
            |> List.map to09avChar
            |> String.fromList


to09avChar : Toop.T5 Bit Bit Bit Bit Bit -> Char
to09avChar =
    \(Toop.T5 bit0 bit1 bit2 bit3 bit4) ->
        case bit0 of
            Bit.O ->
                Toop.T4 bit1 bit2 bit3 bit4 |> toHexChar

            Bit.I ->
                case Toop.T4 bit1 bit2 bit3 bit4 of
                    Toop.T4 Bit.O Bit.O Bit.O Bit.O ->
                        'g'

                    Toop.T4 Bit.O Bit.O Bit.O Bit.I ->
                        'h'

                    Toop.T4 Bit.O Bit.O Bit.I Bit.O ->
                        'i'

                    Toop.T4 Bit.O Bit.O Bit.I Bit.I ->
                        'j'

                    Toop.T4 Bit.O Bit.I Bit.O Bit.O ->
                        'k'

                    Toop.T4 Bit.O Bit.I Bit.O Bit.I ->
                        'l'

                    Toop.T4 Bit.O Bit.I Bit.I Bit.O ->
                        'm'

                    Toop.T4 Bit.O Bit.I Bit.I Bit.I ->
                        'n'

                    Toop.T4 Bit.I Bit.O Bit.O Bit.O ->
                        'o'

                    Toop.T4 Bit.I Bit.O Bit.O Bit.I ->
                        'p'

                    Toop.T4 Bit.I Bit.O Bit.I Bit.O ->
                        'q'

                    Toop.T4 Bit.I Bit.O Bit.I Bit.I ->
                        'r'

                    Toop.T4 Bit.I Bit.I Bit.O Bit.O ->
                        's'

                    Toop.T4 Bit.I Bit.I Bit.O Bit.I ->
                        't'

                    Toop.T4 Bit.I Bit.I Bit.I Bit.O ->
                        'u'

                    Toop.T4 Bit.I Bit.I Bit.I Bit.I ->
                        'v'


bitsToChunksOf5 : List Bit -> List (Toop.T5 Bit Bit Bit Bit Bit)
bitsToChunksOf5 =
    \bits ->
        case bits of
            [] ->
                []

            bit0 :: [] ->
                List.singleton (Toop.T5 Bit.O Bit.O Bit.O Bit.O bit0)

            bit0 :: bit1 :: [] ->
                List.singleton (Toop.T5 Bit.O Bit.O Bit.O bit0 bit1)

            bit0 :: bit1 :: bit2 :: [] ->
                List.singleton (Toop.T5 Bit.O Bit.O bit0 bit1 bit2)

            bit0 :: bit1 :: bit2 :: bit3 :: [] ->
                List.singleton (Toop.T5 Bit.O bit0 bit1 bit2 bit3)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5Up ->
                Toop.T5 bit0 bit1 bit2 bit3 bit4 :: bitsToChunksOf5 bit5Up


toReadableWordsString : List Bit -> String
toReadableWordsString =
    \bits ->
        bits
            |> toReadableWords
            |> String.join " "


toReadableWords : List Bit -> List String
toReadableWords =
    \bits ->
        bits
            |> bitsToChunksOf10
            |> List.map toReadableWord


toReadableWord : Toop.T10 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit -> String
toReadableWord =
    \(Toop.T10 bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8 bit9) ->
        [ Toop.T4 bit0 bit1 bit2 bit3
            |> asFirstLetterInWord
        , Toop.T2 bit4 bit5
            |> asVocal
        , Toop.T4 bit6 bit7 bit8 bit9
            |> asThirdLetterInWord
        ]
            |> String.fromList


{-| Four bits represented in a `Char` of multiple uniquely identifiable symbols
-}
asFirstLetterInWord : Toop.T4 Bit Bit Bit Bit -> Char
asFirstLetterInWord =
    \bits ->
        case bits of
            Toop.T4 Bit.O Bit.O Bit.O Bit.O ->
                'b'

            Toop.T4 Bit.O Bit.O Bit.O Bit.I ->
                'd'

            Toop.T4 Bit.O Bit.O Bit.I Bit.O ->
                'f'

            Toop.T4 Bit.O Bit.O Bit.I Bit.I ->
                'g'

            Toop.T4 Bit.O Bit.I Bit.O Bit.O ->
                'h'

            Toop.T4 Bit.O Bit.I Bit.O Bit.I ->
                'k'

            Toop.T4 Bit.O Bit.I Bit.I Bit.O ->
                'l'

            Toop.T4 Bit.O Bit.I Bit.I Bit.I ->
                'm'

            Toop.T4 Bit.I Bit.O Bit.O Bit.O ->
                'n'

            Toop.T4 Bit.I Bit.O Bit.O Bit.I ->
                'p'

            Toop.T4 Bit.I Bit.O Bit.I Bit.O ->
                'r'

            Toop.T4 Bit.I Bit.O Bit.I Bit.I ->
                's'

            Toop.T4 Bit.I Bit.I Bit.O Bit.O ->
                't'

            Toop.T4 Bit.I Bit.I Bit.O Bit.I ->
                'w'

            Toop.T4 Bit.I Bit.I Bit.I Bit.O ->
                'x'

            Toop.T4 Bit.I Bit.I Bit.I Bit.I ->
                'y'


asThirdLetterInWord : Toop.T4 Bit Bit Bit Bit -> Char
asThirdLetterInWord bits =
    case bits of
        Toop.T4 Bit.O Bit.O Bit.O Bit.O ->
            'b'

        Toop.T4 Bit.O Bit.O Bit.O Bit.I ->
            'd'

        Toop.T4 Bit.O Bit.O Bit.I Bit.O ->
            'f'

        Toop.T4 Bit.O Bit.O Bit.I Bit.I ->
            'g'

        Toop.T4 Bit.O Bit.I Bit.O Bit.O ->
            'k'

        Toop.T4 Bit.O Bit.I Bit.O Bit.I ->
            'l'

        Toop.T4 Bit.O Bit.I Bit.I Bit.O ->
            'm'

        Toop.T4 Bit.O Bit.I Bit.I Bit.I ->
            'n'

        Toop.T4 Bit.I Bit.O Bit.O Bit.O ->
            'p'

        Toop.T4 Bit.I Bit.O Bit.O Bit.I ->
            'r'

        Toop.T4 Bit.I Bit.O Bit.I Bit.O ->
            's'

        Toop.T4 Bit.I Bit.O Bit.I Bit.I ->
            't'

        Toop.T4 Bit.I Bit.I Bit.O Bit.O ->
            'w'

        Toop.T4 Bit.I Bit.I Bit.O Bit.I ->
            'x'

        Toop.T4 Bit.I Bit.I Bit.I Bit.O ->
            'y'

        Toop.T4 Bit.I Bit.I Bit.I Bit.I ->
            ','


asVocal : Toop.T2 Bit Bit -> Char
asVocal bits =
    case bits of
        Toop.T2 Bit.O Bit.O ->
            'a'

        Toop.T2 Bit.O Bit.I ->
            'i'

        Toop.T2 Bit.I Bit.O ->
            'o'

        Toop.T2 Bit.I Bit.I ->
            'u'


bitsToChunksOf10 : List Bit -> List (Toop.T10 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit)
bitsToChunksOf10 =
    \bits ->
        case bits of
            [] ->
                []

            bit0 :: [] ->
                List.singleton (Toop.T10 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0)

            bit0 :: bit1 :: [] ->
                List.singleton (Toop.T10 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1)

            bit0 :: bit1 :: bit2 :: [] ->
                List.singleton (Toop.T10 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2)

            bit0 :: bit1 :: bit2 :: bit3 :: [] ->
                List.singleton (Toop.T10 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: [] ->
                List.singleton (Toop.T10 Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3 bit4)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: [] ->
                List.singleton (Toop.T10 Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3 bit4 bit5)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: [] ->
                List.singleton (Toop.T10 Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3 bit4 bit5 bit6)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: [] ->
                List.singleton (Toop.T10 Bit.O Bit.O bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8 :: [] ->
                List.singleton (Toop.T10 Bit.O bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8 :: bit9 :: bit10Up ->
                Toop.T10 bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8 bit9 :: bitsToChunksOf10 bit10Up


type ShapeAppearance
    = Filled
    | OutlinedSolid
    | OutlinedDash
    | OutlinedDot


toRecognizableCollage :
    List Bit
    -> Collage event_
toRecognizableCollage =
    \bits ->
        let
            collages : List (Collage event_)
            collages =
                bits
                    |> bitsToChunksOf11
                    |> List.map toCollage
        in
        collages
            |> List.indexedMap
                (\i collage ->
                    let
                        collageCount : Int
                        collageCount =
                            collages |> List.length

                        part : Float
                        part =
                            (i |> Basics.toFloat) / (collageCount |> Basics.toFloat)

                        rotation : Float
                        rotation =
                            turns part

                        scale : Float
                        scale =
                            50 / ((collageCount |> Basics.toFloat) ^ 0.8)

                        radius : Float
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


toCollage : Toop.T11 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit -> Collage event_
toCollage =
    \(Toop.T11 bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8 bit9 bit10) ->
        Toop.T3 bit0 bit1 bit2
            |> toShape
            |> shapeAppearing
                { appearance =
                    Toop.T2 bit3 bit4
                        |> shapeAppearance
                , fillStyle =
                    Toop.T6 bit5 bit6 bit7 bit8 bit9 bit10
                        |> toColor
                        |> Collage.uniform
                }


toShape : Toop.T3 Bit Bit Bit -> Collage.Shape
toShape =
    \bits ->
        case bits of
            Toop.T3 Bit.O Bit.O Bit.O ->
                Collage.circle 1

            Toop.T3 Bit.O Bit.O Bit.I ->
                Collage.rectangle 0.5 1.5

            Toop.T3 Bit.O Bit.I Bit.O ->
                Collage.rectangle 1.5 0.5

            Toop.T3 b0 b1 b2 ->
                {- 3 to 8 -}
                Collage.ngon
                    ([ b0, b1, b2 ]
                        |> Bits.toIntUnsigned
                    )
                    1



-- ↓ these could e.g. be code-generated


toColor : Toop.T6 Bit Bit Bit Bit Bit Bit -> Color
toColor =
    \(Toop.T6 bit0 bit1 bit2 bit3 bit4 bit5) ->
        let
            component componentBit0 componentBit1 =
                (1 / 8)
                    + ([ componentBit0, componentBit1 ]
                        |> Bits.toIntUnsigned
                        |> Basics.toFloat
                      )
                    / 4
        in
        Color.rgb
            (component bit0 bit1)
            (component bit2 bit3)
            (component bit4 bit5)


shapeAppearing :
    { appearance : ShapeAppearance, fillStyle : Collage.FillStyle }
    -> (Collage.Shape -> Collage event_)
shapeAppearing { appearance, fillStyle } =
    case appearance of
        Filled ->
            Collage.filled fillStyle

        OutlinedSolid ->
            Collage.outlined (Collage.solid 0.5 fillStyle)

        OutlinedDash ->
            Collage.outlined (Collage.dash 0.5 fillStyle)

        OutlinedDot ->
            Collage.outlined (Collage.dot 0.5 fillStyle)


shapeAppearance : Toop.T2 Bit Bit -> ShapeAppearance
shapeAppearance bits =
    case bits of
        Toop.T2 Bit.O Bit.O ->
            Filled

        Toop.T2 Bit.O Bit.I ->
            OutlinedSolid

        Toop.T2 Bit.I Bit.O ->
            OutlinedDash

        Toop.T2 Bit.I Bit.I ->
            OutlinedDot


bitsToChunksOf11 : List Bit -> List (Toop.T11 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit)
bitsToChunksOf11 =
    \bits ->
        case bits of
            [] ->
                []

            bit0 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0)

            bit0 :: bit1 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1)

            bit0 :: bit1 :: bit2 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2)

            bit0 :: bit1 :: bit2 :: bit3 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3 bit4)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3 bit4 bit5)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3 bit4 bit5 bit6)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O Bit.O bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8 :: [] ->
                List.singleton (Toop.T11 Bit.O Bit.O bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8 :: bit9 :: [] ->
                List.singleton (Toop.T11 Bit.O bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8 bit9)

            bit0 :: bit1 :: bit2 :: bit3 :: bit4 :: bit5 :: bit6 :: bit7 :: bit8 :: bit9 :: bit10 :: bit11Up ->
                Toop.T11 bit0 bit1 bit2 bit3 bit4 bit5 bit6 bit7 bit8 bit9 bit10 :: bitsToChunksOf11 bit11Up
