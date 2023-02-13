module Bits.Convert exposing
    ( to01String
    , to09avChar
    , to09avString
    , toColor
    , toHexChar
    , toHexString
    , toReadableWord
    , toReadableWords
    , toReadableWordsString
    , toRecognizableCollage
    , toShape
    , toUnicodeString
    )

{-| Ideas on how to represent `Bit`s
-}

import Array
import ArraySized exposing (ArraySized)
import Bit as Bit exposing (Bit(..))
import Bit.Convert
import Bits
import Collage exposing (Collage)
import Collage.Layout
import Color exposing (Color)
import Linear exposing (Direction(..))
import N exposing (Add1, Exactly, In, N10, N11, N2, N3, N4, N5, N6, On, To, Up, Up0, Up10, Up11, Up3, Up4, n0, n1, n10, n11, n2, n3, n4, n5, n6)
import N.Local exposing (n20, n32)
import Toop
import Typed exposing (untag)


{-| Convert to a string: `O`s are shown as `0`, `1`s as `I`s.

    as01String (ArraySized.l4 I O I O)
    --> "1010"

-}
to01String : ArraySized Bit (In min_ max_) -> String
to01String =
    \bits ->
        bits
            |> ArraySized.map Bit.Convert.toChar
            |> ArraySized.toString


{-| Convert a `List` of `Bit`s to a

  - short
  - not human readable
  - not recognizable

`String` from unicode `Char`s.

-}
toUnicodeString :
    ArraySized Bit (In (On min_) (Up maxX_ To maxPlusX_))
    -> String
toUnicodeString =
    \bits ->
        bits
            |> Bits.toChunksOf n20
            |> ArraySized.map
                (ArraySized.maxTo n20
                    >> Bits.toN
                    >> N.toInt
                    >> Char.fromCode
                )
            |> ArraySized.toString


{-| Four bits represented as a hex `Char` (0-9 then a-f)
-}
toHexChar : ArraySized Bit (In (Up minTo4_ To N4) (Up maxTo4_ To N4)) -> Char
toHexChar =
    \bits ->
        case bits |> Bits.padToLength n4 |> ArraySized.to4 of
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


toHexString : ArraySized Bit (In (On min_) (Up maxX_ To maxPlusX_)) -> String
toHexString =
    \bits ->
        bits
            |> Bits.toChunksOf n4
            |> ArraySized.map toHexChar
            |> ArraySized.toString


to09avChar : ArraySized Bit (In (Up minTo5_ To N5) (Up maxTo5_ To N5)) -> Char
to09avChar =
    \bits ->
        let
            bitsPadded : ArraySized Bit (Exactly (On N5))
            bitsPadded =
                bits |> Bits.padToLength n5
        in
        case bitsPadded |> ArraySized.element ( Up, n0 ) of
            O ->
                bitsPadded
                    |> ArraySized.take Down { atLeast = n4 } n4
                    |> toHexChar

            I ->
                case bitsPadded |> ArraySized.take Down { atLeast = n4 } n4 |> ArraySized.to4 of
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


to09avString : ArraySized Bit (In (On min_) (Up maxX_ To maxPlusX_)) -> String
to09avString =
    \bits ->
        bits
            |> Bits.toChunksOf n5
            |> ArraySized.map to09avChar
            |> ArraySized.toString


{-| Four bits represented in a `Char` of multiple uniquely identifiable symbols
-}
asFirstLetterInWord : ArraySized Bit (In (Up minTo4_ To N4) (Up maxTo4_ To N4)) -> Char
asFirstLetterInWord =
    \bits ->
        case bits |> Bits.padToLength n4 |> ArraySized.to4 of
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


asThirdLetterInWord : ArraySized Bit (In (Up minTo4_ To N4) (Up maxTo4_ To N4)) -> Char
asThirdLetterInWord =
    \bits ->
        case bits |> Bits.padToLength n4 |> ArraySized.to4 of
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


asVocal : ArraySized Bit (In (Up minTo2_ To N2) (Up maxTo2_ To N2)) -> Char
asVocal =
    \bits ->
        case bits |> Bits.padToLength n2 |> ArraySized.to2 of
            Toop.T2 O O ->
                'a'

            Toop.T2 O I ->
                'i'

            Toop.T2 I O ->
                'o'

            Toop.T2 I I ->
                'u'


toReadableWord : ArraySized Bit (In (Up minTo10_ To N10) (Up maxTo10_ To N10)) -> String
toReadableWord =
    \bits ->
        let
            -- adding ↓ crashes the compiler for whatever reason
            -- bitsPadded : ArraySized (In (Up10 paddedMinX_) (Up10 paddedMaxX_)) Bit
            bitsPadded =
                bits |> Bits.padToLength n10 |> ArraySized.minTo n10
        in
        [ bitsPadded
            |> ArraySized.take Up { atLeast = n4 } n4
            |> asFirstLetterInWord
        , ArraySized.l2
            (bitsPadded |> ArraySized.element ( Up, n4 ))
            (bitsPadded |> ArraySized.element ( Up, n5 ))
            |> asVocal
        , bitsPadded
            |> ArraySized.take Down { atLeast = n4 } n4
            |> asThirdLetterInWord
        ]
            |> String.fromList


toReadableWords :
    ArraySized Bit (In (On min_) (Up maxX To maxPlusX))
    -> ArraySized String (In (Up0 minX_) (Up maxX To (Add1 maxPlusX)))
toReadableWords =
    \bits ->
        bits
            |> Bits.toChunksOf n10
            |> ArraySized.map toReadableWord


toReadableWordsString : ArraySized Bit (In (On min_) (Up maxX_ To maxPlusX_)) -> String
toReadableWordsString =
    \bits ->
        bits
            |> toReadableWords
            |> ArraySized.toList
            |> String.join " "


toColor : ArraySized Bit (In (Up minTo6_ To N6) (Up maxTo6_ To N6)) -> Color
toColor =
    \bits ->
        let
            bitsPadded =
                bits |> Bits.padToLength n6

            component componentBits =
                (1 / 8)
                    + (componentBits
                        |> Bits.toN
                        |> N.toFloat
                      )
                    / 4
        in
        Color.rgb
            (bitsPadded
                |> ArraySized.take Up { atLeast = n2 } n2
                |> component
            )
            (ArraySized.l2 (bitsPadded |> ArraySized.element ( Up, n2 ))
                (bitsPadded |> ArraySized.element ( Up, n3 ))
                |> component
            )
            (bitsPadded
                |> ArraySized.take Down { atLeast = n2 } n2
                |> component
            )


toShape : ArraySized Bit (In (Up minTo3_ To N3) (Up maxTo3_ To N3)) -> Collage.Shape
toShape =
    \bits ->
        case bits |> Bits.padToLength n3 |> ArraySized.to3 of
            Toop.T3 O O O ->
                Collage.circle 1

            Toop.T3 O O I ->
                Collage.rectangle 0.5 1.5

            Toop.T3 O I O ->
                Collage.rectangle 1.5 0.5

            Toop.T3 b0 b1 b2 ->
                {- 3 to 8 -}
                Collage.ngon
                    (ArraySized.l3 b0 b1 b2
                        |> Bits.toN
                        |> N.toInt
                    )
                    1


toCollage : ArraySized Bit (In (Up minTo11_ To N11) (Up maxTo11_ To N11)) -> Collage msg
toCollage =
    \bits ->
        let
            bitsPadded =
                bits |> Bits.padToLength n11
        in
        bitsPadded
            |> ArraySized.take Up { atLeast = n3 } n3
            |> toShape
            |> shapeAppearing
                { appearance =
                    bitsPadded
                        |> ArraySized.drop Up n3
                        |> ArraySized.take Up { atLeast = n2 } n2
                        |> shapeAppearance
                , fillStyle =
                    bitsPadded
                        |> ArraySized.take Down { atLeast = n6 } n6
                        |> toColor
                        |> Collage.uniform
                }


type ShapeAppearance
    = Filled
    | OutlinedSolid
    | OutlinedDash
    | OutlinedDot


shapeAppearing :
    { appearance : ShapeAppearance, fillStyle : Collage.FillStyle }
    -> (Collage.Shape -> Collage msg)
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


shapeAppearance :
    ArraySized Bit (In (Up minTo2_ To N2) (Up maxTo2_ To N2))
    -> ShapeAppearance
shapeAppearance =
    \bits ->
        case bits |> Bits.padToLength n2 |> ArraySized.to2 of
            Toop.T2 O O ->
                Filled

            Toop.T2 O I ->
                OutlinedSolid

            Toop.T2 I O ->
                OutlinedDash

            Toop.T2 I I ->
                OutlinedDot


toRecognizableCollage :
    ArraySized Bit (In (On min_) (Up maxX_ To maxPlusX_))
    -> Collage msg_
toRecognizableCollage =
    \bits ->
        let
            collages =
                bits
                    |> Bits.toChunksOf n11
                    |> ArraySized.map toCollage
                    |> ArraySized.toArray
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
