module Main exposing (main)

import Bit exposing (Bit)
import Bit.Convert
import Bits.Convert
import Browser
import Collage.Render
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Attributes
import List
import Minidenticons
import Phace
import QRCode
import Svg.Attributes


main : Program () State Event
main =
    Browser.document
        { init = \() -> init
        , update = reactTo
        , view = uiDocument
        , subscriptions = \_ -> Sub.none
        }


init : ( State, Cmd Event )
init =
    ( { inputBits = [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]
      }
    , Cmd.none
    )


reactTo : Event -> (State -> ( State, Cmd Event ))
reactTo event =
    case event of
        InputText string ->
            \state ->
                ( { state
                    | inputBits =
                        string
                            |> String.toList
                            |> List.filterMap
                                (\char -> char |> Bit.Convert.fromChar |> Result.toMaybe)
                  }
                , Cmd.none
                )


uiDocument : State -> Browser.Document Event
uiDocument =
    \state ->
        { title = "representing bits"
        , body =
            [ state
                |> view
                |> Element.layoutWith
                    { options =
                        [ Element.focusStyle
                            { borderColor = Nothing
                            , backgroundColor = Nothing
                            , shadow = Nothing
                            }
                        ]
                    }
                    []
            ]
        }


uiInteractiveUnderline : List (Element.Attribute event_)
uiInteractiveUnderline =
    [ Element.Border.widthEach { left = 0, right = 0, top = 0, bottom = 3 }
    , Element.Border.dotted
    , Element.Border.color (Element.rgb 0.5 0.8 1)
    ]


view : State -> Element.Element Event
view state =
    [ [ "representing bits"
            |> Element.text
            |> Element.el [ Element.Font.size 40 ]
      , [ "✎ enter sequence of 0s and 1s:" |> descriptionUi
        , Element.Input.text
            ([ Element.Background.color (Element.rgba 0 0 0 0)
             , Element.Font.size 25
             , Element.Font.family [ Element.Font.monospace ]
             , Element.width Element.fill
             , Element.paddingXY 36 6
             ]
                ++ uiInteractiveUnderline
            )
            { label = Element.Input.labelHidden "enter a bit sequence containing 0s and 1s"
            , onChange = InputText
            , placeholder = Nothing
            , text =
                case state.inputBits of
                    [] ->
                        "0"

                    firstBit :: secondBitUp ->
                        (firstBit :: secondBitUp) |> Bits.Convert.to01String
            }
        ]
            |> Element.column
                [ Element.paddingEach { left = 3, right = 3, top = 3, bottom = 3 }
                , Element.width Element.fill
                ]
      ]
        |> Element.column [ Element.spacing 30, Element.width Element.fill ]
    , Element.wrappedRow
        [ Element.spacing 30
        ]
        ([ ( "bit count" |> descriptionUi
           , \bits ->
                bits
                    |> List.length
                    |> String.fromInt
                    |> Element.text
           )
         , ( externalLinkUi
                { label = "identicon" |> descriptionUi
                , url = "laurentpayot/minidenticons-elm"
                }
           , \bits ->
                bits
                    |> Bits.Convert.toHexString
                    |> Minidenticons.identicon 100 50
                    |> Element.html
                    |> Element.el [ Element.width (Element.px 100), Element.height (Element.px 100) ]
           )
         , ( externalLinkUi
                { label = "qr code" |> descriptionUi
                , url = packageUrl "pablohirafuji/elm-qrcode"
                }
           , \bits ->
                case bits |> Bits.Convert.toHexString |> QRCode.fromString of
                    Err error ->
                        descriptionUi ("Encoding error: " ++ (error |> qrCodeErrorToString))

                    Ok qrCode ->
                        [ qrCode
                            |> QRCode.toSvg
                                [ Svg.Attributes.width "120px"
                                , Svg.Attributes.height "120px"
                                ]
                        ]
                            |> Html.div [ Html.Attributes.style "filter" "invert(100)" ]
                            |> Element.html
                            |> Element.el
                                [ Element.Background.color (Element.rgba 0 0 0 0)
                                , Element.width Element.shrink
                                ]
           )
         , ( "collage" |> descriptionUi
           , \bits ->
                bits
                    |> Bits.Convert.toRecognizableCollage
                    |> Collage.Render.svg
                    |> Element.html
                    |> Element.el [ Element.paddingXY 0 8 ]
           )
         , ( externalLinkUi
                { label = descriptionUi "phace"
                , url = packageUrl "coinop-logan/phace"
                }
           , \bits ->
                case Phace.fromHexString (bits |> Bits.Convert.toHexString |> hexStringToAtLeast32) 100 100 of
                    Err error ->
                        descriptionUi ("Encoding error: " ++ (error |> Phace.errorToString))

                    Ok phace ->
                        phace
                            |> Element.html
                            |> Element.el
                                [ Element.Background.color (Element.rgb 0 0 0)
                                , Element.width Element.shrink
                                ]
           )
         , ( descriptionUi "all unicode"
           , \bits -> bits |> Bits.Convert.toUnicodeString |> Element.text
           )
         , ( descriptionUi "hex: 0..9a..f"
           , \bits -> bits |> Bits.Convert.toHexString |> Element.text
           )
         , ( descriptionUi "0..9a..v"
           , \bits -> bits |> Bits.Convert.to09avString |> Element.text
           )
         , ( descriptionUi "word-ishs"
           , \bits -> bits |> Bits.Convert.toReadableWordsString |> Element.text
           )
         , ( externalLinkUi
                { label = descriptionUi "proquint"
                , url = packageUrl "michaelglass/proquint"
                }
           , \bits -> bits |> Bits.Convert.toProquintSequence |> Element.text
           )
         ]
            |> List.map
                (\( description, representation ) ->
                    Element.column
                        [ Element.Background.color (Element.rgba 0.2 1 0.2 0.06)
                        , Element.height Element.fill
                        , Element.padding 20
                        , Element.spacing 14
                        , Element.Border.rounded 20
                        ]
                        [ description
                            |> Element.el
                                [ Element.centerX
                                ]
                            |> Element.el
                                [ Element.centerX
                                , Element.width (Element.minimum 200 Element.fill)
                                ]
                        , state.inputBits
                            |> representation
                            |> Element.el
                                [ Element.Font.size 24
                                , Element.Font.family [ Element.Font.monospace ]
                                , Element.centerX
                                ]
                        ]
                )
        )
    ]
        |> Element.column
            [ Element.paddingEach { left = 62, right = 62, top = 60, bottom = 0 }
            , Element.spacing 40
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.Background.color (Element.rgb 0 0 0)
            , Element.Font.color (Element.rgb 1 1 1)
            ]


packageUrl : String -> String
packageUrl packageName =
    [ "https://dark.elm.dmy.fr/packages/", packageName, "/latest/" ] |> String.concat


externalLinkUi : { url : String, label : Element.Element event } -> Element.Element event
externalLinkUi =
    Element.newTabLink uiInteractiveUnderline


descriptionUi : String -> Element.Element event_
descriptionUi =
    \description ->
        description
            |> Element.text
            |> Element.el
                [ Element.Font.family [ Element.Font.sansSerif ]
                , Element.Font.size 24
                , Element.alpha 0.7
                ]
            |> List.singleton
            |> Element.paragraph
                [ Html.Attributes.style "word-break" "break-all"
                    |> Element.htmlAttribute
                ]


hexStringToAtLeast32 : String -> String
hexStringToAtLeast32 =
    \hexString ->
        hexString
            ++ String.repeat (34 - (hexString |> String.length)) "0"


type alias State =
    { inputBits : List Bit
    }


type Event
    = InputText String


qrCodeErrorToString : QRCode.Error -> String
qrCodeErrorToString =
    \qrCodeError ->
        case qrCodeError of
            QRCode.AlignmentPatternNotFound ->
                "alignment pattern not found"

            QRCode.InvalidNumericChar ->
                "invalid numeric char"

            QRCode.InvalidAlphanumericChar ->
                "invalid alphanumeric char"

            QRCode.InvalidUTF8Char ->
                "invalid UTF-8 char"

            QRCode.LogTableException n ->
                "log table exception with " ++ (n |> String.fromInt)

            QRCode.PolynomialMultiplyException ->
                "polynomial multiply exception"

            QRCode.PolynomialModException ->
                "polynomial mod exception"

            QRCode.InputLengthOverflow ->
                "input length overflow"
