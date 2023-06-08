module App exposing (main)

import Array
import ArraySized exposing (ArraySized)
import Bit exposing (Bit(..))
import Bit.Convert
import Bits.Convert
import Browser
import Collage exposing (Collage)
import Collage.Render
import Element as Ui
import Element.Background as UiBg
import Element.Border as UiBorder
import Element.Font as Font
import Element.Input as UiInput
import Emptiable
import Html exposing (Html)
import Html.Attributes
import Linear exposing (Direction(..))
import Minidenticons
import N exposing (Min, N0, On, n0, n1)
import N.Local exposing (n32)
import Phace
import QRCode
import Svg.Attributes as SvgA


main : Program () State Event
main =
    Browser.document
        { init = \() -> init
        , update = reactTo
        , view = uiDocument
        , subscriptions = \_ -> Sub.none
        }


type alias State =
    { inputBits : ArraySized Bit (Min (On N0))
    }


init : ( State, Cmd Event )
init =
    ( { inputBits =
            ArraySized.l9 I I O O I O I O I
                |> ArraySized.minTo n0
                |> ArraySized.maxToInfinity
      }
    , Cmd.none
    )


type Event
    = InputText String


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
                                (Bit.Convert.fromChar >> Result.toMaybe)
                            |> Array.fromList
                            |> ArraySized.fromArray
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
                |> Ui.layoutWith
                    { options =
                        [ Ui.focusStyle
                            { borderColor = Nothing
                            , backgroundColor = Nothing
                            , shadow = Nothing
                            }
                        ]
                    }
                    []
            ]
        }


view : State -> Ui.Element Event
view state =
    [ [ "Representing bits"
            |> Ui.text
            |> Ui.el [ Font.size 40 ]
      , [ "✎ enter sequence of 0s and 1s:" |> descriptionUi
        , UiInput.text
            [ UiBg.color (Ui.rgba 0 0 0 0)
            , Font.size 25
            , Font.family [ Font.monospace ]
            , Ui.width Ui.fill
            , Ui.paddingXY 36 6
            , UiBorder.color (Ui.rgba 0 0 0 0)
            ]
            { label = UiInput.labelHidden "enter a bit sequence containing 0s and 1s"
            , onChange = InputText
            , placeholder = Nothing
            , text =
                case state.inputBits |> ArraySized.hasAtLeast1 of
                    Emptiable.Filled atLeast1 ->
                        atLeast1 |> Bits.Convert.to01String

                    Emptiable.Empty _ ->
                        "0"
            }
        ]
            |> Ui.column
                [ Ui.paddingEach { left = 3, right = 3, top = 3, bottom = 3 }
                , Ui.width Ui.fill
                ]
      ]
        |> Ui.column [ Ui.spacing 30, Ui.width Ui.fill ]
    , Ui.column
        [ Ui.spacing 12
        ]
        ([ ( "identicon"
           , \bits ->
                bits
                    |> Bits.Convert.toHexString
                    |> Minidenticons.identicon 100 50
                    |> Ui.html
                    |> Ui.el [ Ui.width (Ui.px 100), Ui.height (Ui.px 100) ]
           )
         , ( "qr code"
           , \bits ->
                case bits |> Bits.Convert.toHexString |> QRCode.fromString of
                    Err error ->
                        descriptionUi ("Encoding error: " ++ (error |> Debug.toString))

                    Ok qrCode ->
                        [ qrCode
                            |> QRCode.toSvg
                                [ SvgA.width "120px"
                                , SvgA.height "120px"
                                ]
                        ]
                            |> Html.div [ Html.Attributes.style "filter" "invert(100)" ]
                            |> Ui.html
                            |> Ui.el
                                [ UiBg.color (Ui.rgb 0 0 0)
                                , Ui.width Ui.shrink
                                ]
           )
         , ( "collage"
           , \bits ->
                bits
                    |> Bits.Convert.toRecognizableCollage
                    |> Collage.Render.svg
                    |> Ui.html
                    |> Ui.el [ Ui.paddingXY 0 8 ]
           )
         , ( "phace"
           , \bits ->
                case Phace.fromHexString (bits |> Bits.Convert.toHexString |> hexStringToAtLeast32) 100 100 of
                    Err error ->
                        descriptionUi ("Encoding error: " ++ (error |> Phace.errorToString))

                    Ok phace ->
                        phace
                            |> Ui.html
                            |> Ui.el
                                [ UiBg.color (Ui.rgb 0 0 0)
                                , Ui.width Ui.shrink
                                ]
           )
         , ( "bit count"
           , \bits ->
                bits
                    |> ArraySized.length
                    |> N.toInt
                    |> String.fromInt
                    |> Ui.text
           )
         , ( "string from all unicode"
           , Bits.Convert.toUnicodeString >> Ui.text
           )
         , ( "string from hex: 0→9 then a→f"
           , Bits.Convert.toHexString >> Ui.text
           )
         , ( "string from 0→9 then a→v"
           , Bits.Convert.to09avString >> Ui.text
           )
         , ( "string from words"
           , Bits.Convert.toReadableWordsString >> Ui.text
           )
         ]
            |> List.map
                (\( description, representation ) ->
                    Ui.column []
                        [ description |> descriptionUi
                        , state.inputBits
                            |> ArraySized.maxToOn
                            |> representation
                            |> Ui.el
                                [ Ui.paddingXY 36 6
                                , Font.size 24
                                , Font.family [ Font.monospace ]
                                ]
                        ]
                )
        )
    ]
        |> Ui.column
            [ Ui.paddingXY 62 60
            , Ui.spacing 40
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            , UiBg.color (Ui.rgb 0 0 0)
            , Font.color (Ui.rgb 1 1 1)
            ]


descriptionUi : String -> Ui.Element event_
descriptionUi =
    \description ->
        description
            |> Ui.text
            |> Ui.el
                [ Font.color (Ui.rgb 1 0.5 0.5)
                , Font.family [ Font.sansSerif ]
                , Font.size 24
                ]
            |> List.singleton
            |> Ui.paragraph
                [ Html.Attributes.style "word-break" "break-all"
                    |> Ui.htmlAttribute
                ]


hexStringToAtLeast32 : String -> String
hexStringToAtLeast32 =
    \hexString ->
        hexString
            ++ String.repeat (34 - (hexString |> String.length)) "0"
