module Main exposing (main)

import Arr exposing (Arr)
import Array
import Bits.Represent as RepresentBits
import Browser
import Collage exposing (Collage)
import Collage.Render
import Element as Ui
import Element.Background as UiBg
import Element.Border as UiBorder
import Element.Font as Font
import Element.Input as UiInput
import Html exposing (Html)
import LinearDirection exposing (LinearDirection(..))
import Lue.Bit as Bit exposing (Bit(..))
import MinArr
import NNats exposing (..)
import Nat exposing (Min)
import TypeNats exposing (..)
import Typed exposing (val)


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> init
        , update = update
        , view = viewDocument
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { inputBits : Arr (Min Nat0) Bit
    }


init : ( Model, Cmd Msg )
init =
    ( { inputBits =
            Arr.from6 O I O O I I
                |> Arr.lowerMinLength nat0
                |> MinArr.value
      }
    , Cmd.none
    )


type Msg
    = InputText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputText string ->
            ( { model
                | inputBits =
                    string
                        |> String.toList
                        |> List.reverse
                        |> List.filterMap
                            (\char ->
                                case char of
                                    '0' ->
                                        Just O

                                    '1' ->
                                        Just I

                                    _ ->
                                        Nothing
                            )
                        |> Array.fromList
                        |> Arr.fromArray
              }
            , Cmd.none
            )


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "try bits"
    , body =
        [ view model
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


view : Model -> Ui.Element Msg
view model =
    [ Ui.text "Representing bits"
        |> Ui.el
            [ Font.size 40 ]
    , [ [ [ UiInput.text
                [ Ui.padding 3
                , UiBg.color (Ui.rgba 0 0 0 0)
                , Font.size 20
                , Ui.width Ui.fill
                , UiBorder.color (Ui.rgba 0 0 0 0)
                ]
                { label = UiInput.labelHidden "enter 0s & 1s"
                , onChange = InputText
                , placeholder = Nothing
                , text =
                    case
                        model.inputBits
                            |> MinArr.isLengthAtLeast nat1
                                { lowest = nat0 }
                    of
                        Nat.EqualOrGreater atLeast1 ->
                            RepresentBits.as01String atLeast1

                        Nat.Below _ ->
                            "enter zeros & ones > "
                }
          , Ui.el
                [ Ui.width Ui.fill
                , UiBg.color (Ui.rgb 1 0.7 0)
                , Ui.height (Ui.px 2)
                ]
                Ui.none
          ]
            |> Ui.column
                [ Ui.width Ui.fill ]
        , Ui.text
            ((model.inputBits
                |> Arr.length
                |> val
                |> String.fromInt
             )
                ++ " bits"
            )
            |> Ui.el
                [ Font.color (Ui.rgba 1 1 1 0.5)
                , Ui.padding 3
                ]
        ]
            |> Ui.column
                [ Ui.width Ui.fill
                , Ui.spacing 3
                ]
      , let
            text =
                Ui.text
                    >> Ui.el
                        [ Font.color (Ui.rgb 1 0.5 0.5)
                        , Font.family [ Font.monospace ]
                        , Font.size 24
                        ]

            svg =
                Collage.Render.svg
                    >> Ui.html
                    >> Ui.el [ Ui.paddingXY 0 8 ]
        in
        [ ( "as recognizable collage"
          , svg << RepresentBits.asRecognizableCollage
          )
        , ( "as short unicode string"
          , text << RepresentBits.asShortUnicodeString
          )
        , ( "as hex (0-9 then a-f) string"
          , text << RepresentBits.asHexString
          )
        , ( "as 0-9 then a-v string"
          , text << RepresentBits.as09avString
          )
        , ( "as readable string from words"
          , text << RepresentBits.asReadableWordsString
          )
        ]
            |> List.map
                (\( description, representation ) ->
                    Ui.column [ Ui.spacing 4 ]
                        [ Ui.el
                            [ Font.family [ Font.typeface "Noto Sans" ]
                            , Font.size 24
                            ]
                            (Ui.text description)
                        , representation model.inputBits
                        ]
                )
            |> Ui.column
                [ Ui.spacing 40, Ui.paddingXY 48 28 ]
      ]
        |> Ui.column
            [ Ui.padding 32
            , Ui.width Ui.fill
            ]
    ]
        |> Ui.column
            [ Ui.paddingXY 40 60
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            , UiBg.color (Ui.rgb 0 0 0)
            , Font.color (Ui.rgb 1 1 1)
            ]
