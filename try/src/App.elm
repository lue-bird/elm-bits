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
import Html exposing (Html)
import Html.Attributes
import Linear exposing (Direction(..))
import N exposing (Fixed, Min, N0, n0, n1)


main : Program () State Event
main =
    Browser.document
        { init = \() -> init
        , update = reactTo
        , view = uiDocument
        , subscriptions = \_ -> Sub.none
        }


type alias State =
    { inputBits : ArraySized (Min (Fixed N0)) Bit
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
                            |> List.reverse
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
      , [ [ UiInput.text
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
                        state.inputBits |> ArraySized.hasAtLeast n1
                    of
                        Ok atLeast1 ->
                            atLeast1 |> Bits.Convert.to01String

                        Err _ ->
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
        , ((state.inputBits
                |> ArraySized.length
                |> N.toInt
                |> String.fromInt
           )
            ++ " bits"
          )
            |> Ui.text
            |> Ui.el
                [ Font.color (Ui.rgba 1 1 1 0.5)
                , Ui.padding 3
                ]
        ]
            |> Ui.column
                [ Ui.width Ui.fill
                , Ui.spacing 3
                ]
      ]
        |> Ui.column [ Ui.spacing 30 ]
    , let
        text =
            Ui.text
                >> Ui.el
                    [ Font.color (Ui.rgb 1 0.5 0.5)
                    , Font.family [ Font.monospace ]
                    , Font.size 24
                    ]
                >> List.singleton
                >> Ui.paragraph
                    [ Ui.padding 8
                    , Ui.width Ui.fill
                    , Html.Attributes.style "word-break" "break-all"
                        |> Ui.htmlAttribute
                    ]

        svg =
            Collage.Render.svg
                >> Ui.html
                >> Ui.el [ Ui.paddingXY 0 8 ]
      in
      Ui.table
        [ Ui.spacingXY 19 4
        ]
        { data =
            [ ( "collage"
              , Bits.Convert.toRecognizableCollage >> svg
              )
            , ( "string from all unicode"
              , Bits.Convert.toUnicodeString >> text
              )
            , ( "string from hex: 0→9 then a→f"
              , Bits.Convert.toHexString >> text
              )
            , ( "string from 0→9 then a→v"
              , Bits.Convert.to09avString >> text
              )
            , ( "string from words"
              , Bits.Convert.toReadableWordsString >> text
              )
            ]
        , columns =
            [ { header =
                    Ui.text "as"
                        |> Ui.el [ Font.size 24 ]
              , width = Ui.shrink
              , view =
                    \( description, _ ) ->
                        description
                            |> Ui.text
                            |> Ui.el
                                [ Font.family [ Font.typeface "Noto Sans" ]
                                , Font.size 24
                                ]
              }
            , { header = Ui.none
              , width = Ui.shrink
              , view =
                    \( _, representation ) ->
                        state.inputBits |> representation
              }
            ]
        }
    ]
        |> Ui.column
            [ Ui.paddingXY 62 60
            , Ui.spacing 40
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            , UiBg.color (Ui.rgb 0 0 0)
            , Font.color (Ui.rgb 1 1 1)
            ]
