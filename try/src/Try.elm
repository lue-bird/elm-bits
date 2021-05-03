module Try exposing (main)

import Arr exposing (Arr)
import Bits.Represent as RepresentBits
import Browser
import Collage exposing (Collage)
import Collage.Render
import Element as Ui
import Element.Background as UiBg
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Html exposing (Html)
import Html.Attributes
import LinearDirection exposing (LinearDirection(..))
import Lue.Bit as Bit exposing (Bit(..))
import MinArr
import NNats exposing (..)
import Nat exposing (Min)
import TypeNats exposing (..)
import Typed exposing (val)
import Util exposing (last, removeLast)


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , view = view
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
            ( case last (String.toList string) of
                Just '0' ->
                    { model
                        | inputBits =
                            model.inputBits
                                |> MinArr.push O
                                |> Arr.lowerMinLength nat0
                    }

                Just '1' ->
                    { model
                        | inputBits =
                            model.inputBits
                                |> MinArr.push I
                                |> Arr.lowerMinLength nat0
                    }

                Nothing ->
                    { model
                        | inputBits = removeLast model.inputBits
                    }

                Just _ ->
                    model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Ui.layoutWith
        { options =
            [ Ui.focusStyle
                { borderColor = Just (Ui.rgba 0 1 1 0.38)
                , backgroundColor = Nothing
                , shadow =
                    Just
                        { color = Ui.rgba 0 1 1 0.13
                        , blur = 1
                        , size = 1
                        , offset = ( 0, 0 )
                        }
                }
            ]
        }
        []
        (Ui.column
            [ Ui.paddingXY 40 60
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            , UiBg.color (Ui.rgb 0 0 0)
            , UiFont.color (Ui.rgb 1 1 1)
            ]
            [ Ui.el
                [ UiFont.size 40
                ]
                (Ui.text "Try out some bit stuff")
            , Ui.row [ Ui.paddingXY 16 24 ]
                [ Ui.paragraph []
                    [ Ui.text
                        (RepresentBits.as01String
                            (model.inputBits |> removeLast)
                        )

                    --editable bit: delete or write 0 or 1
                    , UiInput.text
                        [ UiBorder.color (Ui.rgba 0 1 1 0.2)
                        , UiBorder.rounded 20
                        , Ui.padding 3
                        , UiBg.color (Ui.rgba 0 0 0 0)
                        ]
                        { label = UiInput.labelHidden "enter 0 or 1"
                        , onChange = InputText
                        , placeholder = Nothing
                        , text =
                            model.inputBits
                                |> MinArr.isLengthAtLeast nat1
                                    { min = nat0 }
                                    { equalOrGreater =
                                        \atLeast1 ->
                                            String.fromInt
                                                (Bit.to0or1
                                                    (atLeast1 |> Arr.at nat0 LastToFirst)
                                                    |> val
                                                )
                                    , less = \_ -> "enter 0 or 1 > "
                                    }
                        }
                    ]
                ]
            , Ui.column [ Ui.spacing 26 ]
                ([ ( "bit count", Text << (Arr.length >> val >> String.fromInt) )
                 , ( "as short unicode string"
                   , Text << RepresentBits.asShortUnicodeString
                   )
                 , ( "as hex (0-9 then a-f) string"
                   , Text << RepresentBits.asHexString
                   )
                 , ( "as 0-9 then a-v string"
                   , Text << RepresentBits.as09avString
                   )
                 , ( "as readable string from words"
                   , Text << RepresentBits.asReadableWordsString
                   )
                 , ( "as recognizable collage"
                   , Svg << RepresentBits.asRecognizableCollage
                   )
                 ]
                    |> List.map
                        (\( description, representation ) ->
                            Ui.column [ Ui.spacing 4 ]
                                [ Ui.el
                                    [ UiFont.family [ UiFont.typeface "Noto Sans" ]
                                    , UiFont.size 17
                                    ]
                                    (Ui.text description)
                                , case representation model.inputBits of
                                    Text asString ->
                                        Ui.el
                                            [ UiFont.color (Ui.rgb 1 0.5 0.5)
                                            , UiFont.family [ UiFont.monospace ]
                                            ]
                                            (Ui.text asString)

                                    Svg asCollage ->
                                        Ui.html
                                            (Collage.Render.svg asCollage)
                                ]
                        )
                )
            ]
        )


type BitRepresentation msg
    = Text String
    | Svg (Collage msg)
