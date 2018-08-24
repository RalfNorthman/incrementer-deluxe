module Main exposing (main)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import List
import Regex exposing (regex, replace)


-- Model


type alias Model =
    { current : Int
    , maximum : Int
    , minimum : Int
    }


initialModel : Model
initialModel =
    { current = 0
    , maximum = 0
    , minimum = 0
    }



-- Update


type Msg
    = Modify Int
    | ResetCurrent
    | ResetMax
    | ResetMin


update : Msg -> Model -> Model
update msg model =
    case msg of
        Modify x ->
            let
                newCurrent =
                    model.current + x
            in
                { model
                    | current = newCurrent
                    , maximum = max newCurrent model.maximum
                    , minimum = min newCurrent model.minimum
                }

        ResetCurrent ->
            { model
                | current = 0
                , maximum = max 0 model.maximum
                , minimum = min 0 model.minimum
            }

        ResetMax ->
            { model
                | maximum = model.current
            }

        ResetMin ->
            { model
                | minimum = model.current
            }



-- View Helpers


googleFont : String -> Attribute Msg
googleFont fontName =
    let
        fontString =
            replace Regex.All (regex " ") (\_ -> "+") fontName
    in
        Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=" ++ fontString
                , name = fontName
                }
            ]


buttonStyle : List (Attribute Msg)
buttonStyle =
    [ Background.color lightGrey
    , Border.rounded 5
    , Border.width 2
    , Border.color darkGrey
    , mouseDown [ Background.color grey ]
    , mouseOver [ scale <| 8 / 7 ]
    , width <| px 70
    , padding 10
    ]


modButton : Int -> Element Msg
modButton amount =
    let
        label =
            if amount < 0 then
                toString amount
            else
                "+" ++ toString amount
    in
        button buttonStyle
            { onPress = Just <| Modify amount
            , label = text label
            }


middleText : String -> Int -> Element Msg
middleText label number =
    row [ centerX, width <| px 210, paddingXY 10 0 ]
        [ text <| label ++ ":"
        , el [ alignRight ] <| text <| toString number
        ]


resetButton : Msg -> Element Msg
resetButton msg =
    button buttonStyle
        { onPress = Just msg
        , label = text "Reset"
        }


middleRow : String -> Int -> Msg -> Element Msg
middleRow valueType value reset =
    row
        [ spacing 5
        , paddingXY 10 5

        --  , paddingEach
        --      { left = 5
        --      , bottom = 0
        --      , top = 0
        --      , right = 0
        --      }
        ]
        [ middleText valueType value
        , resetButton reset
        ]



-- View


view : Model -> Html Msg
view model =
    layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ padding 50
        , googleFont "Fjalla One"
        ]
    <|
        column
            [ spacing 5
            , width <| px 275
            , alignLeft
            ]
            [ row [ spacing 5 ] <| List.map modButton [ 1, 10, 100, 1000 ]
            , middleRow "Maximum" model.maximum ResetMax
            , middleRow "Current" model.current ResetCurrent
            , middleRow "Minimum" model.minimum ResetMin
            , row [ spacing 5 ] <| List.map modButton [ -1, -10, -100, -1000 ]
            ]



-- Main


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
