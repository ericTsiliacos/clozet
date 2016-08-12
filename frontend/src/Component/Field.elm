module Component.Field
    exposing
        ( Model
        , Msg
        , init
        , update
        , view
        , currentInput
        )

import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (placeholder)


currentInput : Model -> String
currentInput model =
    model.currentInput


type alias Model =
    { currentInput : String
    , placeholderText : String
    }


init : Maybe String -> Model
init maybePlaceholderText =
    { currentInput = ""
    , placeholderText = Maybe.withDefault "" maybePlaceholderText
    }


type Msg
    = UpdateInput String


update : Msg -> Model -> Model
update action model =
    case action of
        UpdateInput title ->
            { model
                | currentInput = title
            }


view : Model -> Html Msg
view model =
    input
        [ placeholder model.placeholderText
        , onInput UpdateInput
        ]
        []
