module Components.Button exposing (primaryButton)

import Html exposing (..)
import Style exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import ButtonStateStyle exposing (..)


primaryButton : String -> msg -> Html msg
primaryButton content msg =
    styleButtonStates initialState
        activeState
        hoveState
        button
        [ baseStyle
        , onClick msg
        ]
        [ text content ]


baseStyle : Attribute msg
baseStyle =
    style
        [ width (px 200)
        , height (pc 100)
        , border none
        , color "#fff"
        , backgroundColor "#6496c8"
        , textShadow "-1px 1px #417cb8"
        , fontSize (px 50)
        ]


initialState : List ( String, String )
initialState =
    [ backgroundColor "#6496c8"
    , textShadow "-1px 1px #417cb8"
    ]


hoveState : List ( String, String )
hoveState =
    [ backgroundColor "#346392"
    , textShadow "-1px 1px #27496d"
    ]


activeState : List ( String, String )
activeState =
    [ backgroundColor "#27496d"
    , textShadow "-1px 1px #193047"
    ]
