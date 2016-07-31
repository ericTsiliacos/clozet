module App
    exposing
        ( init
        , update
        , view
        )

import Html exposing (..)
import Style exposing (..)
import ButtonStateStyle exposing (styleButtonStates)
import Html.Attributes exposing (style, placeholder, id)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style)


type alias Model =
    { clothing : List String
    , currentTitle : String
    , route : Route
    }


init : ( Model, Cmd Msg )
init =
    ( { clothing = []
      , currentTitle = ""
      , route = WatchList
      }
    , Cmd.none
    )


type Route
    = WatchList
    | AddNewClothing


type Msg
    = NoOp
    | AddClothing
    | UpdateTitleInput String
    | RouteTo Route


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        RouteTo nextRoute ->
            ( { model
                | route = nextRoute
              }
            , Cmd.none
            )

        UpdateTitleInput title ->
            ( { model
                | currentTitle = title
              }
            , Cmd.none
            )

        AddClothing ->
            ( { model
                | clothing = model.clothing ++ [ model.currentTitle ]
                , route = WatchList
              }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )



-- view


view : Model -> Html Msg
view model =
    let
        mainDisplay =
            case model.route of
                WatchList ->
                    clothingWatchList model

                AddNewClothing ->
                    addingClothingForm
    in
        div [ mainContainerStyle ]
            [ addClothingToWatchButton
            , mainDisplay |> mainContentContainer
            ]


mainContentContainer : Html Msg -> Html Msg
mainContentContainer mainDisplay =
    div
        [ style [ width (pc 100) ]
        ]
        [ header
        , mainDisplay
        ]


addClothingToWatchButton : Html Msg
addClothingToWatchButton =
    styleButtonStates addClothingButtonInitialStyle
        addClothingButtonActiveStyle
        addClothingButtonHoverStyle
        button
        [ addClothingButtonStyle
        , onClick (RouteTo AddNewClothing)
        ]
        [ text "+" ]


header : Html Msg
header =
    div [ style [ textAlign center ] ]
        [ text "Clozet" ]


addingClothingForm : Html Msg
addingClothingForm =
    div [ id "watch_clothing" ]
        [ clothingTitleField
        , button [ onClick AddClothing ] [ text "Watch" ]
        ]


clothingTitleField : Html Msg
clothingTitleField =
    input
        [ placeholder "Clothing Description"
        , onInput UpdateTitleInput
        ]
        []


clothingWatchList : Model -> Html Msg
clothingWatchList model =
    ul [ id "watch" ] (List.map (\l -> li [] [ text l ]) model.clothing)



-- styles


mainContainerStyle : Attribute Msg
mainContainerStyle =
    style
        [ display flex'
        , width (pc 100)
        , height (pc 100)
        ]


addClothingButtonStyle : Attribute Msg
addClothingButtonStyle =
    style
        [ width (px 200)
        , height (pc 100)
        , border none
        , color "#fff"
        , backgroundColor "#6496c8"
        , textShadow "-1px 1px #417cb8"
        , fontSize (px 50)
        ]


addClothingButtonInitialStyle : List ( String, String )
addClothingButtonInitialStyle =
    [ backgroundColor "#6496c8"
    , textShadow "-1px 1px #417cb8"
    ]


addClothingButtonHoverStyle : List ( String, String )
addClothingButtonHoverStyle =
    [ backgroundColor "#346392"
    , textShadow "-1px 1px #27496d"
    ]


addClothingButtonActiveStyle : List ( String, String )
addClothingButtonActiveStyle =
    [ backgroundColor "#27496d"
    , textShadow "-1px 1px #193047"
    ]
