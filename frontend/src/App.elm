module App
    exposing
        ( init
        , update
        , view
        )

import Html exposing (..)
import Style exposing (..)
import Html.App as App
import Html.Attributes exposing (style, placeholder, id)
import Html.Events exposing (onInput, onClick)
import Component.Button exposing (primaryButton, secondaryButton)
import Component.Field


type alias Model =
    { clothing : List String
    , addClothingTextField : Component.Field.Model
    , route : Route
    }


init : ( Model, Cmd Msg )
init =
    ( { clothing = []
      , addClothingTextField =
            Component.Field.init (Just "Clothing Description")
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
    | UpdateClothingTextField Component.Field.Msg
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

        AddClothing ->
            ( { model
                | clothing = model.clothing ++ [ Component.Field.currentInput model.addClothingTextField ]
                , route = WatchList
              }
            , Cmd.none
            )

        UpdateClothingTextField msg ->
            ( { model
                | addClothingTextField = Component.Field.update msg model.addClothingTextField
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
        page =
            case model.route of
                WatchList ->
                    clothingWatchList model

                AddNewClothing ->
                    addingClothingForm model
    in
        page |> mainContentContainer


mainContentContainer : Html Msg -> Html Msg
mainContentContainer mainDisplay =
    div
        [ style
            [ width (pc 100)
            , height (pc 100)
            ]
        ]
        [ header
        , mainDisplay
        ]


header : Html Msg
header =
    div [ style [ textAlign center ] ]
        [ text "Clozet" ]


addingClothingForm : Model -> Html Msg
addingClothingForm model =
    div [ id "watch_clothing" ]
        [ App.map UpdateClothingTextField (Component.Field.view model.addClothingTextField)
        , secondaryButton AddClothing "Watch"
        ]


clothingWatchList : Model -> Html Msg
clothingWatchList model =
    div [ mainContainerStyle ]
        [ primaryButton (RouteTo AddNewClothing) "+"
        , ul [ id "watch" ] (List.map (\l -> li [] [ text l ]) model.clothing)
        ]



-- styles


mainContainerStyle : Attribute Msg
mainContainerStyle =
    style
        [ display flex'
        , width (pc 100)
        , height (pc 100)
        ]
