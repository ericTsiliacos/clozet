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
    , page : Page
    }


init : ( Model, Cmd Msg )
init =
    ( { clothing = []
      , addClothingTextField =
            Component.Field.init (Just "Clothing Description")
      , page = WatchList
      }
    , Cmd.none
    )


type Page
    = WatchList
    | AddNewClothing


type Msg
    = NoOp
    | AddClothing
    | UpdateClothingTextField Component.Field.Msg
    | NavigateTo Page


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NavigateTo nextPage ->
            ( { model
                | page = nextPage
              }
            , Cmd.none
            )

        AddClothing ->
            ( { model
                | clothing = model.clothing ++ [ Component.Field.currentInput model.addClothingTextField ]
                , page = WatchList
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
            case model.page of
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
        [ primaryButton (NavigateTo AddNewClothing) "+"
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
