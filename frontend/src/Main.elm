import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import StartApp.Simple as StartApp

type alias Model = { clothing : List String, currentTitle : String }

type Action = AddClothing | UpdateTitleInput String | NoOp

update : Action -> Model -> Model
update action model =
  case action of
    UpdateTitleInput title -> { model | currentTitle = title }
    AddClothing -> { model | clothing = model.clothing ++ [model.currentTitle] }
    NoOp -> model

view : Signal.Address Action -> Model -> Html
view address model =
  div []
  [
    navigation
  , addingClothingForm address model
  , clothingWatchList model
  ]

navigation : Html
navigation =
  div []
  [
    button [id "add"] [ text "+" ]
  ]

addingClothingForm : Signal.Address Action -> Model -> Html
addingClothingForm address model =
  div []
  [
    clothingTitleField address
  , button [onClick address AddClothing] [ text  "Save" ]
  ]

clothingTitleField : Signal.Address Action -> Html
clothingTitleField address =
  input
  [ placeholder "Clothing Description"
  , on "input" targetValue (\str -> Signal.message address (UpdateTitleInput str))
  ] []

clothingWatchList : Model -> Html
clothingWatchList model =
  ul [id "watch"] (List.map (\l -> li [] [ text l ]) model.clothing)

main : Signal Html
main = StartApp.start { model = { clothing = [], currentTitle = "" }, view = view, update = update }

