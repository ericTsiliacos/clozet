import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)

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
  , button [onClick address AddClothing] [ text  "Watch" ]
  ]

clothingTitleField : Signal.Address Action -> Html
clothingTitleField address =
  input
  [ placeholder "Clothing Description"
  , onInput address UpdateTitleInput
  ] []

clothingWatchList : Model -> Html
clothingWatchList model =
  ul [id "watch"] (List.map (\l -> li [] [ text l ]) model.clothing)

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

initialModel : Model
initialModel = { clothing = [], currentTitle = "" }

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

model : Signal Model
model = Signal.foldp update initialModel actions.signal

main : Signal Html
main = Signal.map (view actions.address) model

