import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)

type Route = WatchList | AddNewClothing

type alias Model = { clothing : List String, currentTitle : String, route : Route }

type Action = NoOp | AddClothing | UpdateTitleInput String | RouteTo Route

update : Action -> Model -> Model
update action model =
  case action of
    RouteTo nextRoute -> { model | route = nextRoute }
    UpdateTitleInput title -> { model | currentTitle = title }
    AddClothing -> {
      model | clothing = model.clothing ++ [model.currentTitle], route = WatchList
    }
    NoOp -> model

view : Signal.Address Action -> Model -> Html
view address model =
  let mainDisplay =
    case model.route of
      WatchList -> clothingWatchList model
      AddNewClothing -> addingClothingForm address
  in div []
     [
       navigation address
     , mainDisplay
     ]

navigation : Signal.Address Action -> Html
navigation address =
  div []
  [
    button [id "add"
           , onClick address (RouteTo AddNewClothing)
           ]
           [ text "+" ]
  ]

addingClothingForm : Signal.Address Action -> Html
addingClothingForm address =
  div [id "watch_clothing"]
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
initialModel = { clothing = [], currentTitle = "", route = WatchList }

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

model : Signal Model
model = Signal.foldp update initialModel actions.signal

main : Signal Html
main = Signal.map (view actions.address) model

