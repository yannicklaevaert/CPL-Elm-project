module ItemListPair where

import Signal
import Html exposing ( Html )
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import ItemList

type alias Model = (ItemList.Model, ItemList.Model)

init : Model
init = (ItemList.init, ItemList.initEmpty)

-- UPDATE

type Action = TodoList ItemList.Action
            | DoneList ItemList.Action

update : Action -> Model -> Model
update action (todoList, doneList) =
  case action of
    TodoList subAction -> (ItemList.update subAction todoList, doneList)
    DoneList subAction -> (todoList, ItemList.update subAction doneList)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
      [ if List.length ((fst model).items) == 0
        then Html.p [] []
        else Html.h1 [] [Html.text "To do"]
      , ItemList.view (Signal.forwardTo address TodoList) (fst model)
      , if List.length ((snd model).items) == 0
        then Html.p [] []
        else Html.h1 [] [Html.text "Done"]
      , ItemList.view (Signal.forwardTo address DoneList) (snd model)
      ]
