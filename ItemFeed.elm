module ItemFeed where

import Signal
import Html exposing ( Html )
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ItemList
import Item
import Date


type alias Model =
  { todoList : ItemList.Model
  , doneList : ItemList.Model
  , reminderField : String
  , reminderDate : String
  }

init : Model
init =
  { todoList = ItemList.init
  , doneList = ItemList.initEmpty
  , reminderField = ""
  , reminderDate = "01-01-2015"
  }

-- UPDATE

type Action = TodoList ItemList.Action
            | DoneList ItemList.Action
            | SaveContent String
            | SaveDate String

help : ItemList.Id -> List (ItemList.Id, Item.Model) -> Item.Model
help id list =
  case list of
    (x, y)::rest -> if x==id then y else help id rest
    [] -> Item.dummyItem

update : Action -> Model -> Model
update action model =
  case action of
    TodoList subAction ->
      case subAction of
        ItemList.SubAction id subSubAction ->
          case subSubAction of
            Item.Pin -> { model | todoList = ItemList.update subAction model.todoList }
            Item.Unpin -> { model | todoList = ItemList.update subAction model.todoList }
            Item.MarkAsDone -> { model | doneList = ItemList.update subAction (ItemList.update (ItemList.Add (help id ((model.todoList).items)))),
                                         todoList = ItemList.update (ItemList.Remove id) }
            Item.MarkUndone -> { model | todoList = ItemList.update subAction model.todoList }
            _ -> { model | todoList = ItemList.update subAction model.todoList }

        _ -> { model | todoList = ItemList.update subAction model.todoList }

--      { model | todoList = ItemList.update subAction model.todoList }
    DoneList subAction -> { model | doneList = ItemList.update subAction model.doneList }
    SaveContent string -> { model | reminderField = string }
    SaveDate date -> { model | reminderDate = date }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
      [ if List.length ((model.todoList).items) == 0
        then Html.p [] []
        else Html.h1 [] [Html.text "To do"]
      , ItemList.view (Signal.forwardTo address TodoList) (model.todoList)
      , if List.length ((model.doneList).items) == 0
        then Html.p [] []
        else Html.h1 [] [Html.text "Done"]
      , ItemList.view (Signal.forwardTo address DoneList) (model.doneList)
      , Html.p [] []
      , Html.h1 [] [Html.text "Reminder"]
      , Html.input
          [ placeholder "New Reminder"
            , on "input" targetValue (\str -> Signal.message address (SaveContent str))
            , type' "text"
            , value model.reminderField
          ] []
      , Html.input
          [ type' "date"
            , on "input" targetValue (\date -> Signal.message address (SaveDate date))
            , value model.reminderDate
          ] []
      , Html.button [ onClick address (TodoList (ItemList.Add (Item.newReminder model.reminderField model.reminderDate)))] [ Html.text "Add" ]
      ]
