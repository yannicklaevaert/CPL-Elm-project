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
            -- TODO
            Item.Pin -> { model | todoList = ItemList.update subAction model.todoList }
            -- TODO
            Item.Unpin -> { model | todoList = ItemList.update subAction model.todoList }
            -- TODO Not totally correct, sometimes button is changed to undo, sometimes it stays mark as done
--            Item.MarkAsDone -> { model | doneList = let tempDoneList = (ItemList.update (ItemList.Add (help id ((model.todoList).items))) model.doneList)
--                                                    in ItemList.update subAction tempDoneList,
--                                         todoList = ItemList.update (ItemList.Remove id) model.todoList }
            -- Think it is correct now
            -- When marked as done, item is updated in todolist, added to donelist and removed from todolist
            Item.MarkAsDone -> { model | doneList = let updatedTodoList = ItemList.update subAction model.todoList
                                                    in ItemList.update (ItemList.Add (help id (updatedTodoList.items))) model.doneList,
                                         todoList = ItemList.update (ItemList.Remove id) model.todoList }
            -- Not Possible because when marked done the item is not in this list
            Item.MarkUndone -> model
            -- Truncate or disabletruncate just updates the item in this list
            _ -> { model | todoList = ItemList.update subAction model.todoList }
        -- Only the subaction add is possible when a new reminder is added
        _ -> { model | todoList = ItemList.update subAction model.todoList }

    DoneList subAction ->
      case subAction of
        ItemList.SubAction id subSubAction ->
          case subSubAction of
            -- TODO
            Item.Pin -> { model | doneList = ItemList.update subAction model.doneList }
            -- TODO
            Item.Unpin -> { model | doneList = ItemList.update subAction model.doneList }
            -- Not Possible because when marked undone the item is not in this list
            Item.MarkAsDone -> model
            -- TODO Not totally correct, sometimes button is changed to undo, sometimes it stays mark as done
--            Item.MarkUndone -> { model | todoList = let tempTodoList = (ItemList.update (ItemList.Add (help id ((model.doneList).items))) model.todoList)
--                                                    in ItemList.update subAction tempTodoList,
--                                         doneList = ItemList.update (ItemList.Remove id) model.doneList }
            -- Think it is correct now
            -- When marked as done, item is updated in todolist, added to donelist and removed from todolist
            Item.MarkUndone -> { model | todoList = let updatedDoneList = ItemList.update subAction model.doneList
                                                    in ItemList.update (ItemList.Add (help id (updatedDoneList.items))) model.todoList,
                                         doneList = ItemList.update (ItemList.Remove id) model.doneList }
            -- Truncate or disabletruncate just updates the item in this list
            _ -> { model | doneList = ItemList.update subAction model.doneList }
        -- Only the subaction add is possible when a new reminder is added
        _ -> { model | doneList = ItemList.update subAction model.doneList }

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
