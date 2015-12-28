module ItemListPair where

import Signal
import Html exposing ( Html )
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import ItemList
import Item

type alias Model =
  { todoList : ItemList.Model
  , doneList : ItemList.Model
  , selected : Int
  }

init : Model
init =
  { todoList = let initTodoList = ItemList.init
               in ItemList.update (ItemList.SubAction 0 Item.Select) initTodoList
  , doneList = ItemList.initEmpty
  , selected = 1
  }
-- UPDATE

type Action = TodoList ItemList.Action
            | DoneList ItemList.Action
            | SortOldWithoutPin
            | SortNewWithPin
            | UpdateSelectedItem Item.Action

help : ItemList.Id -> List (ItemList.Id, Item.Model) -> Item.Model
help id list =
  case list of
    (x, y)::rest -> if x==id then y else help id rest
    [] -> Item.dummyItem

getSelectedItemList : Model -> Bool
getSelectedItemList model = if model.selected > List.length (model.todoList).items
                            then False -- selected item in doneList
                            else True -- selected item in todoList

update : Action -> Model -> Model
update action model =
  case action of
    TodoList subAction ->
      case subAction of
        ItemList.SubAction id subSubAction ->
          case subSubAction of
            -- TODO
            Item.TogglePin -> { model | todoList = let updatedTodoList = ItemList.update subAction model.todoList
                                                   in { items = (ItemList.update ItemList.SortNewWithPin updatedTodoList).items,
                                                        nextItemId = (model.todoList).nextItemId }}
            -- TODO Not totally correct, sometimes button is changed to undo, sometimes it stays mark as done
--            Item.MarkAsDone -> { model | doneList = let tempDoneList = (ItemList.update (ItemList.Add (help id ((model.todoList).items))) model.doneList)
--                                                    in ItemList.update subAction tempDoneList,
--                                         todoList = ItemList.update (ItemList.Remove id) model.todoList }


{-            -- Think it is correct now
            -- When marked as done, item is updated in todolist, added to donelist and removed from todolist
            Item.MarkAsDone -> { model | doneList = let updatedTodoList = ItemList.update subAction model.todoList
                                                    in ItemList.update (ItemList.Add (help id (updatedTodoList.items))) model.doneList,
                                         todoList = ItemList.update (ItemList.Remove id) model.todoList }
            -- Not Possible because when marked done the item is not in this list
            Item.MarkUndone -> model
-}
            Item.ToggleDone -> { model | doneList = let updatedTodoList = ItemList.update subAction model.todoList
                                                    in ItemList.update (ItemList.AddItem id (help id (updatedTodoList.items))) model.doneList,
                                         todoList = ItemList.update (ItemList.Remove id) model.todoList }
            -- Truncate or disabletruncate just updates the item in this list
            _ -> { model | todoList = ItemList.update subAction model.todoList }
        -- Only the subaction add is possible when a new reminder is added
        _ -> { model | todoList = ItemList.update subAction model.todoList }

    DoneList subAction ->
      case subAction of
        ItemList.SubAction id subSubAction ->
          case subSubAction of
            -- TODO
            Item.TogglePin -> { model | doneList = let updatedDoneList = ItemList.update subAction model.doneList
                                                   in { items = (ItemList.update ItemList.SortNewWithPin updatedDoneList).items,
                                                        nextItemId = (model.doneList).nextItemId }}
            -- TODO Not totally correct, sometimes button is changed to undo, sometimes it stays mark as done
--            Item.MarkUndone -> { model | todoList = let tempTodoList = (ItemList.update (ItemList.Add (help id ((model.doneList).items))) model.todoList)
--                                                    in ItemList.update subAction tempTodoList,
--                                         doneList = ItemList.update (ItemList.Remove id) model.doneList }


{-            -- Think it is correct now
            -- Not Possible because when marked undone the item is not in this list
            Item.MarkAsDone -> model
            -- When marked as done, item is updated in todolist, added to donelist and removed from todolist
            Item.MarkUndone -> { model | todoList = let updatedDoneList = ItemList.update subAction model.doneList
                                                    in ItemList.update (ItemList.Add (help id (updatedDoneList.items))) model.todoList,
                                         doneList = ItemList.update (ItemList.Remove id) model.doneList }
-}
            Item.ToggleDone -> { model | todoList = let updatedDoneList = ItemList.update subAction model.doneList
                                                    in ItemList.update (ItemList.AddItem id (help id updatedDoneList.items)) model.todoList,
                                         doneList = ItemList.update (ItemList.Remove id) model.doneList }

            -- Truncate or disabletruncate just updates the item in this list
            _ -> { model | doneList = ItemList.update subAction model.doneList }
        -- Only the subaction add is possible when a new reminder is added
        _ -> { model | doneList = ItemList.update subAction model.doneList }
    SortOldWithoutPin -> { model | todoList = ItemList.update ItemList.SortOldWithoutPin model.todoList,
                                   doneList = ItemList.update ItemList.SortOldWithoutPin model.doneList }

    SortNewWithPin -> { model | todoList = ItemList.update ItemList.SortNewWithPin model.todoList,
                                doneList = ItemList.update ItemList.SortNewWithPin model.doneList }

    UpdateSelectedItem subAction ->
      case subAction of
        Item.ToggleDone -> if getSelectedItemList model
                           then let (id, _) = ItemList.getItem model.selected model.todoList
                                in { model | doneList = let updatedTodoList = ItemList.update (ItemList.SubAction id subAction) model.todoList
                                                        in ItemList.update (ItemList.AddItem id (help id updatedTodoList.items)) model.doneList,
                                             todoList = ItemList.update (ItemList.Remove id) model.todoList }
                           else let (id, _) = ItemList.getItem model.selected model.doneList
                                in { model | todoList = let updatedDoneList = ItemList.update (ItemList.SubAction id subAction) model.doneList
                                                        in ItemList.update (ItemList.AddItem id (help id updatedDoneList.items)) model.todoList,
                                             doneList = ItemList.update (ItemList.Remove id) model.doneList }

        _ -> if getSelectedItemList model
             then let (id, _) = ItemList.getItem model.selected model.todoList
                  in { model | todoList = ItemList.update (ItemList.SubAction id subAction) model.todoList }
             else let (id, _) = ItemList.getItem model.selected model.doneList
                  in { model | doneList = ItemList.update (ItemList.SubAction id subAction) model.doneList }
-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
      [ Html.div []
        [ if List.length ((model.todoList).items) == 0
          then Html.p [] []
          else Html.h1 [] [Html.text "To do"]
          , ItemList.view (Signal.forwardTo address TodoList) (model.todoList)
        ]
      , Html.div []
        [ if List.length ((model.doneList).items) == 0
          then Html.p [] []
          else Html.h1 [] [Html.text "Done"]
          , ItemList.view (Signal.forwardTo address DoneList) (model.doneList)
        ]
      ]
