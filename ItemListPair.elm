module ItemListPair where

import Signal
import Html exposing ( Html )
import Html.Attributes as A
import ItemList
import Item

type alias Model =
  { todoList : ItemList.Model
  , doneList : ItemList.Model
  , selected : Int
  , visibilityDone : Bool
  }

init : Model
init =
  { todoList = let initTodoList = ItemList.init
               in ItemList.update (ItemList.SubAction 0 Item.ToggleSelect) initTodoList
  , doneList = ItemList.initEmpty
  , selected = 0
  , visibilityDone = False
  }

findItemWithId : ItemList.Id -> List (ItemList.Id, Item.Model) -> Item.Model
findItemWithId id list =
  case list of
    (x, y)::rest -> if x==id then y else findItemWithId id rest
    [] -> Item.dummyItem

getSelectedItemList : Model -> Bool
getSelectedItemList model = if model.selected >= List.length (model.todoList).items
                            then False -- selected item in doneList
                            else True -- selected item in todoList

getSelectedItem : Model -> (ItemList.Id, Item.Model)
getSelectedItem model = if model.selected >= List.length (model.todoList).items
                        then ItemList.getItem (model.selected - (List.length (model.todoList).items)) (model.doneList)
                        else ItemList.getItem model.selected (model.todoList)

getPreviousItemList : Model -> Bool
getPreviousItemList model = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                            in if (model.selected%totalLength == 0)
                               then if (List.length (model.doneList).items == 0)
                                    then True -- selected item in todoList
                                    else False -- selected item in doneList
                               else if ((model.selected-1)%totalLength) < List.length (model.todoList).items
                                    then True -- selected item in todoList
                                    else False -- selected item in doneList

getPreviousItem : Model -> (ItemList.Id, Item.Model)
getPreviousItem model = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                        in if (model.selected%totalLength == 0)
                           then if (List.length (model.doneList).items == 0)
                                then ItemList.getItem (totalLength - 1) (model.todoList)
                                else ItemList.getItem (totalLength - 1 - (List.length (model.todoList).items)) (model.doneList)
                           else if ((model.selected-1)%totalLength) < List.length (model.todoList).items
                                then ItemList.getItem (model.selected - 1) (model.todoList)
                                else ItemList.getItem (model.selected - 1 - (List.length (model.todoList).items)) (model.doneList)

getNextItemList : Model -> Bool
getNextItemList model = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                        in if (model.selected+1)%totalLength >= List.length (model.todoList).items
                           then False -- selected item in doneList
                           else True -- selected item in todoList

getNextItem : Model -> (ItemList.Id, Item.Model)
getNextItem model = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                    in if (model.selected+1)%totalLength >= List.length (model.todoList).items
                       then ItemList.getItem (model.selected + 1 - (List.length (model.todoList).items)) (model.doneList)
                       else ItemList.getItem ((model.selected + 1)%totalLength) (model.todoList)

-- UPDATE

type Action = TodoList ItemList.Action
            | DoneList ItemList.Action
            | SelectNext
            | SelectPrevious
            | ToggleVisibilityDone

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
                                                    in ItemList.update (ItemList.AddItem id (findItemWithId id (updatedTodoList.items))) model.doneList,
                                         todoList = ItemList.update (ItemList.Remove id) model.todoList,
                                         visibilityDone = if List.isEmpty (model.doneList).items
                                                          then True
                                                          else model.visibilityDone }
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
                                                    in ItemList.update (ItemList.AddItem id (findItemWithId id updatedDoneList.items)) model.todoList,
                                         doneList = ItemList.update (ItemList.Remove id) model.doneList,
                                         visibilityDone = if List.length ((model.doneList).items) == 1
                                                          then False
                                                          else model.visibilityDone }

            -- Truncate or disabletruncate just updates the item in this list
            _ -> { model | doneList = ItemList.update subAction model.doneList }
        -- Only the subaction add is possible when a new reminder is added
        _ -> { model | doneList = ItemList.update subAction model.doneList }

    SelectNext -> { model | selected = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                                       in (model.selected + 1)%totalLength}

    SelectPrevious -> { model | selected = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                                           in if model.selected%totalLength == 0
                                              then (totalLength - 1)
                                              else (model.selected - 1)%totalLength}

    ToggleVisibilityDone -> { model | visibilityDone = not model.visibilityDone}

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
--        [ if List.length ((model.doneList).items) == 0
        [ if not model.visibilityDone
          then Html.p [] []
          else Html.div []
               [ Html.h1 [] [Html.text "Done"]
               , Html.div [ A.style [("opacity", "0.5")]]
               [ ItemList.view (Signal.forwardTo address DoneList) (model.doneList) ]
               ]
        ]
      ]
