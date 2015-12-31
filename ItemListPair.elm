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
            | TogglePin

update : Action -> Model -> Model
update action model =
  case action of
    TodoList subAction ->
      case subAction of
        ItemList.SubAction id subSubAction ->
          case subSubAction of
            -- TODO
            Item.TogglePin -> { model | todoList = let changedTodoList = ItemList.update subAction model.todoList
                                                       (currentId, _) = getSelectedItem model
                                                       (newSelectedId, _) = getSelectedItem model
                                                       updatedTodoList = ItemList.update (ItemList.SubAction currentId Item.ToggleSelect) changedTodoList
                                                       newTodoList = ItemList.update (ItemList.SubAction newSelectedId Item.ToggleSelect) updatedTodoList
                                                   in ItemList.update ItemList.SortNewWithPin newTodoList }
{-                                                   let updatedTodoList = ItemList.update subAction model.todoList
                                                   in { items = (ItemList.update ItemList.SortNewWithPin updatedTodoList).items,
                                                        nextItemId = (model.todoList).nextItemId }}
-}

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
            Item.TogglePin -> { model | doneList = let changedDoneList = ItemList.update subAction model.doneList
                                                       (currentId, _) = getSelectedItem model
                                                       (newSelectedId, _) = getSelectedItem model
                                                       updatedDoneList = ItemList.update (ItemList.SubAction currentId Item.ToggleSelect) changedDoneList
                                                       newDoneList = ItemList.update (ItemList.SubAction newSelectedId Item.ToggleSelect) updatedDoneList
                                                   in ItemList.update ItemList.SortNewWithPin newDoneList }

{-                                                   let updatedDoneList = ItemList.update subAction model.doneList
                                                   in { items = (ItemList.update ItemList.SortNewWithPin updatedDoneList).items,
                                                        nextItemId = (model.doneList).nextItemId }}
-}


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

    SelectNext -> { model | todoList = let (nextId, _) = getNextItem model
                                           (currentId, _) = getSelectedItem model
                                       in let updatedTodoList =
                                            if getSelectedItemList model
                                            then ItemList.update (ItemList.SubAction currentId Item.ToggleSelect) model.todoList
                                            else model.todoList
                                          in if getNextItemList model
                                             then ItemList.update (ItemList.SubAction nextId Item.ToggleSelect) updatedTodoList
                                             else updatedTodoList,
                            doneList = let (nextId, _) = getNextItem model
                                           (currentId, _) = getSelectedItem model
                                       in let updatedDoneList =
                                            if getSelectedItemList model
                                            then model.doneList
                                            else ItemList.update (ItemList.SubAction currentId Item.ToggleSelect) model.doneList
                                          in if getNextItemList model
                                             then updatedDoneList
                                             else ItemList.update (ItemList.SubAction nextId Item.ToggleSelect) updatedDoneList,
                            selected = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                                                               in (model.selected + 1)%totalLength }

    SelectPrevious -> { model | todoList = let (previousId, _) = getPreviousItem model
                                               (currentId, _) = getSelectedItem model
                                           in let updatedTodoList =
                                                if getSelectedItemList model
                                                then ItemList.update (ItemList.SubAction currentId Item.ToggleSelect) model.todoList
                                                else model.todoList
                                              in if getPreviousItemList model
                                                 then ItemList.update (ItemList.SubAction previousId Item.ToggleSelect) updatedTodoList
                                                 else updatedTodoList,
                                doneList = let (previousId, _) = getPreviousItem model
                                               (currentId, _) = getSelectedItem model
                                           in let updatedDoneList =
                                                if getSelectedItemList model
                                                then model.doneList
                                                else ItemList.update (ItemList.SubAction currentId Item.ToggleSelect) model.doneList
                                              in if getPreviousItemList model
                                                 then updatedDoneList
                                                 else ItemList.update (ItemList.SubAction previousId Item.ToggleSelect) updatedDoneList,
                                selected = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                                                                       in if model.selected%totalLength == 0
                                                                          then (totalLength - 1)
                                                                          else (model.selected - 1)%totalLength }

    ToggleVisibilityDone -> { model | visibilityDone = not model.visibilityDone}

    TogglePin -> let (currentId, _) = getSelectedItem model
                 in if getSelectedItemList model
                    then update (TodoList (ItemList.SubAction currentId Item.TogglePin)) model
                    else update (DoneList (ItemList.SubAction currentId Item.TogglePin)) model





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
