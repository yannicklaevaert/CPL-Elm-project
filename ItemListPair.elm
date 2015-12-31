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

getNextSelected : Model -> Int
getNextSelected model = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                        in (model.selected+1)%totalLength

getPreviousSelected : Model -> Int
getPreviousSelected model = let totalLength = (List.length (model.todoList).items + List.length (model.doneList).items)
                            in if (model.selected%totalLength) == 0
                               then totalLength - 1
                               else (model.selected-1)%totalLength

getItem : Int -> Model -> (ItemList.Id, Item.Model)
getItem n model = if n < List.length (model.todoList).items
                  then ItemList.getItem n (model.todoList)
                  else ItemList.getItem (n - List.length (model.todoList).items) (model.doneList)

-- UPDATE

type Action = TodoList ItemList.Action
            | DoneList ItemList.Action
            | SelectNext
            | SelectPrevious
            | ToggleVisibilityDone
            | TogglePin
            | ToggleDone

update : Action -> Model -> Model
update action model =
  case action of
    TodoList subAction ->
      case subAction of

        ItemList.SubAction id subSubAction ->
          case subSubAction of

            Item.TogglePin -> { model | todoList = let changedTodoList = ItemList.update subAction model.todoList
                                                       updatedTodoList = ItemList.update ItemList.SortNewWithPin changedTodoList
                                                       (newSelectedId, _) = ItemList.getItem model.selected updatedTodoList
                                                       adaptedTodoList = ItemList.update (ItemList.SubAction id Item.ToggleSelect) updatedTodoList
                                                   in ItemList.update (ItemList.SubAction newSelectedId Item.ToggleSelect) adaptedTodoList }

            Item.ToggleDone -> let updatedPair = { model | doneList = let updatedTodoList = ItemList.update subAction model.todoList
                                                                      in ItemList.update (ItemList.AddItem id (findItemWithId id (updatedTodoList.items))) model.doneList,
                                                           todoList = ItemList.update (ItemList.Remove id) model.todoList,
                                                           visibilityDone = if List.isEmpty (model.doneList).items
                                                                            then True
                                                                            else model.visibilityDone }
                               in let updatedNewPair = update (DoneList (ItemList.SubAction id Item.ToggleSelect)) updatedPair
                                      previousSelected = getPreviousSelected updatedPair
                                      (newSelectedId, _) = getItem previousSelected updatedNewPair
                                      adaptedPair = if previousSelected < List.length (updatedNewPair.todoList).items
                                                    then update (TodoList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                                    else update (DoneList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                  in { model | todoList = adaptedPair.todoList,
                                               doneList = adaptedPair.doneList,
                                               selected = previousSelected,
                                               visibilityDone = adaptedPair.visibilityDone}

            -- Truncate or disabletruncate just updates the item in this list (Toggle select is not a button, not handled here)
            _ -> { model | todoList = ItemList.update subAction model.todoList }

        -- Only the subaction AddNew is possible (when a new reminder is added)
        ItemList.AddNew item -> { model | todoList = let changedTodoList = ItemList.update (ItemList.SubAction model.selected Item.ToggleSelect) model.todoList
                                                         updatedTodoList = ItemList.update subAction changedTodoList
                                                         adaptedTodoList = ItemList.update ItemList.SortNewWithPin updatedTodoList
                                                         (newSelectedId, _) = ItemList.getItem model.selected adaptedTodoList
                                                     in ItemList.update (ItemList.SubAction newSelectedId Item.ToggleSelect) adaptedTodoList }

        -- No action possible, default.
        _ -> { model | todoList = ItemList.update subAction model.todoList }


    DoneList subAction ->
      case subAction of
        ItemList.SubAction id subSubAction ->
          case subSubAction of
            -- TODO
            Item.TogglePin -> { model | doneList = let changedDoneList = ItemList.update subAction model.todoList
                                                       updatedDoneList = ItemList.update ItemList.SortNewWithPin changedDoneList
                                                       (newSelectedId, _) = ItemList.getItem model.selected updatedDoneList
                                                       adaptedDoneList = ItemList.update (ItemList.SubAction id Item.ToggleSelect) updatedDoneList
                                                   in ItemList.update (ItemList.SubAction newSelectedId Item.ToggleSelect) adaptedDoneList }

            Item.ToggleDone -> let updatedPair = { model | todoList = let updatedDoneList = ItemList.update subAction model.doneList
                                                                      in ItemList.update (ItemList.AddItem id (findItemWithId id updatedDoneList.items)) model.todoList,
                                                           doneList = ItemList.update (ItemList.Remove id) model.doneList,
                                                           visibilityDone = if List.length ((model.doneList).items) == 1
                                                                            then False
                                                                            else model.visibilityDone }
                               in let updatedNewPair = update (TodoList (ItemList.SubAction id Item.ToggleSelect)) updatedPair
                                      nextSelected = getNextSelected updatedPair
                                      (newSelectedId, _) = getItem nextSelected updatedNewPair
                                      adaptedPair = if nextSelected < List.length (updatedNewPair.todoList).items
                                                    then update (TodoList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                                    else update (DoneList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                  in { model | todoList = adaptedPair.todoList,
                                               doneList = adaptedPair.doneList,
                                               selected = nextSelected,
                                               visibilityDone = adaptedPair.visibilityDone}

            -- Truncate or disabletruncate just updates the item in this list
            _ -> { model | doneList = ItemList.update subAction model.doneList }

        -- No action possible, default.
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


    ToggleDone -> let (currentId, _) = getSelectedItem model
                  in if getSelectedItemList model
                     then update (TodoList (ItemList.SubAction currentId Item.ToggleDone)) model
                     else update (DoneList (ItemList.SubAction currentId Item.ToggleDone)) model


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
        [ if not model.visibilityDone
          then Html.p [] []
          else Html.div []
               [ Html.h1 [] [Html.text "Done"]
               , Html.div [ A.style [("opacity", "0.5")]]
               [ ItemList.view (Signal.forwardTo address DoneList) (model.doneList) ]
               ]
        ]
      ]
