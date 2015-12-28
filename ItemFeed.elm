module ItemFeed where

import Signal
import Html exposing ( Html )
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ItemList
import Item
import Json.Decode as Json
import Set
import Char


type alias Model =
  { todoList : ItemList.Model
  , doneList : ItemList.Model
  , reminderField : String
  , reminderDate : String
  , selected : Int
  }

init : Model
init =
  { todoList = let initTodoList = ItemList.init
               in ItemList.update (ItemList.SubAction 0 Item.Select) initTodoList
  , doneList = ItemList.initEmpty
  , reminderField = ""
  , reminderDate = "2015-01-01"
  , selected = 0
  }

-- UPDATE

type Action = TodoList ItemList.Action
            | DoneList ItemList.Action
            | SaveContent String
            | SaveDate String
            | KeyPress Bool (Set.Set (Char.KeyCode))

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
            Item.TogglePin -> { model | todoList = let updatedTodoList = ItemList.update subAction model.todoList
                                                   in { items = (ItemList.sortPinnedUnpinned updatedTodoList.items),
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
                                                    in ItemList.update (ItemList.Add (help id (updatedTodoList.items))) model.doneList,
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
                                               in { items = (ItemList.sortPinnedUnpinned updatedDoneList.items),
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
                                                    in ItemList.update (ItemList.Add (help id (updatedDoneList.items))) model.todoList,
                                         doneList = ItemList.update (ItemList.Remove id) model.doneList }

            -- Truncate or disabletruncate just updates the item in this list
            _ -> { model | doneList = ItemList.update subAction model.doneList }
        -- Only the subaction add is possible when a new reminder is added
        _ -> { model | doneList = ItemList.update subAction model.doneList }

    SaveContent string -> { model | reminderField = string }

    SaveDate date -> { model | reminderDate = date }

    KeyPress altPressed keyCodes ->
      if altPressed
      -- "s" has keycode 83
      then if Set.member 83 keyCodes
           then { model | todoList = ItemList.sortOldNoPin model.todoList,
                          doneList = ItemList.sortOldNoPin model.doneList }
      -- "o" has keycode 79
           else if Set.member 79 keyCodes
           then { model | todoList = if getSelectedItemList model
                                     then let (id, _) = ItemList.getItem model.selected model.todoList
                                          in ItemList.update (ItemList.SubAction id Item.ToggleTruncate) model.todoList
                                     else model.todoList,
                          doneList = if getSelectedItemList model
                                     then model.doneList
                                     else let (id, _) = ItemList.getItem (model.selected - (List.length (model.todoList).items - 1)) model.doneList
                                          in ItemList.update (ItemList.SubAction id Item.ToggleTruncate) model.doneList }
      -- "p" has keycode 80
           else if Set.member 80 keyCodes
           then { model | todoList = if getSelectedItemList model
                                     then let (id, _) = ItemList.getItem model.selected model.todoList
                                          in ItemList.update (ItemList.SubAction id Item.TogglePin) model.todoList
                                     else model.todoList,
                          doneList = if getSelectedItemList model
                                     then model.doneList
                                     else let (id, _) = ItemList.getItem (model.selected - (List.length (model.todoList).items - 1)) model.doneList
                                          in ItemList.update (ItemList.SubAction id Item.TogglePin) model.doneList }
      -- "x" has keycode 88
-- TODO KLOPT HELEMAAL NOG NIET -> Beide lijsten moeten geupdate worden
           else if Set.member 88 keyCodes
           then { model | todoList = if getSelectedItemList model
                                     then let (id, _) = ItemList.getItem model.selected model.todoList
                                          in ItemList.update (ItemList.SubAction id Item.ToggleDone) model.todoList
                                     else model.todoList,
                          doneList = if getSelectedItemList model
                                     then model.doneList
                                     else let (id, _) = ItemList.getItem (model.selected - (List.length (model.todoList).items - 1)) model.doneList
                                          in ItemList.update (ItemList.SubAction id Item.ToggleDone) model.doneList }
      -- "j" has keycode 74
-- TODO KLOPT HELEMAAL NOG NIET
           else if Set.member 74 keyCodes
           then { model | selected = let totalListLength = List.length (model.todoList).items + List.length (model.doneList).items
                                     in if model.selected == totalListLength - 1
                                        then 0
                                        else model.selected + 1,
                          todoList = if List.length (model.todoList).items - 1 > model.selected
                                     then let (selectId, _) = ItemList.getItem (model.selected + 1) model.todoList
                                              (deselectId, _) = ItemList.getItem (model.selected) model.todoList
                                          in ItemList.update (ItemList.DoubleSubAction selectId Item.Select deselectId Item.Deselect) model.todoList
                                     else if (List.length (model.todoList).items - 1 == model.selected && List.isEmpty (model.doneList).items)
                                          then let (selectId, _) = ItemList.getItem 0 model.todoList
                                                   (deselectId, _) = ItemList.getItem (model.selected) model.todoList
                                               in ItemList.update (ItemList.DoubleSubAction selectId Item.Select deselectId Item.Deselect) model.todoList
                                          else model.todoList,
                          doneList = if List.length (model.todoList).items - 1 <= model.selected
                                     then let (id, _) = ItemList.getItem (model.selected + 1 - (List.length (model.todoList).items - 1) ) model.doneList
                                          in ItemList.update (ItemList.SubAction id Item.Select) model.doneList
                                     else model.doneList }
        -- "k" has keycode 75
-- TODO KLOPT HELEMAAL NOG NIET
          else if Set.member 75 keyCodes
          then model
          else { model | todoList = ItemList.sortNewWithPin model.todoList,
                         doneList = ItemList.sortNewWithPin model.doneList }
      else { model | todoList = ItemList.sortNewWithPin model.todoList,
                     doneList = ItemList.sortNewWithPin model.doneList }


getSelectedItemList : Model -> Bool
getSelectedItemList model = if model.selected > List.length (model.todoList).items - 1
                            then False -- selected item in doneList
                            else True -- selected item in todoList

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
      , Html.p [] []
      , Html.h1 [] [Html.text "Reminder"]
      , Html.input
          [ placeholder "New Reminder"
            , on "input" targetValue (\str -> Signal.message address (SaveContent str))
            , type' "text"
            , value model.reminderField
            , onEnter address (TodoList (ItemList.Add (Item.newReminder model.reminderField model.reminderDate)))
          ] []
      , Html.input
          [ type' "date"
            , on "input" targetValue (\date -> Signal.message address (SaveDate date))
            , value model.reminderDate
            , onEnter address (TodoList (ItemList.Add (Item.newReminder model.reminderField model.reminderDate)))
          ] []
      , Html.button [ onClick address (TodoList (ItemList.Add (Item.newReminder model.reminderField model.reminderDate)))] [ Html.text "Add" ]
      ]


onEnter : Signal.Address Action -> Action -> Html.Attribute
onEnter address action =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address action)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
