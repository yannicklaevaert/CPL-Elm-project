module ItemFeed where

import Signal
import Html exposing ( Html )
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import ItemList
import Item
import ItemListPair
import Json.Decode as Json
import Set
import Char


type alias Model =
  { todoDoneListPair : ItemListPair.Model
  , reminderField : String
  , reminderDate : String
  , selected : Int
  }

init : Model
init =
  { todoDoneListPair = ItemListPair.init
  , reminderField = ""
  , reminderDate = "2015-01-01"
  , selected = 0
  }

-- UPDATE

type Action = TodoDoneListPair ItemListPair.Action
            | SaveContent String
            | SaveDate String
            | KeyPress Bool (Set.Set (Char.KeyCode))

update : Action -> Model -> Model
update action model =
  case action of
    TodoDoneListPair subAction -> { model | todoDoneListPair = ItemListPair.update subAction model.todoDoneListPair }

    SaveContent string -> { model | reminderField = string }

    SaveDate date -> { model | reminderDate = date }

    KeyPress altPressed keyCodes ->
      if altPressed
      -- "s" has keycode 83
      then if Set.member 83 keyCodes
           then { model | todoDoneListPair = ItemListPair.update ItemListPair.SortOldWithoutPin model.todoDoneListPair }
      -- "o" has keycode 79
           else if Set.member 79 keyCodes
           then { model |
{-                          todoList = if getSelectedItemList model
                                     then let (id, _) = ItemList.getItem model.selected model.todoList
                                          in ItemList.update (ItemList.SubAction id Item.ToggleTruncate) model.todoList
                                     else model.todoList,
                          doneList = if getSelectedItemList model
                                     then model.doneList
                                     else let (id, _) = ItemList.getItem (model.selected - (List.length (model.todoList).items - 1)) model.doneList
                                          in ItemList.update (ItemList.SubAction id Item.ToggleTruncate) model.doneList }
-}                          todoDoneListPair = ItemListPair.update (ItemListPair.UpdateSelectedItem Item.ToggleTruncate) model.todoDoneListPair }
      -- "p" has keycode 80
           else if Set.member 80 keyCodes
           then { model |
{-                          todoList = if getSelectedItemList model
                                     then let (id, _) = ItemList.getItem model.selected model.todoList
                                          in ItemList.update (ItemList.SubAction id Item.TogglePin) model.todoList
                                     else model.todoList,
                          doneList = if getSelectedItemList model
                                     then model.doneList
                                     else let (id, _) = ItemList.getItem (model.selected - (List.length (model.todoList).items - 1)) model.doneList
                                          in ItemList.update (ItemList.SubAction id Item.TogglePin) model.doneList }
-}                          todoDoneListPair = ItemListPair.update (ItemListPair.UpdateSelectedItem Item.TogglePin) model.todoDoneListPair }
      -- "x" has keycode 88
-- TODO KLOPT HELEMAAL NOG NIET -> Beide lijsten moeten geupdate worden
           else if Set.member 88 keyCodes
           then { model |
{-                          todoList = if getSelectedItemList model
                                     then let (id, _) = ItemList.getItem model.selected model.todoList
                                          in ItemList.update (ItemList.Remove id) model.todoList
                                     else let (id, _) = ItemList.getItem model.selected model.doneList
                                          in let updatedDoneList = ItemList.update (ItemList.SubAction id Item.ToggleDone) model.doneList
                                             in ItemList.update (ItemList.Add (help id (updatedDoneList.items))) model.todoList,
                          doneList = if getSelectedItemList model
                                     then let (id, _) = ItemList.getItem model.selected model.todoList
                                          in let updatedTodoList = ItemList.update (ItemList.SubAction id Item.ToggleDone) model.todoList
                                             in ItemList.update (ItemList.Add (help id (updatedTodoList.items))) model.doneList
                                     else let (id, _) = ItemList.getItem (model.selected - (List.length (model.todoList).items - 1)) model.doneList
                                          in ItemList.update (ItemList.Remove id) model.doneList }
-}                          todoDoneListPair = ItemListPair.update (ItemListPair.UpdateSelectedItem Item.ToggleDone) model.todoDoneListPair }

{-
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
-}          else { model | todoDoneListPair = ItemListPair.update ItemListPair.SortNewWithPin model.todoDoneListPair }
      else { model | todoDoneListPair = ItemListPair.update ItemListPair.SortNewWithPin model.todoDoneListPair }





-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div
      [style [("width", "40%"),
              ("margin", "auto")]
      ]
      [ Html.div []
        [ ItemListPair.view (Signal.forwardTo address TodoDoneListPair) (model.todoDoneListPair) ]
      , Html.p [] []
      , Html.h1 [] [Html.text "Reminder"]
      , Html.input
          [ placeholder "New Reminder"
            , on "input" targetValue (\str -> Signal.message address (SaveContent str))
            , type' "text"
            , value model.reminderField
--            , onEnter address (TodoList (ItemList.Add (Item.newReminder model.reminderField model.reminderDate)))
            , onEnter address (TodoDoneListPair (ItemListPair.TodoList (ItemList.AddNew (Item.newReminder model.reminderField model.reminderDate))))
          ] []
      , Html.input
          [ type' "date"
            , on "input" targetValue (\date -> Signal.message address (SaveDate date))
            , value model.reminderDate
--            , onEnter address (TodoList (ItemList.Add (Item.newReminder model.reminderField model.reminderDate)))
            , onEnter address (TodoDoneListPair (ItemListPair.TodoList (ItemList.AddNew (Item.newReminder model.reminderField model.reminderDate))))
          ] []
--      , Html.button [ onClick address (TodoList (ItemList.Add (Item.newReminder model.reminderField model.reminderDate)))] [ Html.text "Add" ]
      , Html.button [ onClick address (TodoDoneListPair (ItemListPair.TodoList (ItemList.AddNew (Item.newReminder model.reminderField model.reminderDate))))] [ Html.text "Add" ]
      ]


onEnter : Signal.Address Action -> Action -> Html.Attribute
onEnter address action =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address action)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
