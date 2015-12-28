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
  }

init : Model
init =
  { todoDoneListPair = ItemListPair.init
  , reminderField = ""
  , reminderDate = "2015-01-01"
  }

getSelectedItemList : Model -> Bool
getSelectedItemList model = ItemListPair.getSelectedItemList model.todoDoneListPair

getSelectedItem : Model -> (ItemList.Id, Item.Model)
getSelectedItem model = ItemListPair.getSelectedItem model.todoDoneListPair

getPreviousItemList : Model -> Bool
getPreviousItemList model = ItemListPair.getPreviousItemList model.todoDoneListPair

getPreviousItem : Model -> (ItemList.Id, Item.Model)
getPreviousItem model = ItemListPair.getPreviousItem model.todoDoneListPair

getNextItemList : Model -> Bool
getNextItemList model = ItemListPair.getNextItemList model.todoDoneListPair

getNextItem : Model -> (ItemList.Id, Item.Model)
getNextItem model = ItemListPair.getNextItem model.todoDoneListPair

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
           then { model | todoDoneListPair = let newPair = ItemListPair.update (ItemListPair.TodoList ItemList.SortOldWithoutPin) model.todoDoneListPair
                                             in ItemListPair.update (ItemListPair.DoneList ItemList.SortOldWithoutPin) newPair }
      -- "o" has keycode 79
           else if Set.member 79 keyCodes
           then { model | todoDoneListPair = let (id, _) = getSelectedItem model
                                             in if getSelectedItemList model
                                                then ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction id Item.ToggleTruncate)) model.todoDoneListPair
                                                else ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction id Item.ToggleTruncate)) model.todoDoneListPair }
      -- "p" has keycode 80
-- TODO KLOPT HELEMAAL NOG NIET -> selecteditem moet nog wijzigen
           else if Set.member 80 keyCodes
           then { model | todoDoneListPair = let (id, _) = getSelectedItem model
                                             in if getSelectedItemList model
                                                then ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction id Item.TogglePin)) model.todoDoneListPair
                                                else ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction id Item.TogglePin)) model.todoDoneListPair }
      -- "x" has keycode 88
-- TODO KLOPT HELEMAAL NOG NIET -> selecteditem moet nog wijzigen
           else if Set.member 88 keyCodes
           then { model | todoDoneListPair = let (id, _) = getSelectedItem model
                                             in if getSelectedItemList model
                                                then ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction id Item.ToggleDone)) model.todoDoneListPair
                                                else ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction id Item.ToggleDone)) model.todoDoneListPair }


      -- "j" has keycode 74
-- TODO KLOPT HELEMAAL NOG NIET
           else if Set.member 74 keyCodes
           then { model | todoDoneListPair = let (nextId, _) = getNextItem model
                                                 (currentId, _) = getSelectedItem model
                                             in let updatedPair =
                                                  if getNextItemList model && getSelectedItemList model
                                                  then let newPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction nextId Item.ToggleSelect)) model.todoDoneListPair
                                                       in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                  else if getNextItemList model && not (getSelectedItemList model)
                                                  then let newPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction nextId Item.ToggleSelect)) model.todoDoneListPair
                                                       in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                  else if not (getNextItemList model) && getSelectedItemList model
                                                  then let newPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction nextId Item.ToggleSelect)) model.todoDoneListPair
                                                       in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                  else let newPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction nextId Item.ToggleSelect)) model.todoDoneListPair
                                                       in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                in ItemListPair.update ItemListPair.SelectNext updatedPair }
        -- "k" has keycode 75
-- TODO KLOPT HELEMAAL NOG NIET
          else if Set.member 75 keyCodes
          then { model | todoDoneListPair = let (previousId, _) = getPreviousItem model
                                                (currentId, _) = getSelectedItem model
                                            in let updatedPair =
                                                 if getPreviousItemList model && getSelectedItemList model
                                                 then let newPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction previousId Item.ToggleSelect)) model.todoDoneListPair
                                                      in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                 else if getPreviousItemList model && not (getSelectedItemList model)
                                                 then let newPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction previousId Item.ToggleSelect)) model.todoDoneListPair
                                                      in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                 else if not (getPreviousItemList model) && getSelectedItemList model
                                                 then let newPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction previousId Item.ToggleSelect)) model.todoDoneListPair
                                                      in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                 else let newPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction previousId Item.ToggleSelect)) model.todoDoneListPair
                                                      in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                               in ItemListPair.update ItemListPair.SelectPrevious updatedPair }

          else { model | todoDoneListPair = let newPair = ItemListPair.update (ItemListPair.TodoList ItemList.SortNewWithPin) model.todoDoneListPair
                                            in ItemListPair.update (ItemListPair.DoneList ItemList.SortNewWithPin) newPair }
      else { model | todoDoneListPair = let newPair = ItemListPair.update (ItemListPair.TodoList ItemList.SortNewWithPin) model.todoDoneListPair
                                        in ItemListPair.update (ItemListPair.DoneList ItemList.SortNewWithPin) newPair }


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
