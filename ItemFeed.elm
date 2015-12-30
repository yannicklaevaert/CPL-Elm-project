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
  , addReminderVisibility : Bool
  }

init : Model
init =
  { todoDoneListPair = ItemListPair.init
  , reminderField = ""
  , reminderDate = "2015-01-01"
  , addReminderVisibility = False
  }

getSelectedItemList : ItemListPair.Model -> Bool
getSelectedItemList itemListPair = ItemListPair.getSelectedItemList itemListPair

getSelectedItem : ItemListPair.Model -> (ItemList.Id, Item.Model)
getSelectedItem itemListPair = ItemListPair.getSelectedItem itemListPair

getPreviousItemList : ItemListPair.Model -> Bool
getPreviousItemList itemListPair = ItemListPair.getPreviousItemList itemListPair

getPreviousItem : ItemListPair.Model -> (ItemList.Id, Item.Model)
getPreviousItem itemListPair = ItemListPair.getPreviousItem itemListPair

getNextItemList : ItemListPair.Model -> Bool
getNextItemList itemListPair = ItemListPair.getNextItemList itemListPair

getNextItem : ItemListPair.Model -> (ItemList.Id, Item.Model)
getNextItem itemListPair = ItemListPair.getNextItem itemListPair

-- UPDATE

type Action = TodoDoneListPair ItemListPair.Action
            | SaveContent String
            | SaveDate String
            | KeyPress Bool (Set.Set (Char.KeyCode))
            | ToggleAddReminder

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
           then { model | todoDoneListPair = let (currentId, _) = getSelectedItem model.todoDoneListPair
                                             in let updatedPair = let newPair = ItemListPair.update (ItemListPair.TodoList ItemList.SortOldWithoutPin) model.todoDoneListPair
                                                                  in ItemListPair.update (ItemListPair.DoneList ItemList.SortOldWithoutPin) newPair
                                                in if getSelectedItemList model.todoDoneListPair
                                                   then let updatedNewPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                            (newSelectedId, _) = getSelectedItem updatedPair
                                                        in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                                   else let updatedNewPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                            (newSelectedId, _) = getSelectedItem updatedPair
                                                        in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair }
      -- "o" has keycode 79
           else if Set.member 79 keyCodes
           then { model | todoDoneListPair = let (id, _) = getSelectedItem model.todoDoneListPair
                                             in if getSelectedItemList model.todoDoneListPair
                                                then ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction id Item.ToggleTruncate)) model.todoDoneListPair
                                                else ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction id Item.ToggleTruncate)) model.todoDoneListPair }
      -- "p" has keycode 80
-- TODO KLOPT HELEMAAL NOG NIET -> selecteditem moet nog wijzigen
           else if Set.member 80 keyCodes
           then { model | todoDoneListPair = let (currentId, _) = getSelectedItem model.todoDoneListPair
                                             in let updatedPair =
                                                  if getSelectedItemList model.todoDoneListPair
                                                  then ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.TogglePin)) model.todoDoneListPair
                                                  else ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.TogglePin)) model.todoDoneListPair
                                                in if getSelectedItemList model.todoDoneListPair
                                                   then let updatedNewPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                            (newSelectedId, _) = getSelectedItem updatedPair
                                                        in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                                   else let updatedNewPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                            (newSelectedId, _) = getSelectedItem updatedPair
                                                        in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair }
      -- "x" has keycode 88
-- TODO KLOPT HELEMAAL NOG NIET -> selecteditem moet nog wijzigen
           else if Set.member 88 keyCodes
           then { model | todoDoneListPair = let (currentId, _) = getSelectedItem model.todoDoneListPair
                                             in let updatedPair =
                                                  if getSelectedItemList model.todoDoneListPair
                                                  then ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleDone)) model.todoDoneListPair
                                                  else ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleDone)) model.todoDoneListPair
                                                in if getSelectedItemList model.todoDoneListPair
                                                   then let updatedNewPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                            (newSelectedId, _) = getSelectedItem updatedPair
                                                        in if getSelectedItemList updatedNewPair
                                                           then ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                                           else ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                                   else let updatedNewPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                            (newSelectedId, _) = getSelectedItem updatedPair
                                                        in if getSelectedItemList updatedNewPair
                                                           then ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                                           else ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair }

      -- "j" has keycode 74
-- TODO KLOPT HELEMAAL NOG NIET
           else if Set.member 74 keyCodes
           then { model | todoDoneListPair = let (nextId, _) = getNextItem model.todoDoneListPair
                                                 (currentId, _) = getSelectedItem model.todoDoneListPair
                                             in let updatedPair =
                                                  if getNextItemList model.todoDoneListPair && getSelectedItemList model.todoDoneListPair
                                                  then let newPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction nextId Item.ToggleSelect)) model.todoDoneListPair
                                                       in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                  else if getNextItemList model.todoDoneListPair && not (getSelectedItemList model.todoDoneListPair)
                                                  then let newPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction nextId Item.ToggleSelect)) model.todoDoneListPair
                                                       in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                  else if not (getNextItemList model.todoDoneListPair) && getSelectedItemList model.todoDoneListPair
                                                  then let newPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction nextId Item.ToggleSelect)) model.todoDoneListPair
                                                       in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                  else let newPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction nextId Item.ToggleSelect)) model.todoDoneListPair
                                                       in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                in ItemListPair.update ItemListPair.SelectNext updatedPair }
        -- "k" has keycode 75
-- TODO KLOPT HELEMAAL NOG NIET
          else if Set.member 75 keyCodes
          then { model | todoDoneListPair = let (previousId, _) = getPreviousItem model.todoDoneListPair
                                                (currentId, _) = getSelectedItem model.todoDoneListPair
                                            in let updatedPair =
                                                 if getPreviousItemList model.todoDoneListPair && getSelectedItemList model.todoDoneListPair
                                                 then let newPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction previousId Item.ToggleSelect)) model.todoDoneListPair
                                                      in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                 else if getPreviousItemList model.todoDoneListPair && not (getSelectedItemList model.todoDoneListPair)
                                                 then let newPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction previousId Item.ToggleSelect)) model.todoDoneListPair
                                                      in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                 else if not (getPreviousItemList model.todoDoneListPair) && getSelectedItemList model.todoDoneListPair
                                                 then let newPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction previousId Item.ToggleSelect)) model.todoDoneListPair
                                                      in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                                 else let newPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction previousId Item.ToggleSelect)) model.todoDoneListPair
                                                      in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) newPair
                                               in ItemListPair.update ItemListPair.SelectPrevious updatedPair }

          -- "v" has keycode 68
          -- hotkey to toggle the visibility of ‘done’ items
          else if Set.member 86 keyCodes
          then { model | todoDoneListPair = ItemListPair.update ItemListPair.ToggleVisibilityDone model.todoDoneListPair }

          -- "r" has keycode 68
          -- hotkey to toggle the visibility of ‘done’ items
          else if Set.member 82 keyCodes
          then update ToggleAddReminder model

          else { model | todoDoneListPair = let (currentId, _) = getSelectedItem model.todoDoneListPair
                                            in let updatedPair = let newPair = ItemListPair.update (ItemListPair.TodoList ItemList.SortNewWithPin) model.todoDoneListPair
                                                                 in ItemListPair.update (ItemListPair.DoneList ItemList.SortNewWithPin) newPair
                                               in if getSelectedItemList model.todoDoneListPair
                                                  then let updatedNewPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                           (newSelectedId, _) = getSelectedItem updatedPair
                                                       in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                                  else let updatedNewPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                           (newSelectedId, _) = getSelectedItem updatedPair
                                                       in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair }

      else { model | todoDoneListPair = let (currentId, _) = getSelectedItem model.todoDoneListPair
                                        in let updatedPair = let newPair = ItemListPair.update (ItemListPair.TodoList ItemList.SortNewWithPin) model.todoDoneListPair
                                                             in ItemListPair.update (ItemListPair.DoneList ItemList.SortNewWithPin) newPair
                                           in if getSelectedItemList model.todoDoneListPair
                                              then let updatedNewPair = ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                       (newSelectedId, _) = getSelectedItem updatedPair
                                                   in ItemListPair.update (ItemListPair.TodoList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair
                                              else let updatedNewPair = ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction currentId Item.ToggleSelect)) updatedPair
                                                       (newSelectedId, _) = getSelectedItem updatedPair
                                                   in ItemListPair.update (ItemListPair.DoneList (ItemList.SubAction newSelectedId Item.ToggleSelect)) updatedNewPair }

    ToggleAddReminder -> { model | addReminderVisibility = not model.addReminderVisibility}

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
        , if not model.addReminderVisibility
          then Html.p [] []
          else Html.div []
                [ Html.h1 [] [Html.text "Reminder"]
                , Html.input
                    [ placeholder "New Reminder"
                      , on "input" targetValue (\str -> Signal.message address (SaveContent str))
                      , type' "text"
                      , value model.reminderField
                      , onEnter address (TodoDoneListPair (ItemListPair.TodoList (ItemList.AddNew (Item.newReminder model.reminderField model.reminderDate))))
                    ] []
                , Html.input
                    [ type' "date"
                      , on "input" targetValue (\date -> Signal.message address (SaveDate date))
                      , value model.reminderDate
                      , onEnter address (TodoDoneListPair (ItemListPair.TodoList (ItemList.AddNew (Item.newReminder model.reminderField model.reminderDate))))
                    ] []
                , Html.button [ onClick address (TodoDoneListPair (ItemListPair.TodoList (ItemList.AddNew (Item.newReminder model.reminderField model.reminderDate))))] [ Html.text "Add" ]
                ]
      ]


onEnter : Signal.Address Action -> Action -> Html.Attribute
onEnter address action =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address action)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"
