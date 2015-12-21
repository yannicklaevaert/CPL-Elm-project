module ItemList where

import Signal
import Date exposing (..)
import Html exposing ( Html )
import Html.Events as E
import Static
import Item

import List

type alias Id = Int

type alias Model =
  { state : List (Id, Item.Model)
  , nextItemId : Id }


init : Model
init = initialise (Model [] 0) startItems

initEmpty : Model
initEmpty = Model [] 0

startItems : List Item.Model
startItems = let reminders = List.map Item.ReminderItem Static.reminders
                 emails = List.map Item.EmailItem Static.emails
             in sortItems <|List.map Item.init (List.append reminders emails)

initialise : Model -> List Item.Model -> Model
initialise model noIdList =
  let temp = (List.head noIdList)
  in case temp of
    Nothing -> model
    Just a -> let newModel = Model (List.append model.state [(model.nextItemId, a)]) (model.nextItemId + 1)
                  justList = (List.tail noIdList)
              in case justList of
                Nothing -> newModel
                Just rest -> initialise newModel rest

sortItems : List Item.Model -> List Item.Model
sortItems unsorted = let sorter item =
                        case item.content of
                          Item.ReminderItem reminder -> reminder.created
                          Item.EmailItem email -> email.date
                     in List.sortBy sorter unsorted


{-
addItem : Item.Model -> Model -> Model
addItem item model = addItemToList item model (Model [] 0)

addItemToList : Item.Model -> Model -> Model -> Model
addItemToList item model accumulator =
  let createDateItem =
    case item.content of
      Item.ReminderItem reminder -> reminder.created
      Item.EmailItem email -> email.date
  in let createDateFirstItem =
      case ((snd (List.head model.state)).content) of
        Item.ReminderItem firstReminder -> firstReminder.created
        Item.EmailItem firstEmail -> firstEmail.date
     in if createDateFirstItem >= createDateItem
        then let newModel = Model (List.append accumulator.state (accumulator.nextItemId, item))  (nextItemId + 1)
             in updateAfterAddToItemList newModel model
        else let newModel = Model (List.append accumulator.state (accumulator.nextItemId, (snd (List.head model.state))))  (nextItemId + 1)
                 toDoList = List.tail model.state
             in addItemToList item

          Model (List.append model.state [(model.nextItemId, a)]) (model.nextItemId + 1)

updateAfterAddToItemList : Model -> Model -> Model
updateAfterAddToItemList accumulator notUpdated =
  case notUpdated of
    [] -> accumulator
    _ -> let nextID = accumulator.nextItemId
             nextItem = snd <| List.head notUpdated
             toDoList = List.tail notUpdated
         in updateAfterAddToItemList (List.append accumulator ([nextId, nextItem] (nextId + 1))) (toDoList)



  Model ((model.nextCount, item) :: model.state) (model.nextCount + 1) }
-}

addItem : Item.Model -> Model -> Model
addItem item model =
  let items = item :: (List.map snd model.state)
  in initialise (Model [] 0) (sortItems items)

removeItem : Id -> Model -> Model
removeItem id model =
  let firstItem = snd (List.head model.state)

updateItem : (Item.Model -> Item.Model) -> Id -> Model -> Model
updateItem f id model =
  let test (id', x) = (id', if id == id' then f x else x)
  in { model | state = List.map test model.state }

-- UPDATE

type Action = SubAction Id Item.Action
              | Add Item.Model
--            | Remove
--            | SubAction Id Item.Action

update : Action -> Model -> Model
update action model =
  case action of
    Add item ->
      addItem item model
--    Remove ->
--      removeItem model
    SubAction id action ->
      updateItem (Item.update action) id model

view : Signal.Address Action -> Model -> Html
view address model =
  let view' (id, x) = Item.view (Signal.forwardTo address <| SubAction id) x
      items = List.map view' model.state
  in Html.div [] (items)
