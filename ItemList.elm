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
  { items : List (Id, Item.Model)
  , nextItemId : Id
  }


--init : Model
--init = initialise (Model [] 0 ) startItems

initEmpty : Model
initEmpty =
  { items = []
  , nextItemId = 0
  }

startItems : List Item.Model
startItems = let reminders = List.map Item.ReminderItem Static.reminders
                 emails = List.map Item.EmailItem Static.emails
             in sortItems <|List.map Item.newItem (List.append reminders emails)

{-initialise : Model -> List Item.Model -> Model
initialise model noIdList =
  let temp = (List.head noIdList)
  in case temp of
    Nothing -> model
    Just nextItem -> let newModel = { model | items = model.items ++ [(model.nextItemId, nextItem)],
                                              nextItemId = model.nextItemId + 1
                                    }
      --Model (List.append model.items [(model.nextItemId, a)]) (model.nextItemId + 1)
                         justList = (List.tail noIdList)
                     in case justList of
                       Nothing -> newModel
                       Just rest -> initialise newModel rest
-}

sortItems : List Item.Model -> List Item.Model
sortItems unsorted = let sorter item =
                        case item.itemType of
                          Item.ReminderItem reminder -> reminder.created
                          Item.EmailItem email -> email.date
                     in List.sortBy sorter unsorted


init : Model
init = addMultipleItems startItems (Model [] 0)

addMultipleItems : List Item.Model -> Model -> Model
addMultipleItems items model =
  case items of
    [] -> model
    item::rest -> let newModel = addItem item model
                  in addMultipleItems rest newModel

sortPinnedUnpinned : List (Id, Item.Model) -> List (Id, Item.Model)
sortPinnedUnpinned unsorted = sortPinnedHelp unsorted [] []

sortPinnedHelp : List (Id, Item.Model) -> List (Id, Item.Model) -> List (Id, Item.Model) -> List (Id, Item.Model)
sortPinnedHelp unsorted pinnedList unpinnedList =
  case unsorted of
    [] -> (sortIdItems pinnedList []) ++ (sortIdItems unpinnedList [])
    (id, item)::rest -> if item.pinned
                        then sortPinnedHelp rest ((id, item)::pinnedList) unpinnedList
                        else sortPinnedHelp rest pinnedList ((id, item)::unpinnedList)

sortIdItems : List (Id, Item.Model) -> List (Id, Item.Model) -> List (Id, Item.Model)
sortIdItems unsorted acc =
  case unsorted of
     [] -> acc
     (id, item)::rest -> let newAcc = placeIdItem (id, item) acc
                         in sortIdItems rest newAcc

placeIdItem : (Id, Item.Model) -> List (Id, Item.Model) -> List (Id, Item.Model)
placeIdItem (id, item) list =
  case list of
    [] -> [(id, item)]
    (xId, xItem)::rest -> let xItemType = xItem.itemType
                              itemType = item.itemType
                              xDate =
                            case xItemType of
                              Item.ReminderItem xReminder -> xReminder.created
                              Item.EmailItem xEmail -> xEmail.date
                          in let date =
                              case itemType of
                                Item.ReminderItem reminder -> reminder.created
                                Item.EmailItem email -> email.date
                             in if date < xDate
                                then (id, item)::list
                                else (xId, xItem)::(placeIdItem (id, item) rest)

addItem : Item.Model -> Model -> Model
addItem item model =
  let newId = model.nextItemId
  in { model | items = sortPinnedUnpinned((newId, item)::(model.items)),
               nextItemId = newId + 1}

removeItem : Id -> Model -> Model
removeItem id model =
  { model | items = List.filter (\i -> (fst i) /= id) model.items }

updateItem : (Item.Model -> Item.Model) -> Id -> Model -> Model
updateItem f id model =
  let test (id', x) = (id', if id == id' then f x else x)
  in { model | items = List.map test model.items }

-- UPDATE

type Action = SubAction Id Item.Action
              | Add Item.Model
              | Remove Id

update : Action -> Model -> Model
update action model =
  case action of
    Add item ->
      addItem item model
    Remove id ->
      removeItem id model
    SubAction id action ->
      updateItem (Item.update action) id model


view : Signal.Address Action -> Model -> Html
view address model =
  let view' (id, x) = Item.view (Signal.forwardTo address <| SubAction id) x
      items = List.map view' model.items
  in Html.div [] (items)
