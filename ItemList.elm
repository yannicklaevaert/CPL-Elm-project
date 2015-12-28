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
    item::rest -> let newModel = addNewItem item model
                  in addMultipleItems rest newModel

--sortPinnedUnpinned : List (Id, Item.Model) -> List (Id, Item.Model)
--sortPinnedUnpinned unsorted = sortPinnedHelp unsorted [] []

sortPinnedHelp : List (Id, Item.Model) -> List (Id, Item.Model) -> List (Id, Item.Model) -> List (Id, Item.Model)
sortPinnedHelp unsorted pinnedList unpinnedList =
  case unsorted of
    [] -> (sortIdItems pinnedList [] True) ++ (sortIdItems unpinnedList [] True)
    (id, item)::rest -> if item.pinned
                        then sortPinnedHelp rest ((id, item)::pinnedList) unpinnedList
                        else sortPinnedHelp rest pinnedList ((id, item)::unpinnedList)

sortIdItems : List (Id, Item.Model) -> List (Id, Item.Model) -> Bool -> List (Id, Item.Model)
sortIdItems unsorted acc newToOld =
  case unsorted of
     [] -> acc
     (id, item)::rest -> let newAcc = placeIdItem (id, item) acc newToOld
                         in sortIdItems rest newAcc newToOld

placeIdItem : (Id, Item.Model) -> List (Id, Item.Model) -> Bool -> List (Id, Item.Model)
placeIdItem (id, item) list newToOld =
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
                             in if newToOld
                                then if date < xDate
                                     then (id, item)::list
                                     else (xId, xItem)::(placeIdItem (id, item) rest newToOld)
                                else if date > xDate
                                     then (id, item)::list
                                     else (xId, xItem)::(placeIdItem (id, item) rest newToOld)

sortNewWithPin : Model -> Model
sortNewWithPin model = { model | items = let unsorted = model.items
                                     in sortPinnedHelp unsorted [] [] }

sortOldWithoutPin : Model -> Model
sortOldWithoutPin model = { model | items = sortIdItems model.items [] False}

getItem : Int -> Model -> (Id, Item.Model)
getItem n model = let item = List.head (List.drop n model.items)
                  in case item of
                      Just a -> a
                      _ -> (987654321, Item.dummyItem)

addNewItem : Item.Model -> Model -> Model
addNewItem item model =
  let newId = model.nextItemId
  in let newModel = { items = (newId, item)::(model.items),
                      nextItemId = newId + 1}
     in sortNewWithPin newModel

addItem : Id -> Item.Model -> Model -> Model
addItem id item model = let newModel = { items = (id, item)::(model.items),
                                         nextItemId = model.nextItemId }
                        in sortNewWithPin newModel

removeItem : Id -> Model -> Model
removeItem id model =
  { model | items = List.filter (\i -> (fst i) /= id) model.items }

updateItem : (Item.Model -> Item.Model) -> Id -> Model -> Model
updateItem f id model =
  let test (id', x) = (id', if id == id' then f x else x)
  in { model | items = List.map test model.items }

-- UPDATE

type Action = SubAction Id Item.Action
              | AddNew Item.Model
              | AddItem Id Item.Model
              | Remove Id
              | SortOldWithoutPin
              | SortNewWithPin
              | DoubleSubAction Id Item.Action Id Item.Action

update : Action -> Model -> Model
update action model =
  case action of
    AddNew item ->
      addNewItem item model
    AddItem id item ->
      addItem id item model
    Remove id ->
      removeItem id model
    SubAction id action ->
      updateItem (Item.update action) id model
    SortOldWithoutPin ->
      sortOldWithoutPin model
    SortNewWithPin ->
      sortNewWithPin model
    DoubleSubAction firstId firstAction secondId secondAction ->
      let updatedModel = updateItem (Item.update firstAction) firstId model
      in updateItem (Item.update secondAction) secondId updatedModel


view : Signal.Address Action -> Model -> Html
view address model =
  let view' (id, x) = Item.view (Signal.forwardTo address <| SubAction id) x
      items = List.map view' model.items
  in Html.div [] (items)
