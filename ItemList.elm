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
startItems = let reminders = List.map Item.RContent Static.reminders
                 emails = List.map Item.EContent Static.emails
             in List.map Item.init <| sortItems (List.append reminders emails)

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

sortItems : List Item.Content -> List Item.Content
sortItems unsorted = let sorter item =
                        case item of
                          Item.RContent reminder -> reminder.created
                          Item.EContent email -> email.date
                     in List.sortBy sorter unsorted



--addItem : Item.Model -> Model
--addItem model =
--  Model ((model.nextCount, item) :: model.state) (model.nextCount + 1)

--removeItem : Model -> Model
--removeItem model =
--  { model | state = case List.tail model.state of
--                       Just l -> l
--                       Nothing -> []
--  }

updateItem : (Item.Model -> Item.Model) -> Id -> Model -> Model
updateItem f id model =
  let test (id', x) = (id', if id == id' then f x else x)
  in { model | state = List.map test model.state }

-- UPDATE

type Action = SubAction Id Item.Action
--              Add
--            | Remove
--            | SubAction Id Item.Action

update : Action -> Model -> Model
update action model =
  case action of
--    Add ->
--      addItem model
--    Remove ->
--      removeItem model
    SubAction id action ->
      updateItem (Item.update action) id model

view : Signal.Address Action -> Model -> Html
view address model =
  let view' (id, x) = Item.view (Signal.forwardTo address <| SubAction id) x
      items = List.map view' model.state
  in Html.div [] (items)
