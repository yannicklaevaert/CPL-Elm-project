module Main where

import Html exposing ( Html )
import Signal
--import Item
--import ItemList
import ItemListPair
import Html.Events as E
import Maybe
import Static

-- Name: Yannick Laevaert
-- Student ID: r0295605


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Come up with your own extension!
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- Start of program

--main : Signal Html.Html
--main = Html.text "This should work."
--       |> Signal.constant


reminder : Static.Reminder
reminder = { body = "Take out the trash", created = "2016-09-30"}

email : Static.Email
email = { from = "bossman@corporate.me"
     , to = "manager@corporate.me"
     , title = "Corporate Ipsum"
     , body = """Collaboratively administrate empowered markets via plug-and-play
                 networks. Dynamically procrastinate B2C users after installed base
                 benefits. Dramatically visualize customer directed convergence without
                 revolutionary ROI.

                 Efficiently unleash cross-media information without cross-media
                 value. Quickly maximize timely deliverables for real-time
                 schemas. Dramatically maintain clicks-and-mortar solutions
                 without functional solutions.

                 Completely synergize resource taxing relationships via premier
                 niche markets. Professionally cultivate one-to-one customer
                 service with robust ideas. Dynamically innovate
                 resource-leveling customer service for state of the art customer
                 service."""
     , date = "2015-01-30"
     }

--mailbox : Signal.Mailbox (Maybe Item.Action)
mailbox : Signal.Mailbox (Maybe ItemListPair.Action)
mailbox = Signal.mailbox Nothing

isDefined : Maybe a -> Bool
isDefined maybe =
  case maybe of
    Just _ -> True
    _ -> False

--state : Signal Item.Model
state : Signal ItemListPair.Model
state =
  let update action model =
        case action of
          --Just a -> Item.update a model
          Just a -> ItemListPair.update a model
          _ -> model
  --in Signal.foldp update (Item.init <| Item.EContent email) mailbox.signal
  in Signal.foldp update ItemListPair.init mailbox.signal

main : Signal Html
main =
  --let view = Item.view (Signal.forwardTo mailbox.address Just)
  let view = ItemListPair.view (Signal.forwardTo mailbox.address Just)
  in Signal.map view state
