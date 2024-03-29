module Main where

import Html exposing ( Html )
import Signal
import ItemFeed
import Html.Events as E
import Maybe
import Static
import Keyboard
import Date
import Time

-- Name: Yannick Laevaert
-- Student ID: r0295605


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed
-- Summary: The hotkey is alt + v. If there are no items in the done list, the list is automatically toggled invisible.
--          It doesn't matter if there are items in the list or not, the visibility can be changed with the hotkey.


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed
-- Summary: The hotkey is alt + r. The add reminder functionality is toggled invisible as default.


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Attempted
-- Summary: I tried to use a signal to change the date every minute. The inputs function needs to be changed then. But is doesnt seem to do what I want.


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Unattempted
-- Summary: /


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Unattempted
-- Summary: /


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Unattempted
-- Summary: /


-- * Periodically check for e-mails from Json (same url).
-- Status: Unattempted
-- Summary: /


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Unattempted
-- Summary: /


-- * Come up with your own extension!
-- Status: Unattempted
-- Summary: /




mailbox : Signal.Mailbox (Maybe ItemFeed.Action)
mailbox = Signal.mailbox Nothing

isDefined : Maybe a -> Bool
isDefined maybe =
  case maybe of
    Just _ -> True
    _ -> False

state : Signal ItemFeed.Model
state =
  let update action model =
        case action of
          Just a -> ItemFeed.update a model
          _ -> model
  in Signal.foldp update ItemFeed.init inputs

inputs : Signal (Maybe ItemFeed.Action)
inputs = Signal.merge mailbox.signal (Signal.map Just (Signal.map2 ItemFeed.KeyPress Keyboard.alt Keyboard.keysDown))
--inputs = Signal.merge mailbox.signal (Signal.merge (Signal.map Just ((Signal.map2 ItemFeed.KeyPress Keyboard.alt Keyboard.keysDown))) (Signal.map Just ((Signal.map ItemFeed.SaveDate date))))


date : Signal String
date = Signal.map currentDate (Time.every Time.minute)

currentDate : Time.Time -> String
currentDate time =
  let date = Date.fromTime time
      year = toString (Date.year date)
      month = toString (Date.month date)
      day = toString (Date.day date)
  in year ++ "-" ++ month ++ "-" ++ day


main : Signal Html
main =
  let view = ItemFeed.view (Signal.forwardTo mailbox.address Just)
  in Signal.map view state
