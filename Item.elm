module Item where

import Signal
import Date exposing (..)
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A
import Static
import List
import String

type ItemType = ReminderItem Static.Reminder | EmailItem Static.Email

type alias Model =
  { itemType : ItemType,
    pinned : Bool,
    done : Bool,
    truncated : Bool,
    selected: Bool
  }

dummyItem : Model
dummyItem = newReminder "dummyReminder" "2000-01-01"

newItem : ItemType -> Model
newItem itemType =
  case itemType of
    ReminderItem reminder ->
      { itemType = (ReminderItem reminder)
        , pinned = False
        , done = False
        , truncated = False
        , selected = False
      }
    EmailItem email ->
      { itemType = (EmailItem email)
        , pinned = False
        , done = False
        , truncated = True
        , selected = False
      }

newReminder : String -> String -> Model
newReminder reminderBody reminderDate = newItem (ReminderItem { body = reminderBody, created = reminderDate })

--UPDATE

type Action
    = TogglePin
--    | MarkAsDone
--    | MarkUndone
    | ToggleDone
    | ToggleTruncate
    | Select
    | Deselect



update : Action -> Model -> Model
update action model =
  case action of
    TogglePin ->
      { model | pinned = if model.pinned
                            then False
                            else True }

{-    MarkAsDone ->
      { model | done = True }

    MarkUndone ->
      { model | done = False }
-}
    ToggleDone ->
      { model | done = if model.done
                       then False
                       else True }

    ToggleTruncate ->
      { model | truncated = if model.truncated
                            then False
                            else True }

    Select ->
      { model | selected = True }

    Deselect ->
      { model | selected = False }




-- VIEW



view : Signal.Address Action -> Model -> Html
view address model =
  case model.itemType of
    ReminderItem reminder ->
        Html.div
        [ if model.selected
          then A.style [("border-left-style", "double"),
                        ("border-left-width", "thick"),
                        ("border-left-color", "rgb(170, 255, 255)")
                       ]
          else A.style [("border-left-style", "solid"),
                        ("border-left-width", "0px")]
        ]
        [ Html.p [] [Html.text reminder.body]
        , Html.p []
          [ if model.done == False
            then Html.button
--                [ E.onClick address MarkAsDone ]
                [ E.onClick address ToggleDone ]
                [ Html.text "Mark as Done" ]
            else Html.button
--                [ E.onClick address MarkUndone ]
                [ E.onClick address ToggleDone ]
                [ Html.text "Undo" ]
          , if model.pinned == False
            then Html.button
                [ E.onClick address TogglePin ]
                [ Html.text "Pin" ]
            else Html.button
                [ E.onClick address TogglePin ]
                [ Html.text "Unpin" ]
          ]
          , Html.p [] [Html.text <| "date: " ++ reminder.created]
        ]


    EmailItem email ->
        Html.div
        [ if model.selected
          then A.style [("border-left-style", "double"),
                        ("border-left-width", "thick"),
                        ("border-left-color", "rgb(170, 255, 255)")
                       ]
          else A.style [("border-left-style", "solid"),
                        ("border-left-width", "0px")]
        ]
        [ Html.p [] [Html.text <| email.title ++ " | " ++ email.from ++ " says:"]
        , let body =
            case model.truncated of
              False -> email.body
              True -> if String.length email.body >= 200
                      then String.append (String.fromList (List.take 200 (String.toList (email.body)))) "..."
                      else email.body
          in Html.p [] [ Html.text body]
        , Html.p []
          [ if String.length email.body >= 200
            then (if model.truncated == False
                 then Html.button
                    [ E.onClick address ToggleTruncate ]
                    [ Html.text "Less" ]
                 else Html.button
                    [ E.onClick address ToggleTruncate ]
                    [ Html.text "More" ])
            else Html.p [] []
          , if model.done == False
            then Html.button
--                  [ E.onClick address MarkAsDone ]
                  [ E.onClick address ToggleDone ]
                  [ Html.text "Mark as Done" ]
            else Html.button
--                  [ E.onClick address MarkUndone ]
                  [ E.onClick address ToggleDone ]
                  [ Html.text "Undo" ]
          , if model.pinned == False
            then Html.button
                  [ E.onClick address TogglePin ]
                  [ Html.text "Pin" ]
            else Html.button
                  [ E.onClick address TogglePin ]
                  [ Html.text "Unpin" ]
          ]
          , Html.p [] [Html.text <| "date: " ++ email.date]
          ]
