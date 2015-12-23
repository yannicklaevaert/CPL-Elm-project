module Item where

import Signal
import Date exposing (..)
import Html exposing ( Html )
import Html.Events as E
import Static
import List
import String

type ItemType = ReminderItem Static.Reminder | EmailItem Static.Email

type alias Model =
  { itemType : ItemType,
    pinned : Bool,
    done : Bool,
    truncated : Bool
  }

dummyItem : Model
dummyItem = newReminder "dummyReminder" "01-01-2000"

newItem : ItemType -> Model
newItem itemType =
  case itemType of
    ReminderItem reminder ->
      { itemType = (ReminderItem reminder)
        , pinned = False
        , done = False
        , truncated = False
      }
    EmailItem email ->
      { itemType = (EmailItem email)
        , pinned = False
        , done = False
        , truncated = True
      }

newReminder : String -> String -> Model
newReminder reminderBody reminderDate = newItem (ReminderItem { body = reminderBody, created = reminderDate })

--UPDATE

type Action
    = Pin
    | Unpin
    | MarkAsDone
    | MarkUndone
    | Truncate
    | DisableTruncate



update : Action -> Model -> Model
update action model =
  case action of
    Pin ->
      { model | pinned = True }

    Unpin ->
      { model | pinned = False }

    MarkAsDone ->
      { model | done = True }

    MarkUndone ->
      { model | done = False }

    Truncate ->
      { model | truncated = True }

    DisableTruncate ->
      { model | truncated = False }




-- VIEW



view : Signal.Address Action -> Model -> Html
view address model =
  case model.itemType of
    ReminderItem reminder ->
        Html.div []
        [ Html.p [] [Html.text reminder.body]
        , Html.p []
          [ if model.done == False
            then Html.button
                [ E.onClick address MarkAsDone ]
                [ Html.text "Mark as Done" ]
            else Html.button
                [ E.onClick address MarkUndone ]
                [ Html.text "Undo" ]
          , if model.pinned == False
            then Html.button
                [ E.onClick address Pin ]
                [ Html.text "Pin" ]
            else Html.button
                [ E.onClick address Unpin ]
                [ Html.text "Unpin" ]
          ]
          , Html.p [] [Html.text <| "date: " ++ reminder.created]
        ]


    EmailItem email ->
        Html.div []
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
                    [ E.onClick address Truncate ]
                    [ Html.text "Less" ]
                 else Html.button
                    [ E.onClick address DisableTruncate ]
                    [ Html.text "More" ])
            else Html.p [] []
          , if model.done == False
            then Html.button
                  [ E.onClick address MarkAsDone ]
                  [ Html.text "Mark as Done" ]
            else Html.button
                  [ E.onClick address MarkUndone ]
                  [ Html.text "Undo" ]
          , if model.pinned == False
            then Html.button
                  [ E.onClick address Pin ]
                  [ Html.text "Pin" ]
            else Html.button
                  [ E.onClick address Unpin ]
                  [ Html.text "Unpin" ]
          ]
          , Html.p [] [Html.text <| "date: " ++ email.date]
          ]
