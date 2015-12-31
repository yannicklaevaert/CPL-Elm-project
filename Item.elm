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
    | ToggleDone
    | ToggleTruncate
    | ToggleSelect



update : Action -> Model -> Model
update action model =
  case action of
    TogglePin ->
      { model | pinned = if model.pinned
                            then False
                            else True }

    ToggleDone ->
      { model | done = if model.done
                       then False
                       else True }

    ToggleTruncate ->
      { model | truncated = if model.truncated
                            then False
                            else True }

    ToggleSelect ->
      { model | selected = if model.selected
                           then False
                           else True }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.itemType of
    ReminderItem reminder ->
        Html.div
        [ if model.selected
          then A.style [("border-left-style", "double"),
                        ("border-left-width", "thick"),
                        ("border-left-color", "rgb(255, 165, 0)")]
          else A.style [("border-left-style", "solid"),
                        ("border-left-width", "0px")]
        ]
        [ Html.div
          [ if model.pinned
            then A.style [("background-color", "rgb(200, 200, 200)")]
            else A.style [("background-color", "rgb(255, 255, 255)")]
          ]
          [ Html.p [] [Html.text reminder.body]
          , Html.p []
            [ if model.done == False
              then Html.button
                  [ E.onClick address ToggleDone ]
                  [ Html.text "Mark as Done" ]
              else Html.button
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
        ]


    EmailItem email ->
        Html.div
        [ if model.selected
          then A.style [("border-left-style", "double"),
                        ("border-left-width", "thick"),
                        ("border-left-color", "rgb(255, 165, 0)")]
          else A.style [("border-left-style", "solid"),
                        ("border-left-width", "0px")]
        ]
        [ Html.div
          [ if model.pinned
            then A.style [("background-color", "rgb(200, 200, 200)")]
            else A.style [("background-color", "rgb(255, 255, 255)")]
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
                    [ E.onClick address ToggleDone ]
                    [ Html.text "Mark as Done" ]
              else Html.button
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
        ]
