module KeyboardUI
  ( Context
  , inputs
  , view
  ) where

import Char exposing (fromCode)
import Html exposing (div, Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Keyboard
import Models exposing (Guess)
import Signal exposing (Address)
import String

-- MODEL

{- I would love to figure out a way to only indicate the pieces of the model
that this component actually needs, but when I tried to use the extensible
record syntax, 1) it got pretty ugly and 2) it would not compile.  The type
checker would complain about the extra fields that were being passed down from
the parent component.

type alias Model model game =
  { model
    | game :
      { game
      | guesses : List { letter : String, correct : Bool }
      }
  }
-}

type alias Model = List Guess

-- VIEW

type alias Context =
  { guess : Address String
  }

view : Context -> Model -> Html
view context model =
  let
    rows =
      [ "qwertyuiop"
      , "asdfghjkl"
      , "zxcvbnm"
      ]
  in
    div
      [ class "keyboard" ] (List.map (keyboardRow context model) rows)

keyboardRow : Context -> Model -> String -> Html
keyboardRow context model keys =
  div [] (List.map (keyboardKey context model) (String.toList keys))

keyboardKey : Context -> Model -> Char -> Html
keyboardKey context model key =
  let
    keyString = String.fromChar key

    guess =
      List.filter (\{ letter } -> keyString == letter) model
      |> List.head

    keyClass =
      case guess of
        Nothing -> class "keyboard-key"
        Just { correct } ->
          if correct then
            class "keyboard-key keyboard-key-correct"
          else
            class "keyboard-key keyboard-key-incorrect"
  in
    div
      [ keyClass
      , onClick context.guess keyString
      ]
      [ text keyString ]

inputs : Signal String
inputs =
  Signal.map
    (String.toLower << String.fromChar << Char.fromCode)
    Keyboard.presses
