module ControlsUI
  ( Context
  , view
  ) where

import Html exposing (button, Html, li, text, ul)
import Html.Attributes exposing (class, disabled, type')
import Html.Events exposing (onClick)
import Models exposing (Game)
import Signal exposing (Address)
import Utilities exposing (isSolved)

type alias Model =
  { ready : Bool
  , game : Game
  }

type alias Context =
  { newGame : Address ()
  , revealALetter : Address ()
  }

view : Context -> Model -> Html
view context model =
  let
    gameNotReady = not model.ready
    puzzleSolved = isSolved model.game.puzzle
  in
    ul
      [ class "list-inline" ]
      [ li
          [ class "list-inline-item" ]
          [ button
            [ onClick context.newGame ()
            , disabled gameNotReady
            , class "btn btn-primary-outline"
            , type' "button"
            ]
            [ text "New Game" ]
          ]
      , li
          [ class "list-inline-item" ]
          [ button
            [ onClick context.revealALetter ()
            , disabled (gameNotReady || puzzleSolved)
            , class "btn btn-secondary-outline"
            , type' "button"
            ]
            [ text "Reveal A Letter" ]
          ]
      ]
