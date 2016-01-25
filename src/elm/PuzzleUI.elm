module PuzzleUI
  ( view
  ) where

import Html exposing (div, Html, span, text)
import Html.Attributes exposing (class)
import Models exposing (Game, Puzzle, PuzzlePlace)
import PuzzleWords
import Utilities exposing (isPunctuation, isSolved)

type alias Model =
  { ready : Bool
  , game : Game
  }

view : Model -> Html
view model =
  if not model.ready then
      div [] []

  else
      let words = PuzzleWords.toWords model.game.puzzle
          children =
            (List.map aWord words)
            ++ [ (solvedIndicator model.game.puzzle) ]
      in
         div
           [ class "puzzle" ]
           children

aWord : List PuzzlePlace -> Html
aWord places =
  let letters = List.map aLetter places
  in div
       [ class "puzzle-word" ]
       letters

aLetter : PuzzlePlace -> Html
aLetter { letter, show } =
  if show then
      div [ class "puzzle-place puzzle-place-letter" ] [ text letter ]

  else if isPunctuation letter then
      div [ class "puzzle-place puzzle-place-punctuation" ] [ text letter ]

  else
      div [ class "puzzle-place puzzle-place-letter" ] [ text "_" ]

solvedIndicator : Puzzle -> Html
solvedIndicator puzzle =
  let solved =
        isSolved puzzle

      indicator =
        if solved then
            span [] [ text "!" ]

        else
            span [] []

  in indicator

