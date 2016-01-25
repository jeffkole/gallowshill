module ScoreBoardUI
  ( view
  ) where

import Html exposing (div, h3, Html, text)
import Html.Attributes exposing (class)
import Models exposing (Game, Puzzle)
import String
import Utilities exposing (isLetter)

type alias Model = Game

view : Model -> Html
view model =
  let
    ( correctLetterScore, incorrectLetterScore ) =
      letterScores model.puzzle

    ( correct, incorrect ) =
      List.partition .correct model.guesses

    numCorrect =
      List.length correct

    numIncorrect =
      List.length incorrect

    correctScore =
      toFloat numCorrect * correctLetterScore

    incorrectScore =
      toFloat numIncorrect * incorrectLetterScore

    score =
      round (correctScore - incorrectScore)

  in
    div
      []
      [ h3 [] [ text "Scoreboard" ]
      , div
          [ class "h4" ]
          [ text (toString score) ]
      , div
          []
          [ text (toString numCorrect ++ " right")
          , text ", "
          , text (toString numIncorrect ++ " wrong")
          ]
      ]

letterScores : Puzzle -> ( Float, Float )
letterScores puzzle =
  if List.length puzzle == 0 then
    ( 0, 0 )

  else
    let
      letters =
        List.map (String.toLower << .letter) puzzle
          |> List.filter isLetter

      uniqueLetters =
        List.foldl
          (\letter uniques ->
            if List.member letter uniques then
              uniques
            else
              uniques ++ [ letter ]
          )
          []
          letters

      numberOfUniqueLetters =
        List.length uniqueLetters

      correctLetterScore =
        100 / toFloat numberOfUniqueLetters

      incorrectLetterScore =
        100 / toFloat (26 - numberOfUniqueLetters)

    in
      ( correctLetterScore, incorrectLetterScore )
