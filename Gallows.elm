module Gallows where

{-| Gallows is a game of hangman.

-}

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String

---- MODEL ----

type alias Model =
  { puzzle : String
  , guesses : List String
  , solution : Solution
  , solved : Bool
  }

type alias Solution =
  List ( String, Bool )

model =
  newGame "the blood runs cold on gallows hill"

newGame : String -> Model
newGame puzzle =
  { puzzle = puzzle
  , guesses = []
  , solution = List.map (\c -> (String.fromChar c, False)) (String.toList puzzle)
  , solved = False
  }

---- UPDATE ----

type Action
  = NoOp
  | Guess String
  | Reset
  | Cheat

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Guess guess ->
      let solution = checkGuess guess model.solution
      in
          { model |
              guesses = guess :: model.guesses,
              solution = solution,
              solved = isSolved solution
          }

    Reset ->
      newGame "choose your own adventure"

    Cheat ->
      let unguessedLetters =
              List.filter (\( letter, guessed) -> guessed == False) model.solution
                |> List.map fst
          letterCounts =
              List.foldl (\letter counts -> Dict.update letter (\v -> Just ((Maybe.withDefault 0 v) + 1)) counts) Dict.empty unguessedLetters
          leastFrequentLetter =
              letterCounts
                |> Dict.toList
                |> List.sortBy snd
                |> List.head
                |> Maybe.withDefault ( "", 0 )
                |> fst
          guess = leastFrequentLetter
          solution = checkGuess guess model.solution
      in
          { model |
              solution = solution,
              solved = isSolved solution
          }

checkGuess : String -> Solution -> Solution
checkGuess guess solution =
  List.map
    (\( letter, guessed ) -> ( letter, guessed || guess == letter ))
    solution

isSolved : Solution -> Bool
isSolved solution =
  List.all
    (\( letter, guessed ) -> guessed || letter == " ")
    solution

---- VIEW ----

view : Address Action -> Model -> Html
view address model =
  div
    []
    [ h1 [] [ text "The Blood Runs Cold On Gallows Hill" ]
    , div
      []
      [ (controls address) ]
    , div
      []
      [ h2 [] [ text "Puzzle" ]
      , div
        []
        (renderSolution model)
      ]
    , div
      []
      [ h2 [] [ text "Guesses" ]
      , input
        [ placeholder "guess"
        , on "input" targetValue (\guess -> Signal.message address (Guess guess))
        , value "" -- Clear out the value, so only one letter at a time is used
        , maxlength 1
        ]
        []
      , ol
        []
        (List.map (\guess -> li [] [ text guess ]) model.guesses)
      ]
    ]

controls : Address Action -> Html
controls address =
  ul []
    [ li
        []
        [ button [ onClick address Reset ] [ text "Reset" ]
        , button [ onClick address Cheat ] [ text "Cheat" ]
        ]
    ]

renderSolution : Model -> List Html
renderSolution model =
  (List.map renderALetter model.solution)
  ++ [ (renderSolved model.solved) ]

renderALetter : ( String, Bool ) -> Html
renderALetter solution =
  span [ puzzleStyle ] [ text (renderLetter solution) ]

renderLetter : ( String, Bool ) -> String
renderLetter ( letter, guessed ) =
  if guessed then
      letter

  else if letter == " " then
      " "

  else
      "_"

renderSolved : Bool -> Html
renderSolved isSolved =
  if isSolved then
     span [] [ text "!" ]

  else
     span [] []

puzzleStyle : Attribute
puzzleStyle =
  style
    [ ( "font-size", "2em" )
    , ( "margin-right", "0.2em" )
    ]

---- INPUTS ----

main =
  StartApp.start { model = model, view = view, update = update }
