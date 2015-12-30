module Gallows where

{-| Gallows is a game of hangman.

-}

import Array
import Dict
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Signal exposing (Address)
import StartApp
import String
import Task exposing (andThen)

---- MODEL ----

type alias Model =
  { ready : Bool
  , guesses : List String
  , solution : Solution
  , solved : Bool
  , phrases : List String
  , id : Int
  }

type alias Solution =
  List ( String, Bool )

type alias Game =
  { guesses : List String
  , solution : Solution
  , solved : Bool
  }

init : ( Model, Effects Action )
init =
  ( model, fetchPhrases )

model =
  let game = newGame "the blood runs cold on gallows hill"
  in
      { ready = False
      , phrases = []
      , guesses = game.guesses
      , solution = game.solution
      , solved = game.solved
      , id = -1
      }

newGame : String -> Game
newGame puzzle =
  { guesses = []
  , solution = List.map (\c -> (String.fromChar c, False)) (String.toList puzzle)
  , solved = False
  }

fetchPhrases : Effects Action
fetchPhrases =
  Http.get phrases "/data/proverbs.json"
    |> Task.toMaybe
    |> Task.map SetPhrases
    |> Effects.task

phrases : Json.Decoder (List String)
phrases =
  "phrases" := (Json.list Json.string)

---- UPDATE ----

type Action
  = Guess String
  | Reset
  | Cheat
  | SetPhrases (Maybe (List String))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Guess guess ->
      let solution = checkGuess guess model.solution
          model' =
            { model |
                guesses = guess :: model.guesses,
                solution = solution,
                solved = isSolved solution
            }
      in ( model', Effects.none )

    Reset ->
      if not model.ready then
         ( model, Effects.none )

      else
        let id = model.id + 1
            puzzle =
              Array.fromList model.phrases
              |> Array.get id
            game = newGame (Maybe.withDefault "" puzzle)
            model' =
              { model |
                  guesses = game.guesses,
                  solution = game.solution,
                  solved = game.solved,
                  id = id
              }
        in ( model', Effects.none )

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
          model' =
            { model |
                solution = solution,
                solved = isSolved solution
            }
      in ( model', Effects.none )

    SetPhrases phrases ->
      let ( id, ready ) =
            case phrases of
              Nothing -> ( -1, False )
              _ -> ( 0, True )
          model' =
            { model |
                ready = ready,
                phrases = Maybe.withDefault [] phrases,
                id = id
            }
      in ( model', Effects.none )

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
  if model.ready then
      (List.map renderALetter model.solution)
      ++ [ (renderSolved model.solved) ]

  else
      []

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

app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }

main : Signal Html
main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
