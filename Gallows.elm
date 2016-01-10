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
import Task exposing (Task)

---- MODEL ----

type alias Model =
  { ready : Bool
  , game : Game
  , phrases : List String
  , id : Int
  }

type alias Guess =
  { letter : String
  , correct : Bool
  }

type alias PuzzlePlace =
  { letter : String
  , show : Bool
  }

type alias Puzzle =
  List PuzzlePlace

type alias Game =
  { guesses : List Guess
  , puzzle : Puzzle
  }

init : ( Model, Effects Action )
init =
  ( model, fetchPhrases )

model =
  let game = newGame "the blood runs cold on gallows hill"
  in
      { ready = False
      , game = game
      , phrases = []
      , id = -1
      }

newPuzzlePlace : Char -> PuzzlePlace
newPuzzlePlace char =
  { letter = String.fromChar char
  , show = False
  }

newGame : String -> Game
newGame phrase =
  { guesses = []
  , puzzle = List.map newPuzzlePlace (String.toList phrase)
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
  = TakeAGuess String
  | NewGame
  | RevealALetter
  | SetPhrases (Maybe (List String))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    TakeAGuess guess ->
      let game = model.game

          checkedGuess = checkGuess guess game.puzzle

          updatedPuzzle =
            if checkedGuess.correct then
               updatePuzzle guess game.puzzle

            else
               game.puzzle

          updatedGame =
            { game
                | guesses = checkedGuess :: game.guesses
                , puzzle = updatedPuzzle
            }

          updatedModel =
            { model
                | game = updatedGame
            }

      in ( updatedModel, Effects.none )

    NewGame ->
      if not model.ready then
         ( model, Effects.none )

      else
        let id = model.id + 1
            phrase =
              Array.fromList model.phrases
              |> Array.get id

            game = newGame (Maybe.withDefault "" phrase)

            updatedModel =
              { model
                  | game = game
                  , id = id
              }

        in ( updatedModel, Effects.none )

    RevealALetter ->
      let game = model.game

          unguessedLetters = findIncorrectLetters game.puzzle

          guess = findLeastFrequentLetter unguessedLetters

          updatedPuzzle = updatePuzzle guess game.puzzle

          updatedGame =
            { game
                | puzzle = updatedPuzzle
            }

          updatedModel =
            { model
                | game = updatedGame
            }

      in ( updatedModel, Effects.none )

    SetPhrases phrases ->
      let ( id, ready ) =
            case phrases of
              Nothing -> ( -1, False )
              _ -> ( 0, True )

          updatedModel =
            { model
                | ready = ready
                , phrases = Maybe.withDefault [] phrases
                , id = id
            }

      in ( updatedModel, Effects.none )

checkGuess : String -> Puzzle -> Guess
checkGuess guess puzzle =
  { letter = guess
  , correct = List.any (\{ letter } -> letter == guess) puzzle
  }

updatePuzzle : String -> Puzzle -> Puzzle
updatePuzzle guess puzzle =
  List.map
    (\{ letter, show } ->
        { letter = letter
        , show = show || guess == letter
        }
    )
    puzzle

isSolved : Puzzle -> Bool
isSolved puzzle =
  List.all
    (\{ letter, show } -> show || letter == " ")
    puzzle

findIncorrectLetters : Puzzle -> List String
findIncorrectLetters puzzle =
  List.filter (\{ show } -> show == False) puzzle
    |> List.map .letter

findLeastFrequentLetter : List String -> String
findLeastFrequentLetter letters =
  let letterCounts =
        List.foldl
          (\letter counts ->
              Dict.update
                letter
                (\count -> Just ((Maybe.withDefault 0 count) + 1))
                counts
          )
          Dict.empty letters

      leastFrequentLetter =
        letterCounts
          |> Dict.toList
          |> List.sortBy snd
          |> List.head
          |> Maybe.withDefault ( "", 0 )
          |> fst

  in leastFrequentLetter

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
        (puzzle model)
      ]
    , div
      []
      [ h2 [] [ text "Guesses" ]
      , input
        [ placeholder "guess"
        , on "input" targetValue (\guess -> Signal.message address (TakeAGuess guess))
        , value "" -- Clear out the value, so only one letter at a time is used
        , maxlength 1
        ]
        []
      , ol
        []
        (List.map (\{ letter } -> li [] [ text letter ]) model.game.guesses)
      ]
    ]

controls : Address Action -> Html
controls address =
  ul []
    [ li
        []
        [ button [ onClick address NewGame ] [ text "New Game" ]
        , button [ onClick address RevealALetter ] [ text "Reveal A Letter" ]
        ]
    ]

puzzle : Model -> List Html
puzzle model =
  if model.ready then
      (List.map aLetter model.game.puzzle)
      ++ [ (solvedIndicator model.game.puzzle) ]

  else
      []

aLetter : PuzzlePlace -> Html
aLetter { letter, show } =
  let letterText =
        if show then
            letter

        else if letter == " " then
            " "

        else
            "_"
  in span [ puzzleStyle ] [ text letterText ]

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

port tasks : Signal (Task Never ())
port tasks =
  app.tasks
