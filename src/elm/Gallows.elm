module Gallows where

{-| Gallows is a game of hangman.

-}

import CurrentTime
import Dict
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import KeyboardUI
import PuzzleWords
import Random exposing (Seed)
import Signal exposing (Address)
import StartApp
import String
import Task exposing (Task)

---- MODEL ----

type alias Model =
  { ready : Bool
  , game : Game
  , phrases : List String
  , seed : Seed
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
  let model =
      { ready = False
      , game = newGame ""
      , phrases = []
      , seed = Random.initialSeed CurrentTime.now
      }
  in ( model, fetchPhrases )

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

nextPhrase : Seed -> List String -> ( Seed, Maybe String )
nextPhrase seed phrases =
  let generator = Random.int 0 (List.length phrases - 1)

      ( index, newSeed ) = Random.generate generator seed

      phrase =
        List.drop index phrases
        |> List.head

  in ( newSeed, phrase )


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
        let ( seed, phrase ) = nextPhrase model.seed model.phrases

            game = newGame (Maybe.withDefault "" phrase)

            updatedModel =
              { model
                  | game = game
                  , seed = seed
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

    SetPhrases maybePhrases ->
      case maybePhrases of
        Nothing -> ( model, Effects.none )

        Just phrases ->
          let ( seed, phrase ) = nextPhrase model.seed phrases

              game = newGame (Maybe.withDefault "" phrase)

              updatedModel =
                { model
                    | ready = True
                    , phrases = phrases
                    , seed = seed
                    , game = game
                }

          in ( updatedModel, Effects.none )

checkGuess : String -> Puzzle -> Guess
checkGuess guess puzzle =
  let correct =
        List.any
            -- The same toUpper comparison is done in updatePuzzle. DRY?
            (\{ letter } -> (String.toUpper letter) == (String.toUpper guess))
            puzzle

  in { letter = guess
     , correct = correct
     }

updatePuzzle : String -> Puzzle -> Puzzle
updatePuzzle guess puzzle =
  List.map
    (\{ letter, show } ->
        { letter = letter
        -- The same toUpper comparison is done in checkGuess. DRY?
        , show = show || (String.toUpper guess) == (String.toUpper letter)
        }
    )
    puzzle

isSolved : Puzzle -> Bool
isSolved puzzle =
  List.all
    (\{ letter, show } -> show || isPunctuation letter)
    puzzle

findIncorrectLetters : Puzzle -> List String
findIncorrectLetters puzzle =
  let notShown { show } = show == False
      isProperLetter { letter } = not (isPunctuation letter)
      incorrect x = notShown x && isProperLetter x

  in
      List.filter incorrect puzzle
        |> List.map .letter

findLeastFrequentLetter : List String -> String
findLeastFrequentLetter letters =
  let letterCounts =
        List.foldl
          (\letter counts ->
              Dict.update
                (String.toUpper letter)
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
    [ class "container text-xs-center" ]
    [ div [ class "row" ]
        [ div [ class "col-xs-12" ]
            [ h1 []
                [ text "The Blood Runs Cold On Gallows Hill" ]
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-xs-12" ]
            [ controls address model
            ]
        ]
    , div [ class "row m-b-1" ]
        [ div [ class "col-xs-12" ]
            [ h2 [ class "hidden-sm-down" ] [ text "Puzzle" ]
            , h4 [] [ text "Solve the puzzle below" ]
            , puzzle model
            ]
        ]
    , div [ class "row" ]
        [ div [ class "col-xs-12" ]
            [ h2 [ class "hidden-sm-down" ] [ text "Guesses" ]
            , h4 [] [ text "Click a key to guess a letter" ]
            , keyboard address model
            ]
        ]
    ]

keyboard : Address Action -> Model -> Html
keyboard address model =
  let context =
        KeyboardUI.Context (Signal.forwardTo address TakeAGuess)

      -- Unfortunately, I have to construct a model exactly like what the
      -- KeyboardUI wants instead of being able to pass in the whole game,
      -- letting the extensible record syntax indicate the fields that are
      -- needed.
      keyboardModel = model.game.guesses

  in
      KeyboardUI.view context keyboardModel

controls : Address Action -> Model -> Html
controls address model =
  let gameNotReady = not model.ready
      puzzleSolved = isSolved model.game.puzzle
  in
      ul
        [ class "list-inline" ]
        [ li
            [ class "list-inline-item" ]
            [ button
              [ onClick address NewGame
              , disabled gameNotReady
              , class "btn btn-primary-outline"
              , type' "button"
              ]
              [ text "New Game" ]
            ]
        , li
            [ class "list-inline-item" ]
            [ button
              [ onClick address RevealALetter
              , disabled (gameNotReady || puzzleSolved)
              , class "btn btn-secondary-outline"
              , type' "button"
              ]
              [ text "Reveal A Letter" ]
            ]
        ]

puzzle : Model -> Html
puzzle model =
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

isPunctuation : String -> Bool
isPunctuation letter =
  let first = String.left 1 letter

      punctuation = " `~!@#$%^&*()-_=+[]{}|;':\",.<>/?"

  in String.contains first punctuation

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

---- INPUTS ----

app : StartApp.App Model
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
