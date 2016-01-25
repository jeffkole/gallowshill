module Models
  ( Game
  , Guess
  , Puzzle
  , PuzzlePlace
  ) where

type alias Game =
  { guesses : List Guess
  , puzzle : Puzzle
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
