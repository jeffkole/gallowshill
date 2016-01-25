module Utilities
  ( isPunctuation
  , isSolved
  ) where

import Models exposing (Puzzle)
import String

isPunctuation : String -> Bool
isPunctuation letter =
  let
    first = String.left 1 letter

    punctuation = " `~!@#$%^&*()-_=+[]{}|;':\",.<>/?"

  in String.contains first punctuation

isSolved : Puzzle -> Bool
isSolved puzzle =
  List.all
    (\{ letter, show } -> show || isPunctuation letter)
    puzzle
