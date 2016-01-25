module Utilities
  ( isLetter
  , isPunctuation
  , isSolved
  ) where

import Models exposing (Puzzle)
import String

isLetter : String -> Bool
isLetter letter =
  let
    first = String.left 1 letter

    letters = "abcdefghijklmnopqrstuvwxyz"

  in String.contains first letters

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
