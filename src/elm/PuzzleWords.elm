module PuzzleWords
    ( toWords
    ) where

{-| Breaks up a list of letters into words.

    toWords [ { letter = "H" }, { letter = "e" }, { letter = "y" },
      { letter = " " }, { letter = "Y" }, { letter = "a" } ] ==
      [ [ { letter = "H" }, { letter = "e" }, { letter = "y" } ],
        [ { letter = "Y" }, { letter = "a" } ]
      ]

-}

{-
The `{ record | letter : String }` syntax denotes an extensible record, which
means this function can accept any record that has a `letter` field in it.
Without the `record |` prefix, then the only allowed type would be a record
with a `letter` field.

Note: `record` is not a reserved word.  It could be anything.
-}
toWords : List { record | letter : String } -> List (List { record | letter : String })
toWords letters =
  let folder letter words =
        case words of
          [] ->
            if letter.letter == " " then
              [ [ ] ]
            else
              [ [ letter ] ]

          (word :: rest) ->
            if letter.letter == " " then
              [] :: words
            else
              (letter :: word) :: rest

  in List.foldr folder [] letters
