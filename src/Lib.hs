module Lib
    ( Day (..)
    , Part
    ) where


type Part = String -> String

data Day = Day { part1 :: Part
               , part2 :: Maybe (Part)
               }
