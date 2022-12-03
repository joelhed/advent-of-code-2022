module Day3 (day3) where

import Data.List (intersect)
import Data.Char (ord, isUpper, isLower)
import Lib

type Item = Char
type Rucksack = ([Item], [Item])

parseRucksack :: String -> Rucksack
parseRucksack s = splitAt middle s
                  where middle = length s `div` 2

findCommonItem :: Rucksack -> Item
findCommonItem = head . uncurry intersect

getItemPriorityScore :: Item -> Int
getItemPriorityScore c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27
    | otherwise = error "character not permitted"

day3 :: Day
day3 = Day { part1 = show . sum . map (getItemPriorityScore . findCommonItem . parseRucksack) . lines
           , part2 = Nothing
           }
