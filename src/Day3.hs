module Day3 (day3) where

import Data.List (intersect)
import Data.Char (ord, isUpper, isLower)
import Lib

type Item = Char
type RucksackCompartments = ([Item], [Item])

parseRucksackCompartments :: String -> RucksackCompartments
parseRucksackCompartments s = splitAt middle s
                              where middle = length s `div` 2

findCommonItem :: RucksackCompartments -> Item
findCommonItem = head . uncurry intersect

getItemPriorityScore :: Item -> Int
getItemPriorityScore c
    | isLower c = ord c - ord 'a' + 1
    | isUpper c = ord c - ord 'A' + 27
    | otherwise = error "character not permitted"

-- Part 2

type Rucksack = String

-- Group the given list into groups of n items.
group :: Int -> [a] -> [[a]]
group _n [] = []
group  n xs = this:group n rest
              where (this, rest) = splitAt n xs

findGroupCommonItem :: [Rucksack] -> Item
findGroupCommonItem = head . foldr1 intersect

day3 :: Day
day3 = Day { part1 = let lineToScore = getItemPriorityScore . findCommonItem . parseRucksackCompartments
                     in show . sum . map lineToScore . lines
           , part2 = let groupToScore = getItemPriorityScore . findGroupCommonItem
                     in Just $ show . sum . map groupToScore . group 3 . lines
           }
