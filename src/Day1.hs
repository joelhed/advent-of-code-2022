module Day1 (day1) where

import Lib
import Data.List (sort)

-- Split a list at the appearance of a given item. This removes said item.
splitAtItem :: (Eq a) => a -> [a] -> [[a]]
splitAtItem _ [] = []
splitAtItem item xs =
    let (group, rest) = break (== item) xs
    in case rest of
        [] -> group:[]
        rest -> group:(splitAtItem item (tail rest))

getElfInventories :: String -> [[Int]]
getElfInventories = map (map read) . splitAtItem "" . lines

thisPart1 :: String -> String
thisPart1 = show . maximum . map sum . getElfInventories

threeLargest :: [Int] -> [Int]
threeLargest = take 3 . reverse . sort

thisPart2 :: String -> String
thisPart2 = show . sum . threeLargest . map sum . getElfInventories

day1 :: Day
day1 = Day { part1 = thisPart1 --maximum . map (sum . (map read)) . splitAtItem "" . lines
           , part2 = (Just thisPart2)
           }
