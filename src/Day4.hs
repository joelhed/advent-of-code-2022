module Day4 (day4) where

import Lib
import Utils (splitOn)

listToPair :: [a] -> (a, a)
listToPair (x:y:_) = (x, y)
listToPair _       = error "could not construct a pair"

type Assignment = (Int, Int)
type AssignmentPair = (Assignment, Assignment)

parseAssignment :: String -> Assignment
parseAssignment = listToPair . map read . splitOn '-'

parseAssignmentPair :: String -> AssignmentPair
parseAssignmentPair = listToPair . map parseAssignment . splitOn ','

-- Checks whether the second pair is completely contained in the first
fullyOverlaps :: (Int, Int) -> (Int, Int) -> Bool
fullyOverlaps (start, end) (start', end') = (start <= start') && (end' <= end)

anyFullOverlap :: AssignmentPair -> Bool
anyFullOverlap (first, second) = fullyOverlaps first second || fullyOverlaps second first

countTrue :: [Bool] -> Int
countTrue = sum . map (\x -> if x then 1 else 0)

day4 :: Day
day4 = Day { part1 = show . countTrue . map (anyFullOverlap . parseAssignmentPair) . lines
           , part2 = Nothing
           }
