module Day6 (day6) where

import Data.List (nub)
import Lib

day6 :: Day
day6 = Day { part1 = show . startOfSeq 4
           , part2 = Just $ show . startOfSeq 14
           }
       where iterLen len l  = take len l:(iterLen len $ tail l)
             startOfSeq len = (+ len) . length . fst . break ((== len) . length . nub) . iterLen len
