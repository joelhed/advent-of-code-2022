module Day1 (day1) where

import Lib

-- Split a list at the appearance of a given item. This removes said item.
splitAtItem :: (Eq a) => a -> [a] -> [[a]]
splitAtItem _ [] = []
splitAtItem item xs =
    let (group, rest) = break (== item) xs
    in case rest of
        [] -> group:[]
        rest -> group:(splitAtItem item (tail rest))

thisPart1 :: String -> String
thisPart1 input =
    show $ maximum $ map sum groups
    where groups = map (map read) $ splitAtItem "" $ lines input :: [[Int]]

day1 :: Day
day1 = Day { part1 = thisPart1 --maximum . map (sum . (map read)) . splitAtItem "" . lines
           , part2 = Nothing
           }
