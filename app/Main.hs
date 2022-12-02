module Main (main) where

import System.Environment

import Lib
import Day1
import Day2


days :: [Day]
days = [ day1
       , day2
       ]

getLatestPart :: Day -> Part
getLatestPart day =
    case part2 day of
        (Just part) -> part
        Nothing     -> part1 day

getPartByNum :: Integer -> Day -> Part
getPartByNum 1 day = part1 day
getPartByNum 2 day =
    case part2 day of
        (Just part) -> part
        Nothing     -> error "no such part exists"
getPartByNum _ _ = error "no such part exists"


getPartOfDay :: [String] -> Part
getPartOfDay [] =
    getLatestPart $ last days

getPartOfDay [dayStr] =
    let dayIdx = ((read dayStr) - 1)
    in 
        getLatestPart $ days !! dayIdx

getPartOfDay (dayStr:partStr:_) =
    let
        dayIdx = (read dayStr) - 1
        day = days !! dayIdx
        partNum = read partStr
    in getPartByNum partNum $ day

main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    putStrLn $ getPartOfDay args input
