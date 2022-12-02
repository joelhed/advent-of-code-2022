module Main (main) where

import System.Environment
import Data.Maybe (fromJust)

import Lib
import Day1
import Day2

days :: [Day]
days = [ day1
       , day2
       ]

getDayFromDaySpec :: DaySpec -> Day
getDayFromDaySpec = (days !!) . (subtract 1)

getPartFromDayPartSpec :: DayPartSpec -> Maybe Part
getPartFromDayPartSpec spec =
    (getPartFromPartSpec $ partSpec spec) day
    where day = getDayFromDaySpec $ daySpec spec

parseDayPartSpec :: [String] -> DayPartSpec
parseDayPartSpec args = case args of
    []                 -> let daySp = (length days)
                          in DayPartSpec daySp $ getLatestPartSpec $ getDayFromDaySpec daySp
    [dayStr]           -> let daySp = (read dayStr)
                          in DayPartSpec daySp $ getLatestPartSpec $ getDayFromDaySpec daySp
    (dayStr:partStr:_) -> let daySp = (read dayStr)
                          in DayPartSpec daySp $ getPartSpecByNum  $ read partStr

downloadInput :: IO String
downloadInput = error "no worky"

main :: IO ()
main = do
    args <- getArgs
    if args /= [] && head args == "--download" then do
        input <- downloadInput
        return ()
    else do
        input <- getContents
        let part = fromJust $ getPartFromDayPartSpec $ parseDayPartSpec args
        putStrLn $ part input
