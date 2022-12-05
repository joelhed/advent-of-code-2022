{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment
import System.IO (openFile, hGetContents, IOMode (..))
import Data.List (isInfixOf)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple

import Lib
import Day1
import Day2
import Day3
import Day4
import Day5

days :: [Day]
days = [ day1
       , day2
       , day3
       , day4
       , day5
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

getSessionId :: IO String
getSessionId = do
    handle <- openFile "session_id" ReadMode
    hGetContents handle

downloadInput :: DaySpec -> IO (Maybe String)
downloadInput day = do
    manager <- newManager tlsManagerSettings
    sessionId <- getSessionId
    let url = "https://adventofcode.com/2022/day/" ++ (show day) ++ "/input"
    request <- parseRequest url
    let request' = setRequestHeader "Cookie" [(C8.pack $ "session=" ++ sessionId)]
                 $ setRequestManager manager request
    response <- httpLBS request'
    let body = L8.unpack $ responseBody response
    if isInfixOf "Please log in" body then
        error "session id not supplied"
    else
        return $ Just body

main :: IO ()
main = do
    args <- getArgs
    (dayPartSpec, input) <-
        if args /= [] && head args == "--download" then do
            let dayPartSpec = parseDayPartSpec $ tail args
            maybeInput <- downloadInput $ daySpec dayPartSpec
            case maybeInput of
                (Just input) -> return (dayPartSpec, input)
                Nothing      -> error $ "no input to download for day " ++ (show $ daySpec dayPartSpec)
        else do
            let dayPartSpec = parseDayPartSpec args
            input <- getContents
            return (dayPartSpec, input)
    let part = fromJust $ getPartFromDayPartSpec $ dayPartSpec
    putStrLn $ part input
