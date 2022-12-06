{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment
import System.IO (openFile, hGetContents', IOMode (..))
import Data.List (isInfixOf)

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple

import Lib
import Utils (maybeAt)
import Days

parseDayPartSpec :: [String] -> Maybe DayPartSpec
parseDayPartSpec args =
    DayPartSpec daySpec' <$> partSpec'
    where daySpec'  = maybe (length days) read
                    . maybeAt 0 $ args
          partSpec' = maybe (getLatestPartSpec <$> getDayFromDaySpec daySpec') (getPartSpecByNum . read)
                    . maybeAt 1 $ args

getSessionId :: IO String
getSessionId = do
    handle <- openFile "session_id" ReadMode
    hGetContents' handle

downloadInput :: DaySpec -> IO String
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
        fail "session id not supplied"
    else if isInfixOf "Please don't repeatedly request this endpoint before it unlocks!" body then
        fail "input for day not yet released"
    else
        return body

main :: IO ()
main = do
    args <- getArgs
    let (args', shouldDownload) = if args /= [] && head args == "--download"
                                  then (tail args, True)
                                  else (args, False)

    dayPartSpec <- case parseDayPartSpec args' of
        Just x  -> return x
        Nothing -> fail "no such day and/or part exists"

    part <- case getPartFromDayPartSpec dayPartSpec of
        Just x  -> return x
        Nothing -> fail "no such part exists"

    input <-
        if shouldDownload
        then downloadInput $ daySpec dayPartSpec
        else getContents

    putStrLn . part $ input
