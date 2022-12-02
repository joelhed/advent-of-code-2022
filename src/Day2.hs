module Day2 (day2) where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Lib

data Play = Rock | Paper | Scissors deriving (Eq)
data Result = Win | Loss | Draw
type Round = (Play, Play)

charToPlay :: Char -> Play
charToPlay char = case char of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors
    'X' -> Rock
    'Y' -> Paper
    'Z' -> Scissors
    _   -> error "unrecognised play"

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

parseRound :: String -> Round
parseRound = join bimap charToPlay . pair (head, last)

playScore :: Play -> Int
playScore play = case play of
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

didWin :: Round -> Result
didWin round = case round of
    (Rock, Paper)                   -> Win
    (Paper, Scissors)               -> Win
    (Scissors, Rock)                -> Win
    (opponent, me) | opponent == me -> Draw
                   | otherwise      -> Loss

winScore :: Result -> Int
winScore result = case result of
    Win  -> 6
    Draw -> 3
    Loss -> 0

calculateScore :: Round -> Int
-- calculateScore round = (winScore $ didWin round) + (playScore $ snd round)

-- Pont-free version
-- This is less readable than the pointful version above, and is equally long.
-- If it weren't just for practice, I'd use the pointful version.
calculateScore = join $ (flip $ (+) . winScore . didWin) . playScore . snd

-- Part 2

type ExpectedRound = (Play, Result)

charToResult :: Char -> Result
charToResult char = case char of
    'X' -> Loss
    'Y' -> Draw
    'Z' -> Win
    _   -> error "unexpected result"

parseExpectedRound :: String -> ExpectedRound
parseExpectedRound = pair (charToPlay . head, charToResult . last)

findRightPlay :: ExpectedRound -> Play
findRightPlay expected = case expected of
    (opponent, Draw) -> opponent
    (Rock, Win)      -> Paper
    (Paper, Win)     -> Scissors
    (Scissors, Win)  -> Rock
    (Rock, Loss)     -> Scissors
    (Paper, Loss)    -> Rock
    (Scissors, Loss) -> Paper

roundFromExpectedRound :: ExpectedRound -> Round
roundFromExpectedRound = pair (fst, findRightPlay)

day2 :: Day
day2 = Day { part1 = let lineToScore = calculateScore . parseRound
                     in show . sum . map lineToScore . lines
           , part2 = let lineToScore = calculateScore . roundFromExpectedRound . parseExpectedRound
                     in Just $ show . sum . map lineToScore . lines
           }
