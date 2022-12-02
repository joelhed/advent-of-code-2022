module Day2 (day2) where

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

parseRound :: String -> Round
parseRound row =
    (charToPlay opponent , charToPlay me)
    where opponent = head row
          me       = last row

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
calculateScore round = (winScore $ didWin round) + (playScore $ snd round)

-- Part 2

charToResult :: Char -> Result
charToResult char = case char of
    'X' -> Loss
    'Y' -> Draw
    'Z' -> Win
    _   -> error "unexpected result"

parseExpectedResult :: String -> (Play, Result)
parseExpectedResult row =
    (charToPlay opponent, charToResult expected)
    where opponent = head row
          expected = last row

findRightPlay :: (Play, Result) -> Play
findRightPlay expected = case expected of
    (opponent, Draw) -> opponent
    (Rock, Win)      -> Paper
    (Paper, Win)     -> Scissors
    (Scissors, Win)  -> Rock
    (Rock, Loss)     -> Scissors
    (Paper, Loss)    -> Rock
    (Scissors, Loss) -> Paper

roundFromExpectedResult :: (Play, Result) -> Round
roundFromExpectedResult expected = (fst expected, findRightPlay expected)

day2 :: Day
day2 = Day { part1 = let lineToScore = calculateScore . parseRound
                     in show . sum . map lineToScore . lines
           , part2 = let lineToScore = calculateScore . roundFromExpectedResult . parseExpectedResult
                     in Just $ show . sum . map lineToScore . lines
           }
