module Day2 (day2) where

import Lib

data Play = Rock | Paper | Scissors deriving (Eq)
data Result = Win | Loss | Draw
type Round = (Play, Play)

charToPlay :: Char -> Play
charToPlay 'A' = Rock
charToPlay 'B' = Paper
charToPlay 'C' = Scissors
charToPlay 'X' = Rock
charToPlay 'Y' = Paper
charToPlay 'Z' = Scissors
charToPlay _ = error "unrecognised play"

parseRound :: String -> Round
parseRound row =
    (charToPlay opponent , charToPlay me)
    where opponent = head row
          me = last row

playScore :: Play -> Int
playScore Rock = 1
playScore Paper = 2
playScore Scissors = 3

didWin :: Round -> Result
didWin (Rock, Paper) = Win
didWin (Paper, Scissors) = Win
didWin (Scissors, Rock) = Win
didWin (opponent, me)
    | opponent == me = Draw
    | otherwise      = Loss

winScore :: Result -> Int
winScore Win = 6
winScore Draw = 3
winScore Loss = 0

calculateScore :: Round -> Int
calculateScore round = (winScore $ didWin round) + (playScore $ snd round)

-- Part 2
charToResult :: Char -> Result
charToResult 'X' = Loss
charToResult 'Y' = Draw
charToResult 'Z' = Win
charToResult _ = error "unexpected result"

parseExpectedResult :: String -> (Play, Result)
parseExpectedResult row =
    (charToPlay opponent, charToResult expected)
    where opponent = head row
          expected = last row

findRightPlay :: (Play, Result) -> Play
findRightPlay (opponent, Draw) = opponent
findRightPlay (Rock, Win)      = Paper
findRightPlay (Paper, Win)     = Scissors
findRightPlay (Scissors, Win)  = Rock
findRightPlay (Rock, Loss)     = Scissors
findRightPlay (Paper, Loss)    = Rock
findRightPlay (Scissors, Loss) = Paper

roundFromExpectedResult :: (Play, Result) -> Round
roundFromExpectedResult expected = (fst expected, findRightPlay expected)

day2 :: Day
day2 = Day { part1 = show . sum . map (calculateScore . parseRound) . lines
           , part2 = Just $ show . sum . map (calculateScore . roundFromExpectedResult . parseExpectedResult) . lines
           }
