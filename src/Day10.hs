module Day10 (day10) where

import Lib
import Data.List (isPrefixOf, mapAccumL)

data Instruction = Noop | Addx Int deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s | "noop" `isPrefixOf` s = Noop
                   | "addx" `isPrefixOf` s = Addx . read . (!! 1) . words $ s
                   | otherwise             = error "malformed instruction"

type State = Int

simulate :: [Instruction] -> [State]
simulate = concat . snd . mapAccumL step 1
    where step :: State -> Instruction -> (State, [State])
          step regX Noop     = (regX, [regX])
          step regX (Addx x) = (regX + x, [regX, regX])

takeEvery :: Int -> [a] -> [a]
takeEvery n xs = case drop (n - 1) xs of
                 []   -> []
                 y:ys -> y:takeEvery n ys

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = case splitAt n xs of
                  ([], _)  -> []
                  (ys, zs) -> ys:splitEvery n zs

interestingCycles :: [a] -> [a]
interestingCycles cycles =
    x : takeEvery 40 xs
    where (x, xs) = case drop 19 cycles of
                      y:ys -> (y, ys)
                      []   -> error "too few cycles to find anything interesting"

drawPixel :: (Int, State) -> Char
drawPixel (cycleNum, spritePos) =
    if shouldDraw then '#' else '.'
    where currentPixel = (cycleNum - 1) `mod` 40
          shouldDraw = abs (spritePos - currentPixel) <= 1

day10 :: Day
day10 = Day { part1 = show . sum . map (uncurry (*)) . interestingCycles . numberedCycles
            , part2 = Just $ unlines . splitEvery 40 . map drawPixel . numberedCycles
            }
        where numberedCycles = zip [1..] . simulate . map parseInstruction . lines
