module Day8 (day8) where

import Data.Char (isDigit, digitToInt)
import Data.List (transpose)
import GHC.Arr (listArray, Array (..), range, (!), bounds, accumArray, foldrElems)
import Lib

type Position = (Int, Int)
type Forest a = Array Position a

parseForest :: String -> Forest Int
parseForest input =
    listArray ((0, 0), (height - 1, width - 1)) . map digitToInt . filter isDigit $ input
    where rows = lines input
          height = length rows
          width = length . head $ rows

type ScannerState = (Position, Bool, Int)

visibleTrees :: Forest Int -> Forest Bool
visibleTrees forest
    = accumArray (||) False (bounds forest)
    $ concatMap getVisibleTreePositions [north, south, east, west]
    where (highestY, highestX) = snd . bounds $ forest
          boundsForX i = ((0, i), (highestX, i))
          east = range <$> boundsForX <$> [0..highestY]
          west = map reverse east
          south = transpose east
          north = map reverse south
          initialScannerState :: ScannerState
          initialScannerState = ((0, 0), True, -1) -- this will be ignored in the output
          getPosAndVisibility :: ScannerState -> (Position, Bool)
          getPosAndVisibility (pos, visibility, _) = (pos, visibility)
          visibilityScanner :: ScannerState -> Position -> ScannerState
          visibilityScanner (_, _, currentHighest) pos =
              ( pos
              , forest!pos > currentHighest
              , max (forest!pos) currentHighest
              )
          getVisibleTreePositions :: [[Position]] -> [(Position, Bool)]
          getVisibleTreePositions
              = concatMap
              $ map getPosAndVisibility
              . tail
              . scanl visibilityScanner initialScannerState

_prettyForest :: (a -> Char) -> Forest a -> String
_prettyForest toChar forest = unlines ( map (toChar . (forest!))
                                      <$> range . (\i -> ((i, 0), (i, highestX)))
                                      <$> [0..highestY]
                                      )
    where (highestY, highestX) = snd . bounds $ forest

day8 :: Day
day8 = Day { part1 = show . foldrElems ((+) . boolToNum) (0 :: Int) . visibleTrees . parseForest
           , part2 = Nothing
           }
    where boolToNum x = if x then 1 else 0
