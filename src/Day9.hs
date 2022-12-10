module Day9 (day9) where

import Data.List (foldl', nub)
import Data.Bifunctor (bimap, first, second)
import Lib (Day (..))

data Direction = U | D | L | R deriving (Show)
type Position = (Int, Int)
data State = State { headPos :: Position
                   , tailVisited :: [Position]
                   } deriving (Show)

tailPos :: State -> Position
--tailPos s@State{tailVisited=[]} = headPos s
tailPos State{tailVisited=visited} = head visited

parseMovements :: String -> [Direction]
parseMovements = concatMap toDirections . lines
    where toDirections (dir:' ':count) = take (read count) . repeat $ (letterToDir dir)
          toDirections _               = error "malformed line"
          letterToDir c = case c of
            'U' -> U
            'D' -> D
            'L' -> L
            'R' -> R
            _   -> error "unexpected direction"

vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (x, y) (x', y') = (x + x', y + y')

vecScale :: Int -> (Int, Int) -> (Int, Int)
vecScale c (x, y) = (c * x, c * y)

vecSub :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecSub u = vecAdd u . vecScale (-1)

step :: State -> Direction -> State
step state dir = state { headPos = headPos'
                       , tailVisited = tailPos':tailVisited state
                       }
    where headDelta = case dir of
                      U -> ( 0,  1)
                      D -> ( 0, -1)
                      L -> (-1,  0)
                      R -> ( 1,  0)
          headPos' = vecAdd (headPos state) headDelta
          tailHeadVec = vecSub headPos' $ tailPos state
          tailDelta = case bimap abs abs tailHeadVec of
                      (2, _) -> first signum tailHeadVec
                      (_, 2) -> second signum tailHeadVec
                      _      -> (0, 0)
          tailPos' = vecAdd tailDelta $ tailPos state

simulate :: [Direction] -> State
simulate = foldl' step initialState
    where initialState = State { headPos=(0, 0), tailVisited=[(0, 0)] }

day9 :: Day
day9 = Day { part1 = show . length . nub . tailVisited . simulate . parseMovements
           , part2 = Nothing
           }
