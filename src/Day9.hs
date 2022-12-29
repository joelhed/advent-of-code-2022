module Day9 ( day9
            , State(..)
            , Direction (..)
            , Position
            , headPos
            , parseMovements
            , initialState
            , step
            ) where

import Data.List (foldl', nub)
import Data.Bifunctor (bimap, first, second)
import Lib (Day (..))

data Direction = U | D | L | R deriving (Show)
type Position = (Int, Int)
data State = State { rope :: [Position]
                   , tailVisited :: [Position]
                   } deriving (Show)

headPos :: State -> Position
headPos = head . rope

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


mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f = bimap f f

vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (x, y) (x', y') = (x + x', y + y')

vecScale :: Int -> (Int, Int) -> (Int, Int)
vecScale c (x, y) = (c * x, c * y)

vecSub :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecSub u = vecAdd u . vecScale (-1)

step :: State -> Direction -> State
step state dir = state { rope = rope'
                       , tailVisited = tailPos':tailVisited state
                       }
    where headDelta = case dir of
                      U -> ( 0,  1)
                      D -> ( 0, -1)
                      L -> (-1,  0)
                      R -> ( 1,  0)
          headPos' = vecAdd (headPos state) headDelta
          tailPos' = last rope'
          rope' = scanl moveSection headPos' . tail . rope $ state
          moveSection inFront this =
            let diffVec   = vecSub inFront this
                thisDelta = case mapBoth abs diffVec of
                            (2, 2) -> mapBoth signum diffVec
                            (2, _) -> first signum diffVec
                            (_, 2) -> second signum diffVec
                            _      -> (0, 0)
            in vecAdd this thisDelta

initialState :: Int -> State
initialState ropeSize = State { rope = take ropeSize $ repeat (0,0)
                              , tailVisited = []
                              }

simulate :: Int -> [Direction] -> State
simulate ropeSize = foldl' step (initialState ropeSize)

day9 :: Day
day9 = Day { part1 = solution 2
           , part2 = Just $ solution 10
           }
    where solution ropeSize = show . length . nub . tailVisited . simulate ropeSize . parseMovements
