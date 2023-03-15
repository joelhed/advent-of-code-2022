module Day12 (day12) where


import Debug.Trace
import Data.Char (ord)
import Data.List (find)
import GHC.Arr (Array, array, assocs)
import Lib (Day (..))


type Position = (Int, Int)
type Height = Int
data Place = Start | End | Place Height deriving (Show)
type Map = Array Position Place

parseMap :: String -> Map
parseMap input =
    array
        ((0, 0), (width-1, height-1))
        [ ((x, y), toPlace c)
        | (y, row) <- zip [height-1, height-2..0] rows
        , (x, c)   <- zip [0..width-1] row
        ]
  where
    rows = lines input
    height = length rows
    width = length $ head rows
    toPlace :: Char -> Place
    toPlace c = case c of
        'S' -> Start
        'E' -> End
        _   -> Place $ ord c - ord 'a'

start :: Map -> Position
start m = case find (isStart . snd) $ assocs m of
    Just (pos, _) -> pos
    Nothing       -> error "no start found"
  where
    isStart Start = True
    isStart _     = False
    
end :: Map -> Position
end m = case find (isEnd . snd) $ assocs m of
    Just (pos, _) -> pos
    Nothing       -> error "no end found"
  where
    isEnd End = True
    isEnd _   = False

elevation :: Place -> Int
elevation p = case p of
    Start   -> 0
    End     -> ord 'z' - ord 'a'
    Place e -> e

shortestPathLength :: Map -> Int
shortestPathLength m =
    go [start m] initCameFrom

day12 :: Day
day12 = Day
    { part1 = show . end . parseMap
    , part2 = Nothing
    }
