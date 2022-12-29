module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Char (intToDigit)
import GHC.Arr (Array (..), accum, listArray, bounds, range, (!))
import Lib
import Day9

pretty2DArray :: Array Position Char -> String
pretty2DArray arr = unlines ( map (arr!)
                            <$> range . (\y -> ((lowestX, y), (highestX, y)))
                            <$> reverse [lowestY..highestY]
                            )
    where ((lowestX, lowestY), (highestX, highestY)) = bounds arr

prettyState :: (Position, Position) -> State -> String
prettyState thisBounds State{rope=theRope} =
    pretty2DArray arr
    where emptyArr = listArray thisBounds $ repeat '.'
          arr = accum (curry snd) emptyArr associations
          associations =  ((0, 0), 's')
                       :  (reverse $ zip (tail theRope) (map intToDigit [1..]))
                       ++ [(head theRope, 'H')]

printState :: (Position, Position) -> State -> IO ()
printState thisBounds state = do
    putStrLn $ prettyState thisBounds state
    threadDelay 100000
    putStr "\ESC8"

main :: IO ()
main = do
    input <- getContents
    let movements = parseMovements input
        states = scanl step (initialState 10) movements
        (xs, ys) = unzip . map headPos $ states
        lowestPos = (minimum xs, minimum ys)
        highestPos = (maximum xs, maximum ys)
    putStr "\ESC7"
    mapM_ (printState (lowestPos, highestPos)) states
