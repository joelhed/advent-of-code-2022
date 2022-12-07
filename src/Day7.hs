module Day7 (day7) where

import Data.Bifunctor (first, second)
import Data.List (isPrefixOf, uncons)
import Data.Maybe (fromJust)
import Lib

type Filename = String
data Node = File Filename Int | Dir Filename [Node] deriving (Show)

size :: Node -> Int
size node = case node of
    File _ fSize -> fSize
    Dir _ nodes -> sum . map size $ nodes

walkDirs :: Node -> [Node]
walkDirs node = case node of
    File _ _    -> []
    Dir _ nodes -> [node] ++ concatMap walkDirs nodes

setDirContent :: [Filename] -> [Node] -> Node -> Node
setDirContent (pathName:xs) newContent node@(Dir fname oldContent) =
    if pathName /= fname then node
    else let content = case xs of
                       [] -> newContent
                       _  -> map (setDirContent xs newContent) oldContent
         in Dir fname content
setDirContent _  _ node@(File _ _) = node
setDirContent [] _ _               = error "needs at least one item in the path"

type State = (Node, [Filename])
readCommandOutput :: String -> [String] -> State -> State
readCommandOutput command outputLines state@(_, workDir) =
    case drop 2 command of
        'l':'s':_            -> first (setDirContent workDir (map parseLsLine outputLines)) state
        'c':'d':' ':pathName -> second (changeDir pathName) $ state
        _                    -> error "unrecognized command"
    where parseLsLine line = let (firstStr, fname) = case words line of
                                                     x:y:_ -> (x, y)
                                                     _     -> error "malformed ls output"
                             in if firstStr == "dir"
                                then Dir fname []
                                else File fname $ read firstStr
          changeDir fname path = if fname == ".."
                                 then init path
                                 else path ++ [fname]

gatherAllOutput :: String -> Node
gatherAllOutput = fst . foldl (flip $ uncurry readCommandOutput) initialState
                . map (second (takeWhile notCommandLine) . fromJust . uncons) 
                . takeWhile (not . null) . iterate (dropWhile notCommandLine . tail) . lines
    where notCommandLine = not . isPrefixOf "$"
          initialState   = (Dir "/" [], [])

day7 :: Day
day7 = Day { part1 = show . sum . filter (<= 100000) . map size . walkDirs . gatherAllOutput
           , part2 = Nothing
           }
