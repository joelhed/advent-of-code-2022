module Day11 (day11) where


import Data.List (foldl', partition, intercalate, sort, sortOn)
import Text.ParserCombinators.ReadP
    ( ReadP
    , readP_to_S
    , (<++)
    , pfail
    , many1
    , sepBy1
    , get
    , string
    , char
    , skipSpaces
    , eof
    )
import Text.Read.Lex (readDecP)

import Lib (Day (..))


type Item = Integer
data Val = Const Integer | Old deriving (Show)
data Operation = Add Val | Mul Val deriving (Show)
data Monkey = Monkey
    { monkeyNum :: Int
    , items :: [Item]
    , operation :: Operation
    , testDivisor :: Integer
    , trueMonkey :: Int
    , falseMonkey :: Int
    , inspectionCount :: Int
    } deriving (Show)

type Monkeys = [(Int, Monkey)]

-- Parsing

monkeyP :: ReadP Monkey
monkeyP = do
    monkeyNum' <- stringThenIntP "Monkey "
    _ <- char ':'
    skipSpaces

    _ <- string "Starting items: "
    items' <- sepBy1 readDecP (string ", ")
    skipSpaces

    _ <- string "Operation: new = old "
    opChar <- get
    opConstructor <- case opChar of
        '+' -> return Add
        '*' -> return Mul
        _   -> pfail
    skipSpaces
    opVal <- (string "old" *> return Old) <++ (Const <$> readDecP)
    skipSpaces

    testDivisor' <- stringThenIntP "Test: divisible by "
    trueMonkey' <- stringThenIntP "If true: throw to monkey "
    falseMonkey' <- stringThenIntP "If false: throw to monkey "
    
    return Monkey
        { monkeyNum = monkeyNum'
        , items = items'
        , operation = opConstructor opVal
        , testDivisor = testDivisor'
        , trueMonkey = trueMonkey'
        , falseMonkey = falseMonkey'
        , inspectionCount = 0
        }
  where
    stringThenIntP :: (Eq a, Num a) => String -> ReadP a
    stringThenIntP s = string s *> readDecP <* skipSpaces

monkeysP :: ReadP Monkeys
monkeysP =
    toMonkeyAssoc <$> many1 monkeyP <* eof
 where
    toMonkeyAssoc :: [Monkey] -> [(Int, Monkey)]
    toMonkeyAssoc = map (\monkey -> (monkeyNum monkey, monkey))

parseMonkeys :: String -> Monkeys
parseMonkeys = fst . head . readP_to_S monkeysP

_prettyMonkeys :: (Monkey -> String) -> Monkeys -> String
_prettyMonkeys monkeyToString =
    unlines . map monkeyRow . sortOn fst
  where
    monkeyRow (n, monkey) = "Monkey " ++ show n ++ ": " ++ monkeyToString monkey

_prettyItems :: Monkeys -> String
_prettyItems = _prettyMonkeys (intercalate ", " . map show . items)

doOperation :: Operation -> Integer -> Integer
doOperation op old = case op of
    Add x -> old + getVal x old
    Mul x -> old * getVal x old
  where
    getVal :: Val -> Integer -> Integer
    getVal (Const x) _ = x
    getVal Old       x = x

doRound :: (Integer -> Integer) -> Monkeys -> Monkeys
doRound worryModifier monkeys =
    foldl' turn monkeys [0..(length monkeys - 1)]
  where
    turn :: Monkeys -> Int -> Monkeys
    turn ms n =
        map updateMonkeys ms
      where
        monkey = case lookup n ms of
                 Just m  -> m
                 Nothing -> error "what happened to the monkey??"
        worryLevel = worryModifier . doOperation (operation monkey)
        monkeyItems = items monkey
        doTest = (== 0) . (`mod` (testDivisor monkey))
        (trueItems, falseItems) = partition doTest $ map worryLevel monkeyItems
        appendItems xs m@Monkey{ items = xs' } = m { items = xs' ++ xs}
        updateThisMonkey m@Monkey{ inspectionCount = count } =
            m { items = [], inspectionCount = count + length monkeyItems }
        updateMonkeys :: (Int, Monkey) -> (Int, Monkey)
        updateMonkeys pair@(n', m)
            | n' == n = (n', updateThisMonkey m)
            | n' == (trueMonkey monkey)  = (n', appendItems trueItems m)
            | n' == (falseMonkey monkey) = (n', appendItems falseItems m)
            | otherwise = pair

day11 :: Day
day11 = Day
    { part1 = solution 20 (`div` 3)
    , part2 = Just $ solution 10000 id
    }
  where
    solution :: Int -> (Integer -> Integer) -> String -> String
    solution rounds worryModifier input
        = show . foldr1 (*) . take 2 . reverse . sort . map (inspectionCount . snd)
        -- _prettyMonkeys (show . inspectionCount)
        . (!! rounds)
        . iterate (doRound ((`mod` commonModulo) . worryModifier))
        $ firstMonkeys
      where
        firstMonkeys = parseMonkeys input
        monkeyDivisors = map (testDivisor . snd) firstMonkeys
        commonModulo = foldr1 (*) monkeyDivisors
