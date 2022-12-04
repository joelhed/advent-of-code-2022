module Utils (pair, splitOn) where

-- Split a list at the appearance of a given item. This removes said item.
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn item xs =
    let (group, rest) = break (== item) xs
    in group:case rest of
        [] -> []
        _  -> splitOn item (tail rest)

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)
