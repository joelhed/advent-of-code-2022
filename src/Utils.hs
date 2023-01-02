module Utils
    ( pair
    , splitOn
    , maybeAt
    , assocUpdate
    ) where


-- (!!) but returns Maybe
maybeAt :: Int -> [a] -> Maybe a
maybeAt idx xs = case drop idx xs of
    []    -> Nothing
    (x:_) -> (Just x)

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

assocUpdate :: (Eq a) => a -> (b -> b) -> [(a, b)] -> [(a, b)]
assocUpdate key f assoc =
    (key, f value):filter ((/= key) . fst) assoc
  where
    value = case lookup key assoc of
            Just x  -> x
            Nothing -> error "key not found" 
