-- Problem 1: Safe versions of functions

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x = Just (last x)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit x = Just (init x)


-- Problem 2: Splits whenever predicate returns false
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = start : splitWith f (tail' end)
    where
        (start, end) = break f xs
        -- Need this function to handle an empty list
        tail' [] = []
        tail' (_:ys) = ys