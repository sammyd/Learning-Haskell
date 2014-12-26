
import Data.List

-- CH3 Problem 1 - replicate length
elementCount []      = 0
elementCount (x:xs)  = 1 + (elementCount xs)


-- CH3 Problem 2 - Add a type signature
elementCount2 :: [a] -> Int
elementCount2 []     = 0
elementCount2 (x:xs) = 1 + (elementCount2 xs)


-- CH3 Problem 3 - Mean of list
sumList [] = 0
sumList (x:xs) = x + sumList xs
listMean x = sumList x / (fromIntegral (length x))


-- CH3 Problem 4 - List into palindrome
palindrome [] = []
palindrome (x:xs) = x : (palindrome xs) ++ [x]


-- CH3 Problem 5 - Is a list a palindrome?
isPalindrome :: [Int] -> Bool
isPalindrome xs = (reverse xs  == xs)


-- CH3 Problem 6 - Sort list of lists by length

sortByLength :: [[a]] -> [[a]]
sortByLength l = sortBy compareLength l
                 where compareLength list1 list2 = compare (length list1) (length list2)

-- CH3 Problem 7 - Intersperse
isperse :: a -> [[a]] -> [a]
isperse _ []  = []
isperse _ [x] = x
isperse s (x:xs) = x ++ [s] ++ isperse s xs

-- CH3 Problem 8 - Tree height
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ t1 t2) = (max (treeHeight t1) (treeHeight t2)) + 1

