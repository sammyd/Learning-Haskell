
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


-- CH3 Problem 9 - Direction datatype
data Direction = LeftTurn
               | RightTurn
               | StraightLine
                 deriving (Show)


-- CH3 Problem 10 - Calculate turn Direction
data Cartesian2D = Cartesian2D Double Double deriving (Eq, Show)

turnDirection :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Direction
turnDirection a b c
  | grad1 < grad2  = LeftTurn
  | grad1 == grad2 = StraightLine
  | grad1 > grad2  = RightTurn
  where
    grad1 = grad a b
    grad2 = grad b c
    grad (Cartesian2D x1 y1) (Cartesian2D x2 y2) = (y2 - y1) / (x2 - x1)


directionList :: [Cartesian2D] -> [Direction]
directionList [] = []
directionList (x1:x2:x3:xs) = (turnDirection x1 x2 x3) : directionList (x2 : x3 : xs)
directionList (x:xs) = []


