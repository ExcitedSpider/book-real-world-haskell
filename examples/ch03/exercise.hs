import Data.Ratio
import Data.List
myLength :: [a] -> Integer
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

mean :: Fractional a => [a] -> a
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
  | x == last xs  = isPalindrome (init xs)
  | otherwise     = False

storeLists :: Foldable t => [t a] -> [t a]
storeLists [] = []
storeLists xs = sortBy compareList xs
  where compareList a b
          | length a > length b = GT
          | otherwise = LT

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ [x] = x
myIntersperse sep (x:xs) = x ++ [sep] ++ myIntersperse sep xs

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height:: Tree a -> Integer
height Empty = 0
height (Node _ left right) = max (1 + height left) (1 + height right)

simpleTree = Node "parent" (Node "left child"
                              (Node "Leaf" Empty Empty)
                              Empty)
                           (Node "right child" Empty Empty)


data Direction = DLeft
               | DRight
               | DStraight
              deriving (Show)

type CoordX = Double
type CoordY = Double
data Point2D = Point2D CoordX CoordY

-- Calculate Direction: https://algorithmtutor.com/Computational-Geometry/Determining-if-two-consecutive-segments-turn-left-or-right/
direction :: Point2D -> Point2D -> Point2D -> Direction
direction a b c
  | crossProd a b c < 0  = DRight
  | crossProd a b c == 0 = DStraight
  | crossProd a b c > 0  = DLeft
  where crossProd (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3)
          = (x2 - x1)*(y3 - y1) - (x3 - x1)*(y2 - y1)

seqDirections :: [Point2D] -> [Direction]
seqDirections (x1:x2:x3:xs) = direction x1 x2 x3: seqDirections (x2:x3:xs)
seqDirections _ = []