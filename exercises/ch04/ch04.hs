import Data.Char (digitToInt, isDigit)
import Text.Printf

safeHead::[a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x


splitWith:: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith pred xs =
  let (first, rest) = break pred xs
  in  first : case rest of
    (y:ys) -> splitWith pred ys
    _ -> []

firstWords :: String -> String
firstWords [] = []
firstWords xs = unwords (map firstWord allLines)
  where allLines = lines xs
        firstWord s = head (words s)

lineTranspose:: String -> String
lineTranspose [] = []
lineTranspose xs = unlines (map reverse (lines xs))

asInt:: String -> Int
asInt ('-':intPart) = -1 * asInt intPart
asInt intPart = foldl acc 0 intPart
  where acc :: Int -> Char -> Int 
        acc cur c
          | isDigit c = cur * 10 + digitToInt c 
          | otherwise = error (printf "not a digit `%c`" c)

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':intPart) = case asInt_either intPart of
  (Left err) -> Left err
  (Right val) -> Right (-1 * val)
  
asInt_either intPart = foldl acc (Right 0) intPart
  where acc :: Either ErrorMessage Int -> Char -> Either ErrorMessage Int 
        acc (Left err) _ = Left err
        acc (Right cur) c
          | isDigit c = Right (cur * 10 + digitToInt c) 
          | otherwise = Left (printf "not a digit `%c`" c)

myConcat:: [[a]] -> [a]
myConcat [] = []
myConcat xs = foldr step [] xs
  where step :: [a] -> [a] -> [a]
        step acc x = acc ++ x

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pred (x:xs)
    | (pred x) = x:(myTakeWhile pred xs)
    | otherwise = []

myTakeWhile_fold :: (a -> Bool) -> [a] -> [a]
myTakeWhile_fold pred xs = foldr step [] xs
  where step x acc 
          | pred x = x : acc 
          | otherwise = []    


myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy relation xs = foldr step [] xs
  where step x [] = [[x]]
        step x1 ((x2:xs):others) 
          | relation x1 x2 = (x1:x2:xs):others
          | otherwise = [x1] : (x2:xs):others