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