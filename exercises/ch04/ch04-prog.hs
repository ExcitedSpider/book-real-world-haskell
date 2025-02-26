import System.Environment (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- assign to myFunction
        myFunction = firstWords

firstWords :: String -> String
firstWords [] = []
firstWords xs = unwords (map firstWord allLines)
  where allLines = lines xs
        firstWord s = head (words s)