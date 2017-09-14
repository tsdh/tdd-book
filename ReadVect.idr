import Data.Vect

printVect : Show a => (len ** Vect len a) -> IO ()
printVect (len ** v) = do
  putStrLn $ show v ++ " (length " ++ show len ++ ")"

readVect' : IO (len ** Vect len String)
readVect' = do
  x <- getLine
  if x == ""
    then pure $ (_ ** [])
    else do (_ ** xs) <- readVect'
            pure $ (_ ** x :: xs)

readVect : IO (len ** Vect len String)
readVect = do
  putStrLn "Enter a vector, one element per line.  Finish with an empty line."
  v <- readVect'
  printVect v
  pure v

zipInputs : IO ()
zipInputs = do
  (l1 ** v1) <- readVect
  (l2 ** v2) <- readVect
  case exactLength l1 v2 of
    Just v2 => printVect $ (l1 ** zip v1 v2)
    Nothing => putStrLn $ "Sorry, vectors have different sizes, "
                          ++ show l1 ++ " vs " ++ show l2
