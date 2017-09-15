import Data.Vect

readToBlank' : IO (List String)
readToBlank' = do
  x <- getLine
  if x == ""
    then pure []
    else do xs <- readToBlank'
            pure $ x :: xs

readToBlank : IO (List String)
readToBlank = do
  putStrLn "Enter elements, one element per line.  Finish with an empty line."
  readToBlank'

readAndSave : IO ()
readAndSave = do
  xs <- readToBlank
  putStr "Enter file name: "
  fileName <- getLine
  Right unit  <- writeFile fileName (unlines xs)
    | Left err => putStrLn ("Error when writing file " ++fileName ++ ": " ++ (show err))
  pure ()

readVectFile' : File -> IO (Either String (n ** Vect n String))
readVectFile' file = do
  Right l <- fGetLine file | Left err => pure (Left (show err))
  eof <- fEOF file
  if eof
    then pure (Right (_ ** []))
    else do Right (_ ** xs) <- readVectFile' file | err => pure err
            let chars = unpack l
            case nonEmpty chars of
              Yes _ => do let x = pack $ Prelude.List.init chars
                          pure (Right (_ ** x :: xs))
              No  _ => pure (Left "Encountered empty line")

readVectFile : (filename : String) -> IO (Either String (n ** Vect n String))
readVectFile filename = do
  Right file <- openFile filename Read
    | Left err => pure (Left (show err))
  readVectFile' file
