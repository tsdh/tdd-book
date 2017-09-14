myRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myRepl prompt onInput = do
  putStr prompt
  s <- getLine
  myRepl (onInput s) onInput

myReplWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt onInput = do
  putStr prompt
  s <- getLine
  case onInput state s of
    Just (output, newState) => do putStr output
                                  myReplWith newState prompt onInput
    Nothing => pure ()

foo : Nat -> String -> Maybe (String, Nat)
foo s i = Just ((show s) ++ " " ++ i, (S s))
