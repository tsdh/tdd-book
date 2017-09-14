module Main

import System

printLength : IO ()
printLength =
  putStr "Input text: "
  >>= \_ => getLine
  >>= \s => putStrLn ("The length of the input was " ++ show (length s))

printLonger : IO ()
printLonger = do
  putStrLn "Enter two lines of text:"
  l1 <- getLine
  l2 <- getLine
  let l = max (length l1) (length l2)
  putStrLn ("The longer line is " ++ (show l) ++ " characters long.")

printLonger2 : IO ()
printLonger2 = putStrLn "Enter two lines of text:" >>=
  \_ => getLine >>=
  \l1 => getLine >>=
  \l2 => let l = max (length l1) (length l2) in
           putStrLn ("The longer line is " ++ (show l) ++ " characters long.")

readNat : IO (Maybe Nat)
readNat = do
  input <- getLine
  pure $ if all isDigit (unpack input)
            then Just (cast input)
            else Nothing

readNats : IO (Maybe (Nat, Nat))
readNats = do
  Just n1 <- readNat | Nothing => pure Nothing
  Just n2 <- readNat | Nothing => pure Nothing
  pure $ Just (n1, n2)


guessRunner : Nat -> Nat -> IO ()
guessRunner k g = do
  putStr ((show g) ++ ". guess: ")
  Just n <- readNat | Nothing => do putStrLn "Invalid input. Try again!"
                                    guessRunner k (S g)
  if n == k
    then putStrLn ("Congrats, the number is indeed " ++ (show k)
                   ++ "!\nYou needed " ++ (show g) ++ " guesses.")
    else do putStrLn ("Too " ++ (if n < k then "small" else "big"))
            guessRunner k (S g)

guess : Nat -> IO ()
guess k = guessRunner k 1


main : IO ()
main = do
 t <- time
 let rndVal = mod t 100
 guess $ cast rndVal
