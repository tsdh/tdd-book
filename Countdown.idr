import System

countdown : Nat -> IO ()
countdown Z = putStrLn "TAKEOFF!!!"
countdown i@(S k) = do
  putStrLn ((show i) ++ " seconds to go.")
  usleep 1000000
  countdown k
