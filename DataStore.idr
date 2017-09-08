module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size' items) s = MkData (size' + 1) (items ++ [s])

data Command = Add String
             | Get Integer
             | Invalid String
             | Size
             | Search String
             | Quit

total
parseCommand : String -> String -> Command
parseCommand "add"    arg = Add arg
parseCommand "get"    arg = if all isDigit $ unpack arg then Get (cast arg)
                                                        else Invalid $ "Invalid arg to get: " ++ arg
parseCommand "quit"   arg = Quit
parseCommand "size"   arg = Size
parseCommand "search" arg = Search arg
parseCommand cmd      arg = Invalid $ cmd ++ " kenne ich nicht"

total
parseInput : String -> Command
parseInput s = case span (/= ' ') s of
                    (cmd, arg) => let arg = trim arg in
                                    parseCommand cmd arg

total
processCommand : Command -> DataStore -> Maybe (String, DataStore)
processCommand (Add s) ds       = Just ("ID: " ++ show (size ds) ++ "\n", addToStore ds s)
processCommand (Get i) ds       = case integerToFin i (size ds) of
                                    Nothing  => Just ("Den Index gibt es nicht!\n", ds)
                                    Just idx => Just ((index idx $ items ds) ++ "\n", ds)
processCommand Quit    ds       = Nothing
processCommand (Invalid msg) ds = Just (msg ++ "\n", ds)
processCommand Size          ds = Just ("No of items: " ++ show (size ds) ++ "\n", ds)
processCommand (Search infx) ds = Just (case map (\i => show (finToInteger i)
                                                     ++ ": " ++ index i (items ds) ++ "\n")
                                                 (findIndices (\el => isInfixOf infx el)
                                                              (items ds)) of
                                         []     => "No match.\n"
                                         items  => concat items
                                      , ds)

total
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds i = let cmd = parseInput i in
                    processCommand cmd ds

main : IO ()
main = do replWith (MkData _ []) "Command: " processInput
