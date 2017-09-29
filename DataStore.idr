module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

%name Schema s, s1, s2

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (s1 .+. s2) = (SchemaType s1, SchemaType s2)


record DataStore where
  constructor MkData
  schema : Schema
  size   : Nat
  items  : Vect size (SchemaType schema)

%name DataStore ds

addToStore : (ds : DataStore) -> SchemaType (schema ds)  -> DataStore
addToStore (MkData schema size items) newitem
  = MkData schema (S size) (addToData items)
  where addToData : Vect os (SchemaType schema) -> Vect (S os) (SchemaType schema)
        addToData [] = [newitem]
        addToData (y :: xs) = y :: addToData xs


data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Invalid : String -> Command schema
  Size : Command schema
  Search : String -> Command schema
  Quit : Command schema

mutual
  parseSchema' : List String -> Schema -> Maybe Schema
  parseSchema' xs schema
    = case xs of
        [] => Just schema
        _ => do xsSchema <- parseSchema xs
                Just (schema .+. xsSchema)

  parseSchema : List String -> Maybe Schema
  parseSchema ("String" :: xs) = parseSchema' xs SString
  parseSchema ("Int" :: xs) = parseSchema' xs SInt
  parseSchema _ = Nothing

total
parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString arg = getQuoted (unpack arg)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                              (quoted, '"' :: rest) => Just (pack quoted, ltrim $ pack rest)
                              _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt arg = case span isDigit arg of
                         ("", rest) => Nothing
                         (digit, rest) => Just (cast digit, ltrim rest)
parsePrefix (s1 .+. s2) arg = do (v1, r) <- parsePrefix s1 arg
                                 (v2, r) <- parsePrefix s2 r
                                 Just ((v1, v2), r)

total
parseArgBySchema : (schema : Schema) -> (arg : String) -> Either String (SchemaType schema)
parseArgBySchema schema arg = case parsePrefix schema arg of
                                Just (res, "") => Right res
                                Just (_, rem) => Left $ "superfluous input: " ++ rem
                                Nothing => Left $ "Cannot parse '" ++ arg ++ "'"

parseCommand : (schema : Schema) -> String -> String -> Command schema
parseCommand _ "schema" arg = case parseSchema $ words arg of
                                Nothing => Invalid $ "Invalid schema spec: " ++ arg
                                Just schema => SetSchema schema
parseCommand schema "add" arg = case parseArgBySchema schema arg of
                                  Left err => Invalid $ "Invalid arg to add: " ++ err
                                  Right arg' => Add arg'
parseCommand _"get" arg = if all isDigit $ unpack arg
                            then Get (cast arg)
                            else Invalid $ "Invalid arg to get: " ++ arg
parseCommand _ "quit" arg = Quit
parseCommand _ "size" arg = Size
parseCommand _ "search" arg = Search arg
parseCommand _ cmd arg = Invalid $ "Unknown command '" ++ cmd ++ "'"

total
parseInput : (schema : Schema) -> String -> Command schema
parseInput schema input =
  case span (/= ' ') input of
    (cmd, arg) => let arg = trim arg in
                    parseCommand schema cmd arg

total
showItem : SchemaType schema -> String
showItem {schema = SString} item = item
showItem {schema = SInt} item = show item
showItem {schema = (x .+. y)} (i1, i2) = showItem i1 ++ ", " ++ showItem i2

total
setSchema : (ds : DataStore) -> Schema -> Maybe DataStore
setSchema ds schema = case size ds of
                        Z => Just $ MkData schema _ []
                        S k => Nothing

total
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput ds i
  = case parseInput (schema ds) i of
      (SetSchema newSchema)
        => case setSchema ds newSchema of
             Nothing => Just ("Can't update schema!\n", ds)
             Just newDs => Just ("Schema updated\n", newDs)
      (Add s) => Just ("ID: " ++ show (size ds) ++ "\n", addToStore ds s)
      (Get i)
        => case integerToFin i (size ds) of
             Nothing  => Just ("No such index!\n", ds)
             Just idx => Just (showItem (index idx $ items ds) ++ "\n", ds)
      Quit => Nothing
      (Invalid msg) => Just (msg ++ "\n", ds)
      Size => Just ("No of items: " ++ show (size ds) ++ "\n", ds)
      (Search infx)
        => Just (case map (\i => show (finToInteger i) ++ ": "
                                 ++ showItem (index i (items ds))
                                 ++ "\n")
                          (Data.Vect.findIndices (\el => isInfixOf infx (showItem el))
                                                             (items ds)) of
                   [] => "No match.\n"
                   items  => concat items
               , ds)

main : IO ()
main = do replWith (MkData SString _ []) "Command: " processInput
