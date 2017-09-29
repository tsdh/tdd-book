module Error

data Error : {a : Type} -> (str : String) -> Type where
  MkError : a -> Error {a} str

isTrue : Bool -> Type
isTrue True = ()
isTrue False = Error {a = Void} "IsNotTrue!"

fromTrue : (b : Bool) -> isTrue b -> Integer
fromTrue True _ = 0
fromTrue False (MkError a) = absurd a
