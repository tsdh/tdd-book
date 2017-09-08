import Data.Vect
import Data.Fin

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex x xs {n} = case integerToFin x n of
                      Nothing => Nothing
                      Just idx => Just $ index idx xs

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z _ = []
vectTake (S k) (x :: xs) = x :: vectTake k xs


sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos xs ys {n} = case integerToFin pos n of
                             Nothing => Nothing
                             Just idx => Just $ (index idx xs) + (index idx ys)

head : (xs : List a) -> {auto p : isCons xs = True} -> a
head (x :: xs) = x


