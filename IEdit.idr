import Data.Vect

total
invert : Bool -> Bool
invert False = True
invert True  = False

total
allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

total
allLengths2 : List String -> List Nat
allLengths2 = map length

total
xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

mutual
  total
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  total
  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

total
allLengthsV : Vect n String -> Vect n Nat
allLengthsV [] = []
allLengthsV (x :: xs) = length x :: allLengthsV xs


total
allLengthsV2 : Vect n String -> Vect n Nat
allLengthsV2 = map length

total
insertionSort : Ord a => Vect n a -> Vect n a
insertionSort [] = []
insertionSort (x :: xs) = insert x $ insertionSort xs
                            where
                              insert : Ord a => a -> Vect k a -> Vect (S k) a
                              insert x [] = [x]
                              insert x (y :: xs) = if x < y
                                                   then x :: y :: xs
                                                   else y :: insert x xs


-- TODO: why can't that be total
quickSort : Ord a => List a -> List a
quickSort [] = []
quickSort (x :: xs) = let (lt, gteq) = partition (< x) xs
                      in quickSort lt ++ [x] ++ quickSort gteq

-- TODO: This does not compile.  I guess I have to use the fact that
--       partition returns a pair of dependent pairs, but how...
--
-- quickSortV : Ord a => Vect n a -> Vect n a
-- quickSortV [] = []
-- quickSortV (x :: xs) = let (lt, gteq) = partition (< x) xs
--                        in quickSortV lt ++ [x] ++ quickSortV gteq


total
vLength : Vect n a -> Nat
vLength {n} _ = n

total
lReverse : List a -> List a
lReverse [] = []
lReverse (x :: xs) = (lReverse xs) ++ [x]

total
my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

total
vmap : (a -> b) -> Vect n a -> Vect n b
vmap f [] = []
vmap f (x :: xs) = f x :: vmap f xs

