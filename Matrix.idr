import Data.Vect

transposeMatrix : Vect m (Vect n a) -> Vect n (Vect m a)
transposeMatrix [] {n} = replicate n []
transposeMatrix (x :: xs) = let xsTrans = transposeMatrix xs in
                            zipWith (::) x xsTrans

addMatrix : Num a => Vect m (Vect n a) -> Vect m (Vect n a) -> Vect m (Vect n a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

total
multMatrix : Num a => Vect m (Vect n a) -> Vect n (Vect o a) -> Vect m (Vect o a)
multMatrix [] xs = []
multMatrix xs ys {m} = let ysTrans = transposeMatrix ys in
                       zipWith (\x, ys' => map (sum . (zipWith (*) x)) ys')
                               xs
                               (replicate m ysTrans)



