module Matrix

%default total

{-
-- A matrix is a Vect (Vect ... (Vect a))
Matrix : Vect n Nat -> Type -> Type
Matrix dims a = foldr Vect a dims
-}

data Matrix : Vect n Nat -> Type -> Type where
  Mat : {dims : Vect n Nat} -> foldr Vect a dims -> Matrix dims a

unMatrix : {dims : Vect n Nat} -> Matrix dims a -> foldr Vect a dims
unMatrix (Mat vs) = vs

instance Functor (Matrix {n} dims) where
  -- map : (a -> b) -> Matrix dims a -> Matrix dims b

  {-
  map f (Mat {dims = Nil} a) = Mat $ f a
  map f (Mat {dims} vs) = Mat $ map (unMatrix . map f . Mat) vs
  -}

  map f (Mat {dims} vs) = case dims of
    Nil => Mat $ f vs
    _   => Mat $ map (unMatrix . map f . Mat) vs

  -- map f = Mat . map f . unMatrix

-- Generate a vector
vect : {n : Nat} -> (Nat -> a) -> Vect n a
vect {n = Z}   _   = []
vect {n = S n} gen = gen (S n) :: vect gen

-- A matrix where each cell holds its position
iterate : (dims : Vect n Nat) -> Matrix dims (Vect n Nat)
iterate = ?iter

-- Generate a matrix
-- {v = Vect n Nat} -> {dims: v} -> (v -> a) -> Matrix dims a
matrix : {dims : Vect n Nat} -> (Vect n Nat -> a) -> Matrix dims a
matrix {dims} gen = map gen ixes where
  ixes : Matrix dims (Vect n Nat)
  ixes = ?ixes -- iterate dims

-- Identity matrix where the diagonal is all 1s and everwhere else is 0s
-- TODO - generalize this to RingWithUnity or something
identity : {dims : Vect (S n) Nat} -> Matrix dims Integer
-- identity [] = unity
identity {dims} = matrix gen where
  gen : Vect (S n) Nat -> Integer
  gen vec = if (and $ map (== (head vec)) vec) then 1 else 0

-- Example: A row x cols matrix
Matrix2 : Nat -> Nat -> Type -> Type
Matrix2 rows cols a = Matrix [rows, cols] a

-- A 2x2 identity matrix, written out
identity2 : Matrix [2,2] Integer
identity2 = Mat $ [[1,0],
                   [0,1]]

-- A 2x2 identity matrix
identity2' : Matrix [2,2] Integer
identity2' = identity {dims = [2,2]}

{-
total
mtrans : Matrix a n m -> Matrix a m n
mtrans [] = repeat _ []
mtrans (r :: rs) = zipWith (::) r (mtrans rs)

dotp : (Num a) => Vect n a -> Vect n a -> a
dotp [] _ = 0
dotp (x :: xs) (y :: ys) = x * y + dotp xs ys

mmult : (Num a) => Matrix a i j -> Matrix a j k -> Matrix a i k
mmult m n = zipWith vectMult m (repeat _ n')
    where n' = mtrans n
          vectMult : (Num a) => Vect j a -> Matrix a k j -> Vect k a
          vectMult v m = zipWith dotp m (repeat _ v)
-}
