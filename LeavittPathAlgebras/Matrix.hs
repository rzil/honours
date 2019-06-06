module Matrix (content, inverse, isUnimodular, pivots, eigenvaluesRealPositive, rref, echelonForm, isPositiveDefinite, mainDiagonal, fromMainDiagonal, isNilpotent, fastDeterminant, determinant, characteristicPolynomial, toList, nullity, rank, ncols, nrows, (<|>), (<->), solve, solve1, identity, elementwise, (!), matrix, transpose, Matrix.isZero, Matrix) where

import qualified Data.List as L
import Data.Ratio
import Orthogonals (determinantClow)
import Polynomial hiding (isZero)
import Data.Function (on)

newtype Matrix a = Matrix {content :: [[a]]}

instance Eq a => Eq (Matrix a) where
  (Matrix xs) == (Matrix ys) = xs == ys

instance Show a => Show (Matrix a) where
  show (Matrix rs) = unlines [L.intercalate "\t\t" (map show r) | r <- rs]

instance Functor Matrix where
  fmap f (Matrix m) = Matrix (map (map f) m)

instance Num a => Num (Matrix a) where
  xs + ys = elementwise (+) xs ys
  negate = fmap negate
  xs * ys = Matrix [[sum (zipWith (*) c r) | c <- content (transpose xs)] | r <- content ys]
  fromInteger n = error "cannot convert integer"
  abs m = undefined
  signum m = undefined

-- https://rosettacode.org/wiki/Reduced_row_echelon_form#Haskell

rref_ :: (Fractional b, Eq b) => [[b]] -> [[b]]
rref_ m = f m 0 [0 .. rows - 1]
  where rows = length m
        cols = length $ head m
        
        f m _    []              = m
        f m lead (r : rs)
            | indices == Nothing = m
            | otherwise          = f m' (lead' + 1) rs
          where indices = L.find p l
                p (col, row) = m !! row !! col /= 0
                l = [(col, row) |
                    col <- [lead .. cols - 1],
                    row <- [r .. rows - 1]]
 
                Just (lead', i) = indices
                newRow = map (/ m !! i !! lead') $ m !! i
 
                m' = zipWith g [0..] $
                    replace r newRow $
                    replace i (m !! r) m
                g n row
                    | n == r    = row
                    | otherwise = zipWith h newRow row
                  where h = subtract . (* row !! lead')

-- | Reduced row echelon form
rref :: (Fractional a, Eq a) => Matrix a -> Matrix a
rref (Matrix m) = Matrix (rref_ m)

{- Replaces the element at the given index. -}
replace :: Int -> a -> [a] -> [a]
replace n e l = a ++ e : b
  where (a, _ : b) = splitAt n l

inverse :: (Fractional a, Eq a) => Matrix a -> Maybe (Matrix a)
inverse m = solve m (identity (ncols m))

isUnimodular :: Integral a => Matrix a -> Bool
isUnimodular m = abs (determinant m) == 1

echelonForm_ :: (Num a, Eq a) => [[a]] -> (a,[[a]])
echelonForm_ [] = (1,[])
echelonForm_ ([]:rs) = (1,[]:rs)
echelonForm_ ((0:xs):rs) = maybe
  (let (x,m) = echelonForm_ (xs : (map tail rs)) in (x,map (0:) m))
  (\n -> let (x,m) = echelonForm_ ((rs !! n) : (replace n (0:xs) rs)) in (negate x,m))
  (L.findIndex ((0 /=) . head) rs)
echelonForm_ ((p:ps):rs) = let (x,m) = echelonForm_ [zipWith (-) (map (p *) (q:qs)) (map (q *) (p:ps)) | (q:qs) <- rs] in (p^(length rs) * x,(p:ps) : m)

-- | Row echelon form with no divisions. Includes scaling factor for computing determinants.
echelonForm :: (Num a, Eq a) => Matrix a -> (a,Matrix a)
echelonForm (Matrix m) = (x,Matrix e)
 where (x,e) = echelonForm_ m

mainDiagonal_ :: [[t]] -> [t]
mainDiagonal_ [] = []
mainDiagonal_ ([]:_) = []
mainDiagonal_ ((p:_):rs) = p : mainDiagonal_ (map tail rs)

trace :: Num c => Matrix c -> c
trace = sum . mainDiagonal

mainDiagonal :: Matrix a -> [a]
mainDiagonal (Matrix m) = mainDiagonal_ m

fromMainDiagonal :: Num a => [a] -> Matrix a
fromMainDiagonal xs = matrix (length xs) (length xs) (\(i,j) -> if i == j then xs !! (i-1) else 0)

fastDeterminant :: (Fractional t, Eq t) => Matrix t -> t
fastDeterminant m = let (x,e) = echelonForm m in (product (mainDiagonal e)) / x

determinant :: Num a => Matrix a -> a
determinant (Matrix m) = Orthogonals.determinantClow m

characteristicPolynomial :: (Eq t,Num t) => Matrix t -> Polynomial t
characteristicPolynomial m = determinant (elementwise (-) (fmap (polynomial . (:[])) m) (fmap (x *) (identity (ncols m))))
 where x = basis !! 1

nullity :: (Num a, Eq a) => Matrix a -> Int
nullity m = length $ filter (all (0 ==)) $ content $ snd $ echelonForm m

rank :: (Num a, Eq a) => Matrix a -> Int
rank m = nrows m - nullity m

ncols :: Matrix a -> Int
ncols (Matrix m) = length (head m)

nrows :: Matrix t -> Int
nrows (Matrix m) = length m

(<|>) :: Matrix a -> Matrix a -> Matrix a
(<|>) (Matrix xs) (Matrix ys) = Matrix $ zipWith (++) xs ys

(<->) :: Matrix a -> Matrix a -> Matrix a
(<->) (Matrix xs) (Matrix ys) = Matrix $ xs ++ ys

-- assumes matrix is in row echelon form
-- returns the indices of the pivot columns
pivots :: (Num a, Eq a) => Matrix a -> [Maybe Int]
pivots m = [L.findIndex (0 /=) row | row <- content m]

solve :: (Fractional a, Eq a) => Matrix a -> Matrix a -> Maybe (Matrix a)
solve left right
  | rank left < rank aug = Nothing
  | otherwise =
     let m = rref aug
         pivs = pivots m
         a = [(p,row) | (Just p,row) <- zip pivs (map (drop (ncols left)) (content m))]
         b = [(i,take (ncols right) (repeat 0)) | i <- [0..(ncols left - 1)] L.\\ [p | Just p <- pivs]]
     in Just (Matrix (map snd (L.sortBy (on compare fst) (a ++ b))))
 where aug = left <|> right

solve1 :: (Fractional a, Eq a) => Matrix a -> [a] -> Maybe [a]
solve1 mat vec = do
  solution <- solve mat (Matrix (map (:[]) vec))
  return (concat (content solution))

matrix :: Int -> Int -> ((Int, Int) -> a) -> Matrix a
matrix rows cols f = Matrix [[f (r,c) | c <- [1..cols]] | r <- [1..rows]]

identity :: Num a => Int -> Matrix a
identity n = matrix n n $ \(i,j) -> if i == j then 1 else 0

elementwise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
elementwise f (Matrix xs) (Matrix ys) = Matrix (zipWith (zipWith f) xs ys)

(!) :: Matrix a -> (Int, Int) -> a
(!) (Matrix m) (r,c) = (m !! (r-1)) !! (c-1)

transpose :: Matrix a -> Matrix a
transpose (Matrix m) = Matrix (L.transpose m)

-- | https://en.wikipedia.org/wiki/Nilpotent_matrix
isNilpotent :: Real a => Matrix a -> Bool
isNilpotent m = any isZero (take (1 + ncols m) (iterate (m *) (identity (ncols m))))
--isNilpotent m = (toPrimitive (characteristicPolynomial m)) == xToTheN
-- where xToTheN = basis !! (ncols m)

toList :: Matrix a -> [a]
toList (Matrix m) = concat m

isZero :: (Num a, Eq a) => Matrix a -> Bool
isZero m = all (0 ==) (toList m)

leadingPrincipalMinors :: Matrix a -> [Matrix a]
leadingPrincipalMinors m = [Matrix (map (take k) (take k (content m))) | k <- [1..ncols m]]

-- https://en.wikipedia.org/wiki/Sylvester%27s_criterion
-- assumes matrix is Hermitian (or symmetric)
sylvestersCriterion :: (Ord a, Num a) => Matrix a -> Bool
sylvestersCriterion m = all ((> 0) . determinant) (leadingPrincipalMinors m)

isPositiveDefinite :: (Ord a, Num a) => Matrix a -> Bool
isPositiveDefinite m = sylvestersCriterion (m + (transpose m))

eigenvaluesRealPositive :: (Fractional a, Eq a) => Matrix a -> Bool
eigenvaluesRealPositive m = countNegativeRealRoots p == 0 && countPositiveRealRoots p == degree p
 where p = squarefreePart (characteristicPolynomial m)

-- example
ns = Matrix [[-1, 1, 0, 0, 0, 0, 0, 0, 1, 1],[ 1, -1, 0, 0, 0, 0, 1, 0, 1, 1],[ 1, 1, -1, 0, 0, 0, 0, 1, 0, 1],[ 0, 0, 1, -1, 0, 0, 0, 0, 0, 1],[ 0, 0, 0, 1, -1, 1, 0, 0, 0, 1],[ 0, 0, 0, 0, 0, -1, 1, 0, 0, 1],[ 0, 0, 0, 0, 0, 0, -1, 0, 1, 1],[ 0, 0, 0, 0, 0, 0, 0, -1, 0, 1],[ 0, 0, 0, 0, 0, 0, 0, 0, -1, 1]]

ms = Matrix (map init (content ns))
