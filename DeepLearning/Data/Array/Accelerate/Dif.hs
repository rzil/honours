{-# LANGUAGE TypeOperators, FlexibleContexts #-}

{-
Author: Ruben Zilibowitz
Date: 14 April 2017
Inspired by ideas and code by Conal Elliot
http://conal.net/blog/posts/beautiful-differentiation

Here we are concerned with concurrency and we are using Accelerate to achieve
fast performance.
-}

module Data.Array.Accelerate.Dif where

import Prelude as P
import Data.Array.Accelerate as A

type Matrix a = Array DIM2 a
type Activation a = (Exp a -> Exp a, Exp a -> Exp a)   -- a function paired with its derivative

-- dVal is the output value of some differentiable function for some input
-- deriv is the Jacobian at this input point
data Dif = D { dVal :: Acc (Vector Double), deriv :: Acc (Matrix Double) }

instance Show Dif where show d = "D " P.++ (show $ dVal d) P.++ " ..."

matrixDimensions :: Elt e => Acc (Matrix e) -> (Exp Int, Exp Int)
matrixDimensions matrix = let Z :. rows :. columns = unlift (shape matrix) in (rows,columns)

neuralNet :: [Acc (Vector Double)] -> [Acc (Matrix Double)] -> Activation Double -> Acc (Vector Double) -> Dif
neuralNet (b:bs) (a:as) f = (dAffine_wrt_parameters_and_input b a) . (nn bs as f)

nn :: [Acc (Vector Double)] -> [Acc (Matrix Double)] -> Activation Double -> Acc (Vector Double) -> Dif
nn [b] [a] f = (dActivation f) . (dAffine_wrt_parameters b a)
nn (b:bs) (a:as) f = (dActivation f) . (dAffine_wrt_parameters_and_input b a) . (nn bs as f)

dAffine_wrt_parameters_and_input :: A.Acc (A.Vector Double) -> A.Acc (Matrix Double) -> Dif -> Dif
dAffine_wrt_parameters_and_input b a x = let px = p (dVal x) in D (dVal px) (deriv px A.++ (deriv (q x)))
 where
  p = dAffine_wrt_parameters b a
  q = dAffine b a

jacobian_wrt_matrix_multiply :: A.Num a => Exp Int -> Acc (Vector a) -> Acc (Matrix a)
jacobian_wrt_matrix_multiply rows x = generate (index2 rows (rows * n)) generator
 where
  n = A.length x
  generator sh = let Z :. r :. c = unlift sh in ifThenElse (r*n A.<= c A.&& c A.< (r+1)*n) (x A.! (lift (Z:.(c-r*n)))) 0

-- dAffine_wrt_parameters computes a*x + b, where
-- a is a matrix
-- x is a column vector
-- b is a column vector
-- the return value is the affine transform a*x + b
-- together with a matrix representing the jacobian w.r.t. the elements in a and b
-- NB: This is not the derivative w.r.t. x.
dAffine_wrt_parameters :: Acc (Vector Double) -> Acc (Matrix Double) -> Acc (Vector Double) -> Dif
dAffine_wrt_parameters b a x = D (b ^+^ (a !* x)) ((jacobian_wrt_matrix_multiply n x) A.++ (identityN n))
 where n = A.length b

-- dAffine computes a*x + b, where
-- a is a matrix
-- x is a column vector
-- b is a column vector
-- the return value is the affine transform a*x + b
-- together with a matrix representing the jacobian w.r.t. x
dAffine :: Acc (Vector Double) -> Acc (Matrix Double) -> Dif -> Dif
dAffine b a = (dTranslate b) . (dLinear a)

-- dLinear computes a*x, where
-- a is a matrix
-- x is a column vector
-- the return value is a*x
-- together with the jacobian w.r.t x, which for a linear
-- transformation is just a.
dLinear :: Acc (Matrix Double) -> Dif -> Dif
dLinear matrix = (matrix !*) >-< (const matrix)

-- dTranslate computes b + x
-- b is a vector
-- x is a vector
-- the return value is b + x
-- together with a matrix representing the jacobian w.r.t. x,
-- which happens to be the identity.
dTranslate :: Acc (Vector Double) -> Dif -> Dif
dTranslate vector = (vector ^+^) >-< (const (identityN (A.length vector)))

-- more efficiently
--dTranslate vector (D x x') = D (vector ^+^ x) x'

-- dActivation takes a differentiable function and its derivative (f,f')
-- it applies this function f to every element in the vector x.
-- It returns the result, together with the jacobian matrix w.r.t. x.
dActivation :: Activation Double -> Dif -> Dif
dActivation (f,f') = (A.map f) >-< (diagonal . A.map f')

-- more efficiently
{-
dActivation (f,d) (D x x') = D (A.map f x) (imap transform x')
 where
  transform sh e = let Z :. row :. _ = unlift sh :: (Z :. Exp Int :. Exp Int) in (dx A.! (lift (Z:.row))) * e
  dx = A.map d x
-}

dSquareError :: Exp Double -> Dif -> Dif
dSquareError a = dActivation (\x -> (x - a) P.^ 2,\x -> 2 * (x - a))

-- converts a vector to a column matrix
columnMatrix :: Elt e => Acc (Vector e) -> Acc (Matrix e)
columnMatrix vector = reshape (lift (Z:.n:.(1 :: Int))) vector
 where (Z :. n) = unlift (shape vector) :: Z :. Exp Int

-- converts a vector to a column matrix
rowMatrix :: Elt e => Acc (Vector e) -> Acc (Matrix e)
rowMatrix vector = reshape (lift (Z:.(1 :: Int):.n)) vector
 where (Z :. n) = unlift (shape vector) :: Z :. Exp Int

-- http://eli.thegreenplace.net/2016/the-softmax-function-and-its-derivative/

oneHotEncoding :: (Elt e, P.Num e) => Int -> Int -> Vector e
oneHotEncoding k n = fromList (Z :. n) [if i P.== k then 1 else 0 | i <- [0..n]]

dCrossEntropy :: Acc (Vector Double) -> Dif -> Dif
dCrossEntropy p = dLinear (rowMatrix (A.map negate p)) . dActivation (log,recip)

-- dCrossEntropyOneHotEncoding i (D x x')
--  is equivalent to
-- dCrossEntropy (use (oneHotEncoding i (A.length x)))
--  but this one is more efficient
dCrossEntropyOneHotEncoding :: Exp Int -> Dif -> Dif
dCrossEntropyOneHotEncoding i (D x x') = D y y'
 where
  y  = reshape (lift (Z:.(1::Int))) (unit (negate (log q)))
  y' = rowMatrix (A.map (neg_recip_q *) (slice x' (lift (Z:.i:.All))))
  q = x A.! (lift (Z:.i))
  neg_recip_q = negate (recip q)

-- computes the softmax function of the input
-- and the jacobian.
dSoftmax :: Dif -> Dif
dSoftmax = softmax >-< (softmaxJacobian . softmax)

-- more efficiently
--dSoftmax (D x x') = let smx = softmax x in D smx (softmaxJacobian smx)

softmax :: Acc (Vector Double) -> Acc (Vector Double)
softmax vec = A.map ((/ divisor) . exp) vec_reduced
 where
  d = negate (the (A.maximum vec))
  vec_reduced = A.map (d +) vec
  divisor = the (A.sum (A.map exp vec_reduced))

kroneckerDelta :: A.Num a => Exp Int -> Exp Int -> Exp a
kroneckerDelta i j = ifThenElse (i A.== j) 1 0

softmaxJacobian :: Acc (Vector Double) -> Acc (Matrix Double)
softmaxJacobian softmaxedVector = A.imap (\sh a -> let Z :. r :. c = unlift sh in (ifThenElse (r A.== c) (softmaxedVector A.! (lift (Z :. r))) 0) + (negate a)) (A.zipWith (*) (A.replicate (lift (Z :. All :. n)) softmaxedVector) (A.replicate (lift (Z :. n :. All)) softmaxedVector))
 where n = A.length softmaxedVector

{-
softmaxJacobian2 :: Acc (Vector Double) -> Acc (Matrix Double)
softmaxJacobian2 softmaxedVector = generate (index2 n n) generator
 where
  s :: Exp Int -> Exp Double
  s i = softmaxedVector A.! (lift (Z :. i))
  ds j i = (s i)*((kroneckerDelta i j) - (s j))
  generator sh = let Z :. r :. c = unlift sh :: (Z :. Exp Int :. Exp Int) in ds r c
  n = A.length softmaxedVector
-}

shapeIsSquare :: Exp ((Z :. Int) :. Int) -> Exp Int
shapeIsSquare sh = let Z :. r :. c = unlift sh in kroneckerDelta r c

diagonal :: Acc (Vector Double) -> Acc (Matrix Double)
diagonal xs = generate (index2 n n) diagonalGenerator
 where
  diagonalGenerator sh = let Z :. r :. c = unlift sh in (ifThenElse (r A.== c) (xs A.! (lift (Z:.r))) 0)
  n = A.length xs

-- identity matrix of size n * n
identityN :: Exp Int -> Acc (Matrix Double)
identityN n = generate (index2 n n) (A.fromIntegral . shapeIsSquare)

-- zero matrix of size n * n
zeroN :: Exp Int -> Acc (Matrix Double)
zeroN n = fill (index2 n n) 0

-- Take a vector and makes and assumes the derivative
-- w.r.t. every element is 1.
dId :: Acc (Vector Double) -> Dif
dId x = D x (identityN (A.length x))

-- Take a vector of constants and return differentiable type
dConst :: Acc (Vector Double) -> Dif
dConst x = D x (zeroN (A.length x))

-- matrix multiplication using Accelerate
mmMult :: A.Num a => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
mmMult arr brr = result
 where
  result = fold (+) 0 (A.zipWith (*) arrRepl brrRepl)
  --check = rowsA A.== colsB   -- what to do if this is false?
  trr = transpose brr
  arrRepl = A.replicate (lift (Z :. All   :. colsB :. All)) arr
  brrRepl = A.replicate (lift (Z :. rowsA :. All   :. All)) trr
  (rowsA,colsA) = matrixDimensions arr
  (rowsB,colsB) = matrixDimensions brr

-- matrix/vector multiplication using Accelerate
mvMult :: A.Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvMult matrix vector = reshape (lift (Z :. rows)) result
 where
  result = matrix !*! (columnMatrix vector)
  (rows,_) = matrixDimensions matrix

-- matrix multiplication operator
infix 6 !*!
(!*!) :: A.Num a => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
(!*!) = mmMult

-- matrix-vector multiplication operator
infixr 7 !*
(!*) :: A.Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
(!*) = mvMult

-- vector addition using Accelerate
infix 5 ^+^
(^+^) :: A.Num a => Acc (Vector a) -> Acc (Vector a) -> Acc (Vector a)
(^+^) = A.zipWith (+)

-- chain rule
infix 0 >-<
(>-<) :: (Acc (Vector Double) -> Acc (Vector Double)) -> (Acc (Vector Double) -> Acc (Matrix Double)) -> Dif -> Dif
f >-< d = \ (D u u') -> D (f u) ((d u) !*! u')

difAdd (D x x') (D y y') = D (x ^+^ y) (A.zipWith (+) x' y')
