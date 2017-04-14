{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Data.Array.Accelerate.Dif where

import Prelude as P

import Data.Function (on)
import Data.NumInstances ()

--import Data.Random.Normal
import Debug.Trace

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as Interp


type Matrix a = Array DIM2 a
type Activation a = (Exp a -> Exp a, Exp a -> Exp a)

--
testMatrix :: Matrix Double
testMatrix = fromList (Z:.3:.3) [0..]

testVector :: Vector Double
testVector = fromList (Z:.3) [1..]

testActivation :: Activation Double
testActivation = (log, recip)

testLayer :: Dif -> Dif
testLayer = layerWRTInput (use testMatrix) (use testVector) testActivation

testInput :: Vector Double
testInput = fromList (Z:.3) [4,5,6]

test = testLayer (dId (use testInput))
--

layerWRTParameters :: Acc (Matrix Double) -> Acc (Vector Double) -> Activation Double -> Dif -> Dif
layerWRTParameters a b (f, f') (D x _) = D (A.map f y) jacobian
 where
  y = b ^+^ (a !* x)
  rows = A.length b
  n = A.length x
  f'y = A.map f' y
  jacobian = generate (index2 rows (rows * n)) jacobianGenerator A.++ diagonal f'y
  jacobianGenerator sh = let Z :. r :. c = unlift sh in ifThenElse (r*n A.<= c A.&& c A.< (r+1)*n) ((x A.! (lift (Z:.(c-r*n)))) * f'y A.! (lift (Z:.r))) 0

layerWRTInput :: Acc (Matrix Double) -> Acc (Vector Double) -> Activation Double -> Dif -> Dif
layerWRTInput a b f = (dActivation f) . (dTranslate b) . (dLinear a)

dLinear :: Acc (Matrix Double) -> Dif -> Dif
dLinear matrix = (matrix !*) >-< (const matrix)

dTranslate :: Acc (Vector Double) -> Dif -> Dif
dTranslate vector = (vector ^+^) >-< (const (identityN (A.length vector)))

-- more efficiently
--dTranslate vector (D x x') = D (vector ^+^ x) x'

dActivation :: Activation Double -> Dif -> Dif
dActivation (f,d) = (A.map f) >-< (diagonal . A.map d)

-- more efficiently
{-
dActivation (f,d) (D x x') = D (A.map f x) (imap transform x')
 where
  transform sh e = let Z :. row :. _ = unlift sh :: (Z :. Exp Int :. Exp Int) in (dx A.! (lift (Z:.row))) * e
  dx = A.map d x
-}

-- the derivative should be a map from parameter space to the space of the value
data Dif = D { dVal :: Acc (Vector Double), deriv :: Acc (Matrix Double) }

shapeIsSquare :: Exp ((Z :. Int) :. Int) -> Exp Int
shapeIsSquare sh = let Z :. r :. c = unlift sh in (ifThenElse (r A.== c) 1 0)

diagonal :: Acc (Vector Double) -> Acc (Matrix Double)
diagonal xs = generate (index2 n n) diagonalGenerator
 where
  diagonalGenerator sh = let Z :. r :. c = unlift sh in (ifThenElse (r A.== c) (xs A.! (lift (Z:.r))) 0)
  n = A.length xs

identityN :: Exp Int -> Acc (Matrix Double)
identityN n = generate (index2 n n) (A.fromIntegral . shapeIsSquare)

zeroN :: Exp Int -> Acc (Matrix Double)
zeroN n = fill (index2 n n) 0

dId :: Acc (Vector Double) -> Dif
dId x = D x (identityN (A.length x))

dConst :: Acc (Vector Double) -> Dif
dConst x = D x (zeroN (A.length x))

instance Show Dif where show d = "D " P.++ (show $ dVal d) P.++ " ..."

-- matrix multiplication using Accelerate
mmMult :: A.Num a => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
mmMult arr brr = result
 where
  result = fold (+) 0 (A.zipWith (*) arrRepl brrRepl)
  --check = rowsA A.== colsB   -- what to do if this is false?
  trr = transpose brr
  arrRepl = A.replicate (lift (Z :. All   :. colsB :. All)) arr
  brrRepl = A.replicate (lift (Z :. rowsA :. All   :. All)) trr
  (Z :. rowsA :. colsA) = unlift (shape arr) :: Z :. Exp Int :. Exp Int
  (Z :. rowsB :. colsB) = unlift (shape brr) :: Z :. Exp Int :. Exp Int

infix 6 !*!
(!*!) :: A.Num a => Acc (Matrix a) -> Acc (Matrix a) -> Acc (Matrix a)
(!*!) = mmMult

-- matrix addition using Accelerate
mmAdd arr brr = A.zipWith (+) arr brr

-- matrix subtraction using Accelerate
mmMinus arr brr = A.zipWith (-) arr brr

-- matrix-vector multiplication using Accelerate
(!*) :: A.Num a => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
(!*) matrix vector = reshape (lift (Z :. mCols)) result
 where
  result = matrix !*! columnMatrix
  columnMatrix = reshape (lift (Z :. vCols :. (1::Int))) vector
  (Z :. vCols) = unlift (shape vector) :: Z :. Exp Int
  (Z :. mCols :. _) = unlift (shape matrix) :: Z :. Exp Int :. Exp Int

-- vector addition using Accelerate
(^+^) :: A.Num a => Acc (Vector a) -> Acc (Vector a) -> Acc (Vector a)
(^+^) vectorA vectorB = A.zipWith (+) vectorA vectorB

-- chain rule
infix 0 >-<
(>-<) :: (Acc (Vector Double) -> Acc (Vector Double)) -> (Acc (Vector Double) -> Acc (Matrix Double)) -> Dif -> Dif
f >-< d = \ (D u u') -> D (f u) ((d u) !*! u')
