module QR (qr) where

import Data.Matrix
import qualified Data.Vector as V
import Data.Number.BigFloat

type BF = BigFloat Prec50

norm vec = V.sum (V.map (^2) vec)

directSum1 mat = fromLists ((1 : take (ncols mat) (repeat 0)) : (map (0 :) (toLists mat)))

atest :: Matrix BF
atest = fromLists [[12,-51,4],[6,167,-68],[-4,24,-41]]

atest2 :: Matrix BF
atest2 = fromLists [[4,1,-2,2],[1,2,0,1],[-2,0,3,-2],[2,1,-2,-1]]

atest3 :: Matrix Double
atest3 = fromLists [[4,1,0,0],[1,2,0,0],[0,0,3,-2],[0,0,-2,-1]]

roundTo n x = (fromInteger (round (x * (fromInteger (10^n))))) * 10**(- (fromInteger n))

houseHolderMatrix a = elementwise (-) (identity (nrows a)) (fmap (2 *) (multStd (colVector v) (rowVector v)))
  where
    x = getCol 1 a
    alphaSqr = norm x
    u = V.imap (\i b -> if i == 0 then b - (sqrt alphaSqr) else b) x
    v = V.map (/ (sqrt (norm u))) u

getQs mat | nrows mat < 2 && ncols mat < 2 = []
getQs mat = let q = houseHolderMatrix mat in q : map directSum1 (getQs (minorMatrix 1 1 (multStd q mat)))

qr mat = let qs = getQs mat in (foldl1 multStd (map transpose qs), foldl1 multStd ((reverse qs) ++ [mat]))

shurForm mat = (iterate f (identity (nrows mat),mat)) !! 50
 where f (q_product,m) = let (q,r) = qr m in (multStd q_product q, multStd r q)

houseHolderMatrix2 k a = p
  where
    a_skk = a ! (k+1,k)
    alpha = negate (signum a_skk) * sqrt (sum [(a!(j,k))^2 | j <- [k+1..nrows a]])
    r = sqrt (0.5 * (alpha^2 - a_skk*alpha))
    v = V.fromList ((take k (repeat 0)) ++ (a_skk - alpha) / (2*r) : [(a ! (j,k)) / (2*r) | j <- [k+2..nrows a]])
    p = elementwise (-) (identity (nrows a)) (fmap (2*) (multStd (colVector v) (rowVector v)))

tridiagonal mat = tridiagonal' 1 mat

tridiagonal' k mat | k + 1 >= nrows mat = mat
tridiagonal' k mat =
  let q1 = houseHolderMatrix2 k mat
  in tridiagonal' (k+1) (foldl1 multStd [q1,mat,q1])

isTridiagonal epsilon mat = and [(mat ! (i,j)) < epsilon | i <- [1..nrows mat], j <- [1..ncols mat], abs (i-j) <= 1]

eig mat
 | isTridiagonal 1e-9 mat = shurForm mat
 | otherwise = shurForm (tridiagonal mat)

{-
q1 = houseHolderMatrix atest

q1a = minorMatrix 1 1 (multStd q1 atest)

q2 = directSum1 (houseHolderMatrix q1a)

q = multStd (transpose q1) (transpose q2)

r = multStd (multStd q2 q1) atest
-}
