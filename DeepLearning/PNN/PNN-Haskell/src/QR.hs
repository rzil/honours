module QR (qr) where

import Data.Matrix
import qualified Data.Vector as V

norm vec = V.sum (V.map (^2) vec)

directSum1 mat = fromLists ((1 : take (ncols mat) (repeat 0)) : (map (0 :) (toLists mat)))

atest :: Matrix Double
atest = fromLists [[12,-51,4],[6,167,-68],[-4,24,-41]]

houseHolderMatrix a = elementwise (-) (identity (nrows a)) (fmap (2 *) (multStd (colVector v) (rowVector v)))
  where
    x = getCol 1 a
    alphaSqr = norm x
    u = V.imap (\i b -> if i == 0 then b - (sqrt alphaSqr) else b) x
    v = V.map (/ (sqrt (norm u))) u

getQs mat | nrows mat < 2 && ncols mat < 2 = []
getQs mat = let q = houseHolderMatrix mat in q : map directSum1 (getQs (minorMatrix 1 1 (multStd q mat)))

qr mat = let qs = getQs mat in (foldl1 multStd (map transpose qs), foldl1 multStd ((reverse qs) ++ [mat]))

shurForm mat = iterate f mat
 where f m = let (q,r) = qr m in multStd r q

{-
q1 = houseHolderMatrix atest

q1a = minorMatrix 1 1 (multStd q1 atest)

q2 = directSum1 (houseHolderMatrix q1a)

q = multStd (transpose q1) (transpose q2)

r = multStd (multStd q2 q1) atest
-}
