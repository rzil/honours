module PNN where

import Data.Complex
import qualified Data.Vector as V
import Data.Matrix
import Data.Random.Normal

n = 100 :: Int
k = 3 :: Int

s = 2.1 :: Double
sd = 1 / sqrt(s * (fromIntegral n))
seed = 231

makeComplex (x:y:xs) = (x :+ y) : makeComplex xs
complexes = makeComplex (mkNormals' (0,sd) seed)

w_res = fromList (n-k) (n-k) complexes
w_in = fromList (n-k) k $ drop (nrows w_res * ncols w_res) complexes
x_res = V.fromList $ take (n-k) $ drop (nrows w_res * ncols w_res +
                                        nrows w_in * ncols w_in) complexes

-- function to predict
-- vector should have length k
as c = let c' = 0.01 * (fromIntegral c) in
  V.fromList [sin c',
              cos (2*c'+1),
              3*(sin (c'+1)) - cos (3*c' + 2)]

-- matrix / vector multiply
(*:) mat vec = getCol 1 (multStd mat (colVector vec))

-- vector addition
(=+=) va vb = V.zipWith (+) va vb

-- vector addition
(=-=) va vb = V.zipWith (-) va vb

fromVectors vs = fromLists (map V.toList vs)

mat_a = fromVectors (zipWith (V.++) (map as [0..])
                     (take n (map snd mat_a_gen)))

mat_a_gen = iterate f (0, x_res) where
  f (i, x_res') = (i+1, (w_in *: (as i)) =+= (w_res *: x_res'))

{-
-- not sure why this doesn't work
w_out =
  let m = mat_a <|> (fromVectors (map as [1..n]))
      Right m' = rref m
  in m' --submatrix 1 (n+1) (n+1) (n+k) m'
-}

solve m = backsub (triangular m)
 where
   downsub (u:us) (v:vs) = zipWith (-) (map ((v / u) *) us) vs

   triangular [] = []
   triangular rs | all (0 ==) (map head rs) = error "underdetermined"
   triangular ((0:r):rs) = triangular (rs ++ [0:r])
   triangular ((p:xs):rs) =
     let r = 1 : map (/ p) xs
         ss = map (downsub r) rs
     in r :  (triangular ss)

   backsubOne (u:us) (1:vs) = zipWith (-) us (map (u *) vs)
   backsub' us [] = us
   backsub' us (v:vs) = backsub' ((tail v) : map (flip backsubOne v) us) vs
   backsub rs = reverse (backsub' [] rs)

w_out = let m = mat_a <|> (fromVectors (map as [1..n]))
        in fromLists (solve (toLists m))

m = (transpose w_out) <-> (w_in <|> w_res)

guesses = map (V.take k) (iterate (m *:) ((as 0) V.++ x_res))

norm1 mat = maximum [V.sum (V.map (realPart . abs) (getCol c mat)) |
   c <- [1 .. ncols mat]]

err = norm1 (fromVectors ((zipWith (=-=) (map as [0..]) (take n guesses))))

someFunc :: IO ()
someFunc = do
   let a = fromList 3 4 [1,6,2,8,9,3,9,0,1,5,2,7]
   let Right ra = rref a
   print (getDiag ra)
   print (getCol 4 ra)