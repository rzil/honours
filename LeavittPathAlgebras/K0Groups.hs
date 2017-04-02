module K0Groups where

import Data.Ratio
import Matrix
import Graph
import GraphMonoid

-- is unimodular lifted to rational matrices
isUni :: Integral a => Matrix (Ratio a) -> Bool
isUni m = all ((1 ==) . denominator) (concat (content m)) && isUnimodular (fmap numerator m)

-- are the K0 groups of these graphs isomorphic?
-- Conjecture: LPA's are isomorphic iff the K0 groups are isomorphic
isK0Isomorphic :: (Ord w, Ord v) => Graph k v -> Graph j w -> Bool
isK0Isomorphic g h = maybe False isUni (Matrix.solve (graphGroupMatrix g) (graphGroupMatrix h))

-- finds examples of random graphs that have isomorphic K0 groups
-- n : number of vertices
-- p : edge probability
k0isos :: Num t => Int -> Float -> [(Int, Int, t)]
k0isos n p = [(a,b,determinant (graphGroupMatrix (randomGraph a n p))) | a <- [1..100], b <- [a+1..100], isK0Isomorphic (randomGraph a n p) (randomGraph b n p)]

{-
*GraphMonoid Data.Maybe> let g = randomGraph 7 3 0.4
*GraphMonoid Data.Maybe> let h = randomGraph 30 3 0.4
*GraphMonoid Data.Maybe> let Just m = Matrix.solve (graphGroupMatrix g) (graphGroupMatrix h :: Matrix Rational)
*GraphMonoid Data.Maybe> m
1 % 1		0 % 1		0 % 1
0 % 1		1 % 1		0 % 1
(-1) % 1		0 % 1		1 % 1

*GraphMonoid Data.Maybe> inverse m
Just 1 % 1		0 % 1		0 % 1
0 % 1		1 % 1		0 % 1
1 % 1		0 % 1		1 % 1

Since m is unimodular, the K_0 groups for the LPA's of g and h and isomorphic
Question: are the LPA's for g and h isomorphic?
-}
