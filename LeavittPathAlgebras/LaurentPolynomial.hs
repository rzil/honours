module LaurentPolynomial where

import Polynomial

data LaurentPolynomial a = LaurentPolynomial {p :: Polynomial a, d :: Int}  deriving Show

-- removes leading zeroes
simplify (LaurentPolynomial p _) | p == 0 = LaurentPolynomial (fromInteger 0) 0
simplify (LaurentPolynomial p d) = LaurentPolynomial (stripLeadingZeroes p) (d + zeroMultiplicity p)

instance Functor LaurentPolynomial where
  fmap f (LaurentPolynomial p d) = LaurentPolynomial (fmap f p) d

instance (Num a,Eq a) => Eq (LaurentPolynomial a) where
  p == q = eq (simplify p) (simplify q)

eq (LaurentPolynomial px dx) (LaurentPolynomial py dy) = px == py && dx == dy

instance (Num a) => Num (LaurentPolynomial a) where
  x@(LaurentPolynomial _ dx) + y@(LaurentPolynomial _ dy) | dx < dy = y + x
  (LaurentPolynomial px dx) + (LaurentPolynomial py dy) = LaurentPolynomial (py + (shift n px)) dy
   where n = dx - dy
  negate = fmap negate
  (LaurentPolynomial px dx) * (LaurentPolynomial py dy) = LaurentPolynomial (px * py) (dx + dy)
  fromInteger n = LaurentPolynomial (fromInteger n) 0
  abs = undefined
  signum = undefined

-- substitute x^{-1} for x
--reverseLaurentPolynomial :: (Eq a, Num a) => LaurentPolynomial a -> LaurentPolynomial a
reverseLaurentPolynomial isZero (LaurentPolynomial p d) = LaurentPolynomial p' (- (d + e))
 where (p',e) = reversePolynomial isZero p

lptest :: LaurentPolynomial Int
lptest = (LaurentPolynomial (constant 1) (-1)) * (LaurentPolynomial (constant 1) (1))

lptest2 :: LaurentPolynomial Int
lptest2 = (LaurentPolynomial (constant 2) (-1)) + (LaurentPolynomial (constant 1) (1))

lptest3 :: LaurentPolynomial Int
lptest3 = (LaurentPolynomial (constant 3) (-2)) + (LaurentPolynomial (constant 1) (1))
