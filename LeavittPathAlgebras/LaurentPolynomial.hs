module LaurentPolynomial where

import Polynomial

data LaurentPolynomial a = LaurentPolynomial {p :: Polynomial a, d :: Int}  deriving Show

-- removes leading zeroes
simplify (LaurentPolynomial p d) = LaurentPolynomial (stripLeadingZeroes p) (d + zeroMultiplicity p)

instance Functor LaurentPolynomial where
  fmap f (LaurentPolynomial p d) = LaurentPolynomial (fmap f p) d

instance (Num a,Eq a) => Eq (LaurentPolynomial a) where
  p == q = eq (simplify p) (simplify q)

eq (LaurentPolynomial px dx) (LaurentPolynomial py dy) = px == py && dx == dy

instance (Eq a,Num a) => Num (LaurentPolynomial a) where
  x@(LaurentPolynomial _ dx) + y@(LaurentPolynomial _ dy) | dx < dy = y + x
  (LaurentPolynomial px dx) + (LaurentPolynomial py dy) = LaurentPolynomial (py + x^n * px) dy
   where
    n = dx - dy
    x = polynomial [0,1]
  negate = fmap negate
  (LaurentPolynomial px dx) * (LaurentPolynomial py dy) = LaurentPolynomial (px * py) (dx + dy)
  fromInteger n = LaurentPolynomial (fromInteger n) 0
  abs = undefined
  signum = undefined

test :: LaurentPolynomial Int
test = (LaurentPolynomial (constant 1) (-1)) * (LaurentPolynomial (constant 1) (1))

test2 :: LaurentPolynomial Int
test2 = (LaurentPolynomial (constant 2) (-1)) + (LaurentPolynomial (constant 1) (1))

test3 :: LaurentPolynomial Int
test3 = (LaurentPolynomial (constant 3) (-2)) + (LaurentPolynomial (constant 1) (1))
