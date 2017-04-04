{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.Dif
-- Copyright   :  (c) Conal Elliott 2008
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Automatic differentiation, as in Jerzy Karczmarczuk's paper /Functional
-- Differentiation of Computer Programs/ (ICFP version),
-- <http://citeseer.ist.psu.edu/karczmarczuk98functional.html>.
-- 
-- See also the blog post
-- <http://conal.net/blog/posts/beautiful-differentiation/>.
----------------------------------------------------------------------

--
-- Modified by Ruben Zilibowitz 31st March 2017
-- To deal with derivatives w.r.t. more than one variable
--

module Data.Dif (Dif(..), dId, dConst) where

import Data.Function (on)
import Data.NumInstances ()

import Data.AdditiveGroup
import Data.VectorSpace

-- | Tower of derivatives.
data Dif e a = D { dVal :: a, deriv :: e -> Dif e a }

-- | Differentiable identity function (sampled).  Sometimes called "the
-- derivation variable" or similar, but it's not really a variable.
dId :: (Num a, Eq e) => e -> a -> Dif e a
dId name x = D x (\i -> if i == name then 1 else 0)

-- The papers refer to dId as a "derivation variable" or similar.  I like
-- to reserve "variable" for a name (if syntax) or storage (if
-- semantics).  @dId x@ is the derivative tower associated with the
-- identity function sampled at x.

-- | Differentiable constant function.  See also 'dConstV'.
dConst :: Num a => a -> Dif e a
dConst x = D x (const 0)

-- Ruben added the read instance 4/4/17
instance (Num a, Read a) => Read (Dif e a) where readsPrec d r = [(dConst a, b) | (a,b) <- readsPrec d r]

-- I'm not sure about the next three, which discard information
instance Show a => Show (Dif e a) where show    = show     .   dVal 
instance Eq   a => Eq   (Dif e a) where (==)    = (==)    `on` dVal
instance Ord  a => Ord  (Dif e a) where compare = compare `on` dVal

-- Later generalize derivatives to non-scalar vector spaces
-- | Like dConst' but assuming 'VectorSpace' instead of 'Num'.
dConstV :: AdditiveGroup s => s -> Dif e s
dConstV v = D v zeroV

instance AdditiveGroup s => AdditiveGroup (Dif e s) where
  zeroV = dConstV zeroV
  D x x' ^+^ D y y' = D (x ^+^ y) (x' ^+^ y')
  negateV (D x x') = D (negateV x) (\i -> negateV (x' i))

instance VectorSpace s => VectorSpace (Dif e s) where
  type Scalar (Dif e v) = Scalar v
  s *^ D x x' = D (s*^x) (\i -> s*^(x' i))

-- The chain rule
infix 0 >-<
(>-<) :: (Num a) => (a -> a) -> (Dif e a -> Dif e a) -> (Dif e a -> Dif e a)
f >-< d = \ p@(D u u') -> D (f u) (\i -> d p * (u' i))

instance Num a => Num (Dif e a) where
  fromInteger             = dConst . fromInteger
  D x x' + D y y'         = D (x + y) (\i -> (x' i) + (y' i))
  D x x' - D y y'         = D (x - y) (\i -> (x' i) - (y' i))
  p@(D x x') * q@(D y y') = D (x * y) (\i -> (x' i) * q + p * (y' i))

  negate = negate >-< -1
  abs    = abs    >-< signum
  signum = signum >-< 0

-- More efficiently:
--   signum (D x _)          = dConst (signum x)

-- Though really, signum isn't differentiable at zero, without something
-- like Dirac impulses.

instance Fractional a => Fractional (Dif e a) where
  fromRational = dConst . fromRational
  recip        = recip >-< - sqr recip

-- More efficiently:
--   recip (D x x') = ip
--     where ip = D (recip x) (-x' * ip * ip)

sqr :: Num a => a -> a
sqr x = x*x

instance (Fractional a, Floating a) => Floating (Dif e a) where
  pi    = dConst pi
  exp   = exp   >-< exp
  log   = log   >-< recip
  sqrt  = sqrt  >-< recip (2 * sqrt)
  sin   = sin   >-< cos
  cos   = cos   >-< - sin
  sinh  = sinh  >-< cosh
  cosh  = cosh  >-< sinh
  asin  = asin  >-< recip (sqrt (1-sqr))
  acos  = acos  >-< recip (- sqrt (1-sqr))
  atan  = atan  >-< recip (1+sqr)
  asinh = asinh >-< recip (sqrt (1+sqr))
  acosh = acosh >-< recip (- sqrt (sqr-1))
  atanh = atanh >-< recip (1-sqr)

-- More efficiently:
--   exp (D x x') = r where r = D (exp x) (x' * r)

