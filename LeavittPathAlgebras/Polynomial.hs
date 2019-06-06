module Polynomial (
   Polynomial,
   polynomial,
   degree,
   zeroMultiplicity,
   stripLeadingZeroes,
   reversePolynomial,
   shift,
   isZero,
   constant,
   basis,
   toMonic,
   toPrimitive,
   leadingCoefficient,
   polyQuotRem,
   derivative,
   sturmChain,
   polyGCD,
   polyQuot,
   polyRem,
   signAtInfinity,
   squarefreeFactorise,
   squarefreePart,
   countRealRoots,
   countPositiveRealRoots,
   countNegativeRealRoots,
   realRootsBetween) where

import Data.Ratio

newtype Polynomial a = Polynomial {content :: [a]}  deriving Show
data ExtendedNum a = ExtendedNum a | PositiveInfinity | NegativeInfinity  deriving Show

polynomial :: [a] -> Polynomial a
polynomial xs = Polynomial xs

stripTrailingZeroes :: (Num a, Eq a) => Polynomial a -> Polynomial a
stripTrailingZeroes (Polynomial xs) = Polynomial (reverse (dropWhile (0 ==) (reverse xs)))

stripLeadingZeroes :: (Num a, Eq a) => Polynomial a -> Polynomial a
stripLeadingZeroes (Polynomial xs) = Polynomial (dropWhile (0 ==) xs)

--reversePolynomial :: (Eq a, Num a) => Polynomial a -> (Polynomial a, Int)
reversePolynomial isZero (Polynomial xs) = (Polynomial (reverse cs),length zs)
  where (zs,cs) = break (not . isZero) xs

zeroMultiplicity :: (Eq a, Num a) => Polynomial a -> Int
zeroMultiplicity (Polynomial xs) = length (takeWhile (0 ==) xs)

degree :: (Num a, Eq a) => Polynomial a -> Int
degree p = length (content (stripTrailingZeroes p)) - 1

isZero :: (Num a, Eq a) => Polynomial a -> Bool
isZero (Polynomial xs) = all (0 ==) xs

constant :: a -> Polynomial a
constant n = Polynomial [n]

shift :: Num a => Int -> Polynomial a -> Polynomial a
shift n (Polynomial xs) = Polynomial (take n (repeat 0) ++ xs)

basis :: Num a => [Polynomial a]
basis = [Polynomial (take k (repeat 0) ++ [1]) | k <- [0..]]

add :: Num c => [c] -> [c] -> [c]
add [] ys = ys
add xs [] = xs
add (x:xs) (y:ys) = x + y : add xs ys

mult :: Num t => [t] -> [t] -> [t]
mult [] _ = []
mult (x:xs) ys = add (mult xs (0 : ys)) (map (x *) ys)

toMonic :: Fractional a => Polynomial a -> Polynomial a
toMonic (Polynomial []) = Polynomial []
toMonic (Polynomial xs) = let x = last xs in Polynomial (map (/ x) xs)

toPrimitive :: Real a => Polynomial a -> Polynomial Integer
toPrimitive (Polynomial xs) = toPrimitive' (polynomial (map (toInt . (a *) . toRational) xs))
 where
  a = toRational (foldl lcm 1 (map (denominator . toRational) xs))
  toInt r = (numerator r) `div` (denominator r)

toPrimitive' p | isZero p = Polynomial []
toPrimitive' p@(Polynomial xs) = Polynomial (map (`div` x) xs)
 where x = signum (leadingCoefficient p) * abs (foldl1 gcd xs)

leadingCoefficient :: (Num a, Eq a) => Polynomial a -> a
leadingCoefficient p | isZero p = 0
leadingCoefficient p = let Polynomial xs = stripTrailingZeroes p in last xs

leadingTerm :: (Num a, Eq a) => Polynomial a -> Polynomial a
leadingTerm p | isZero p = 0
leadingTerm p = polynomial ((take ((degree p) - 1) (repeat 0)) ++ [leadingCoefficient p])

instance Functor Polynomial where
  fmap f (Polynomial xs) = Polynomial (map f xs)

instance (Num a,Eq a) => Eq (Polynomial a) where
  xs == ys =
    let Polynomial as = stripTrailingZeroes xs
        Polynomial bs = stripTrailingZeroes ys in as == bs

instance (Num a,Ord a,Eq a) => Ord (Polynomial a) where
  compare xs ys =
    let Polynomial as = stripTrailingZeroes xs
        Polynomial bs = stripTrailingZeroes ys in compare as bs

instance (Num a) => Num (Polynomial a) where
  (Polynomial p) + (Polynomial q) = Polynomial (add p q)
  negate = fmap negate
  (Polynomial p) * (Polynomial q) = Polynomial (mult p q)
  fromInteger n = constant (fromInteger n)
  abs = undefined
  signum = undefined

countRealRoots p = realRootsBetween p NegativeInfinity PositiveInfinity
countPositiveRealRoots p = realRootsBetween p (ExtendedNum 0) PositiveInfinity
countNegativeRealRoots p = realRootsBetween p NegativeInfinity (ExtendedNum 0)

-- | Counts real roots in the half-open interval (a,b]
-- | Uses Sturm's theorem. Polynomial p is assumed to be squarefree.
-- | Often seems to work for non-squarefree polynomials though.
-- | https://en.wikipedia.org/wiki/Sturm%27s_theorem#Applications
realRootsBetween :: (Fractional a, Eq a) => Polynomial a -> ExtendedNum a -> ExtendedNum a -> Int
realRootsBetween p a b = (gf a) - (gf b)
 where
  chain = sturmChain p
  f x = filter (0 /=) (map (flip evaluateExtendedSign x) chain)
  g signs = length (filter (True ==) (zipWith (/=) signs (tail signs)))
  gf = g . f

evaluateExtendedSign p (ExtendedNum x) = signum (evaluate p x)
evaluateExtendedSign p PositiveInfinity = signAtInfinity p True
evaluateExtendedSign p NegativeInfinity = signAtInfinity p False

-- | Evaluate sign of polynomial at positive or negative infinity
signAtInfinity :: (Num a, Eq a) => Polynomial a -> Bool -> a
signAtInfinity p True = signum (leadingCoefficient p)
signAtInfinity p False = signum (leadingCoefficient p) * (if even (degree p) then 1 else (-1))

evaluate :: Num a => Polynomial a -> a -> a
evaluate p x = sum (zipWith (*) (content p) (iterate (x *) 1))

polyGCD :: (Fractional a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
polyGCD a b
 | isZero b = a
 | otherwise = polyGCD b (polyRem a b)

sturmChain :: (Fractional a, Eq a) => Polynomial a -> [Polynomial a]
sturmChain p = p : sturmChain' p (derivative p)

sturmChain' _ q | isZero q = [q]
sturmChain' p q = q : sturmChain' q (negate (polyRem p q))

derivative :: (Num a, Eq a) => Polynomial a -> Polynomial a
derivative p = polynomial (tail (zipWith (*) (map fromIntegral [0..]) (content p)))

polyQuot n d = fst (polyQuotRem n d)
polyRem n d = snd (polyQuotRem n d)

-- | Polynomial long division
-- | https://en.wikipedia.org/wiki/Polynomial_long_division
polyQuotRem :: (Fractional a, Eq a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
polyQuotRem n d
 | isZero d = undefined
 | otherwise = polyQuotRem' d (0, n)

polyQuotRem' d (q,r)
 | isZero r = (q,r)
 | degree r < degree d = (q,r)
 | otherwise = let t = constant ((leadingCoefficient r) / (leadingCoefficient d)) * (basis !! ((degree r) - (degree d))) in polyQuotRem' d (q + t, r - (t * d))

p1 = (polynomial [1,-2,3])^2 * (polynomial [3,-1,2%1])^3
p2 = (polynomial [1,-2,5,3])^5 * (polynomial [3,-1,-1,2%1])^3
p3 = (polynomial [1,-2,5,3]) * (polynomial [3,-1,-1,2%1])

squarefreePart :: (Fractional a, Eq a) => Polynomial a -> Polynomial a
squarefreePart p = polyQuot p (polyGCD p (derivative p))

-- | This factorises the polynomial into squarefree factors, including multiplicity
-- | https://en.wikipedia.org/wiki/Square-free_polynomial
squarefreeFactorise :: (Fractional a, Eq a) => Polynomial a -> [(Polynomial a, Int)]
squarefreeFactorise p | isZero p = error "cannot factorise 0"
squarefreeFactorise p | p == constant 1 = []
squarefreeFactorise p = let (z,m) = sqf' p in (z,m) : squarefreeFactorise (polyQuot p (z^m))
 where
  sqf' p = (last qs,length qs)
   where
    f q = toMonic (polyGCD q (derivative q))
    qs = takeWhile ((constant 1) /=) (iterate f p)
