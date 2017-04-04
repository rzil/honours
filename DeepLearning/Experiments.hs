module Main where

import System.Random
import Data.List.Split
import Data.Dif
import Data.Numbers.Primes

differentiable :: Num a => [a] -> [Dif Int a]
differentiable zs = zipWith (\i z -> dId i z) [1..] zs

gradient :: Num a => ([Dif Int a] -> Dif Int b) -> [a] -> [Dif Int b]
gradient f params = [deriv y i | i <- [1 .. (length params)]]
 where y = f (differentiable params)

neuralNetWidth :: Int
neuralNetWidth = 5

activation :: Floating a => a -> a
activation = recip (1+(exp negate))

neuralNet :: Floating a => [a] -> a -> a
neuralNet params z = sum (zipWith (*) w1 (map activation (zipWith (+) (map (z *) w0) c))) + b
 where [w0,c,w1,[b]] = splitPlaces [neuralNetWidth,neuralNetWidth,neuralNetWidth,1] params

myFloor :: (Show b, Read a) => b -> a
myFloor x = read (head (splitOn "." (show x)))

myRound :: (Show b, Read a, Fractional b) => b -> a
myRound x = myFloor (x+0.5)

target :: (Floating a, Read a, Show a) => a -> a
target x = if isPrime (myRound x) then 1 else 0

neuralNet_Error :: (Floating a, Read a, Show a) => [a] -> a
neuralNet_Error params = sum [let z = fromIntegral z_ in ((neuralNet params z) - (target z))^2 / 2 | z_ <- inputSpace]
 where inputSpace = [1..11]

neuralNet_Error_Gradient :: (Floating a, Read a, Show a) => [a] -> [a]
neuralNet_Error_Gradient params = map dVal (gradient neuralNet_Error params)
---

-- dot product
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

-- normalise vector to unit length
normalise :: Floating b => [b] -> [b]
normalise xs = map (/ (sqrt n)) xs
 where n = dot xs xs

neuralNet_update :: (Ord c, Floating c, Read c, Show c) => [c] -> [c]
neuralNet_update params = zipWith (+) params (map (stepSize *) searchDirection)
 where
  -- the gradient at current point in parameter space
  grad = neuralNet_Error_Gradient params
  
  -- use line search with Armijo condition to find best step size
  searchDirection = map negate (normalise grad)
  startingStepSize = 10
  stepSize = backtrackingLineSearch neuralNet_Error params grad searchDirection startingStepSize

backtrackingLineSearch
  :: (Ord t, Fractional t) =>
     ([t] -> t) -> [t] -> [t] -> [t] -> t -> t
backtrackingLineSearch f x g p a0 = head (filter (\a -> armijo a || (a < 1e-16)) (iterate (tau *) a0))
 where
  tau = 0.8
  c = 0.7
  fx = f x
  gp = dot g p
  armijo a = f x2 <= fx + c * a * gp
   where x2 = zipWith (+) x (map (a *) p)

------

neuralNet_gradient_descent :: (Ord c, Floating c, Read c, Show c) => [c] -> [[c]]
neuralNet_gradient_descent startingParams = iterate neuralNet_update startingParams

------

main = do
  -- generate some random numbers as our initial parameters
  let rs = map ((2*) - 1) (randoms (mkStdGen 3) :: [Double])
  
  -- run gradient descent
  let ps = neuralNet_gradient_descent (take (4*neuralNetWidth + 1) rs)
  
  -- obtain one of the iterations of gradient descent
  let p = (ps !! 10000)
  
  -- print the new parameters
  print p
  
  -- print the new error
  print $ neuralNet_Error p
