module Main where

import System.Random
import Data.List.Split
import Data.Complex.Generic
import Data.Dif

differentiable :: Num a => [Complex a] -> [Complex (Dif Int a)]
differentiable zs = zipWith (\z [i,j] -> (dId i (realPart z)) :+ (dId j (imagPart z))) zs (chunksOf 2 [1..])

gradient :: Num b => ([Complex (Dif Int b)] -> Dif Int a) -> [Complex b] -> [Complex (Dif Int a)]
gradient f params = [(deriv y i) :+ (deriv y j) | [i,j] <- chunksOf 2 [1 .. 2*(length params)]]
 where y = f (differentiable params)

neuralNetWidth :: Int
neuralNetWidth = 3

activation :: Floating a => a -> a
activation = exp

neuralNet :: Floating a => [a] -> a -> a
neuralNet params z = sum (zipWith (*) w1 (map activation (zipWith (+) (map (z *) w0) c))) + b
 where [w0,c,w1,[b]] = splitPlaces [neuralNetWidth,neuralNetWidth,neuralNetWidth,1] params

target :: Num a => Complex a -> Complex a
target z = (realPart z) :+ 0

norm :: Num a => Complex a -> a
norm z = (realPart z)^2 + (imagPart z)^2

neuralNet_Error :: (Ord a, Floating a) => [Complex a] -> a
neuralNet_Error params = sum [let z = fromIntegral z_ in (norm ((neuralNet params z) - (target z))) / 2 | z_ <- inputSpace]
 where inputSpace = [1..9]

neuralNet_Error_Gradient :: (Ord a, Floating a) => [Complex a] -> [Complex a]
neuralNet_Error_Gradient params = map (fmap dVal) (gradient neuralNet_Error params)

---

normDot :: Num a => [Complex a] -> [Complex a] -> a
normDot xs ys = sum (zipWith (*) (map realPart xs) (map realPart ys)) + sum (zipWith (*) (map imagPart xs) (map imagPart ys))

normalise :: (Ord a, Floating a) => [Complex a] -> [Complex a]
normalise xs = map (* (sqrt n)) xs
 where n = (recip (normDot xs xs)) :+ 0

neuralNet_update :: (Ord a, Floating a) => [Complex a] -> [Complex a]
neuralNet_update params = zipWith (+) params (map ((stepSize :+ 0) *) searchDirection)
 where
  -- the gradient at current point in parameter space
  grad = neuralNet_Error_Gradient params
  
  -- use line search with Armijo condition to find best step size
  searchDirection = map negate (normalise grad)
  startingStepSize = 10
  stepSize = backtrackingLineSearch neuralNet_Error params grad searchDirection startingStepSize

backtrackingLineSearch
  :: (Ord t, Floating t) =>
     ([Complex t] -> t)
     -> [Complex t] -> [Complex t] -> [Complex t] -> t -> t
backtrackingLineSearch f x g p a0 = head (filter (\a -> armijo a || (a < 1e-16)) (iterate (tau *) a0))
 where
  tau = 0.8
  c = 0.7
  fx = f x
  gp = normDot g p
  armijo a = f x2 <= fx + c * a * gp
   where x2 = zipWith (+) x (map ((a:+0) *) p)

------

neuralNet_gradient_descent :: (Ord a, Floating a) => [Complex a] -> [[Complex a]]
neuralNet_gradient_descent startingParams = iterate neuralNet_update startingParams

------

-- uniformly distributed points in unit circle
complexRandoms :: (Random a, Ord a, Floating a) => Int -> [Complex a]
complexRandoms seed = [let (a,b) = (min x0 x1,max x0 x1) in mkPolar b (2*pi*a/b) | [x0,x1] <- chunksOf 2 (randoms (mkStdGen seed))]

main = do
  -- generate some random numbers as our initial parameters
  let rs = complexRandoms 13 :: [Complex Double]
  
  -- run gradient descent
  let ps = neuralNet_gradient_descent (take (4*neuralNetWidth + 1) rs)
  
  -- obtain one of the iterations of gradient descent
  let p = (ps !! 1000)
  
  -- print the new parameters
  print p
  
  -- print the new error
  print $ neuralNet_Error p
