module Main where

import System.Random
import Data.List.Split
import Data.Numbers.Primes
import Data.Matrix
import Data.Dif

differentiable :: Num a => [a] -> [Dif Int a]
differentiable zs = zipWith (\i z -> dId i z) [1..] zs

gradient :: Num a => ([Dif Int a] -> Dif Int b) -> [a] -> [Dif Int b]
gradient f params = [deriv y i | i <- [1 .. (length params)]]
 where y = f (differentiable params)

neuralNetInputs :: Int
neuralNetInputs = 2

neuralNetWidth :: Int
neuralNetWidth = 3

neuralNetDepth :: Int
neuralNetDepth = 2

numberOfParameters :: Int
numberOfParameters = (neuralNetDepth - 1)*neuralNetWidth^2 + neuralNetInputs*neuralNetWidth + neuralNetWidth + neuralNetDepth*neuralNetWidth + 1

logistic :: Floating a => a -> a
logistic = recip (1+(exp negate))

activation :: Floating a => a -> a
activation = logistic

neuralNet :: Floating t => [t] -> t -> t -> t
neuralNet params u v = (((transpose w2) * (fmap activation (w1 * (fmap activation ((w0 * x) + c)) + b))) + d) ! (1,1)
 where
  -- construct input vector
  x = fromList 2 1 [u,v]
  
  -- extract the various components
  [w0_param,c_param,w1_param,b_param,w2_param,d_param] = splitPlaces [2*neuralNetWidth,neuralNetWidth,neuralNetWidth^2,neuralNetWidth,neuralNetWidth,1] params
  
  -- construct matrices and vectors
  w0 = fromList neuralNetWidth 2 w0_param
  c = fromList neuralNetWidth 1 c_param
  w1 = fromList neuralNetWidth neuralNetWidth w1_param
  b = fromList neuralNetWidth 1 b_param
  w2 = fromList neuralNetWidth 1 w2_param
  d = fromList 1 1 d_param

{-
neuralNet :: Floating a => [a] -> a -> a -> a
neuralNet params u v = sum (zipWith (*) (zipWith3 (\x y z -> x + y + z) (zipWith (*) w00 (repeat u)) (zipWith (*) w01 (repeat v)) c) w1) + b
 where
  [w00,w01,c,w1,[b]] = splitPlaces [neuralNetWidth,neuralNetWidth,neuralNetWidth,neuralNetWidth,1] params
-}

{-
neuralNet :: Floating a => [a] -> a -> a
neuralNet params x = sum (zipWith (*) w2 (applyLayer w1 c1 (applyLayer w0 c0 (repeat x)))) + c2
 where
  [w0,c0,w1,c1,w2,[c2]] = splitPlaces [neuralNetWidth,neuralNetWidth,neuralNetWidth,neuralNetWidth,neuralNetWidth,1] params
  applyLayer w c xs = map activation (zipWith (+) (zipWith (*) w xs) c)
-}

myFloor :: (Show b, Read a) => b -> a
myFloor x = read (head (splitOn "." (show x)))

myRound :: (Show b, Read a, Fractional b) => b -> a
myRound x = myFloor (x+0.5)

target :: (Floating a, Read a, Show a) => a -> a -> a
target u v = let x = 3*u + v in if isPrime (myRound x) then 1 else 0 --  3*u + v - 2*u*v

neuralNet_Error :: (Floating a, Read a, Show a) => [a] -> a
neuralNet_Error params = sum [let (u,v) = (fromIntegral a,fromIntegral b) in ((neuralNet params u v) - (target u v))^2 / 2 | (a,b) <- inputSpace]
 where inputSpace = [(u,v) | u <- [0..2], v <- [0..2]]

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
  let rs = [-5.997964617436484,6.8366753807468905,1.9946181529853286,-2.0013185819246995,-3.072544454848253,3.1034310008887287,-5.979828742108006,-3.1902084692473425,-1.4136501174627154,1.2927066753777938,-0.7099211453394163,-2.0648259100507986,4.545574047629832,-1.210595301923914,-4.532966851592963,0.9353161955234404,-0.9619315229579541,-2.0069557079385962,-0.23942932061733338,0.2858828985321634,-0.17420719349960065,1.8978890423090384,5.705283980931233,1.7484994317561444,-3.2301006289511487]
  
  -- run gradient descent
  let ps = neuralNet_gradient_descent (take numberOfParameters rs)
  
  -- obtain one of the iterations of gradient descent
  let p = (ps !! 1000)
  
  -- print the new parameters
  print p
  
  -- print the new error
  print $ neuralNet_Error p
