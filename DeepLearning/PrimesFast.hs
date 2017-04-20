module Main where

import Data.Random.Normal
import Data.List.Split
import Data.Numbers.Primes

import Data.NumInstances
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Dif

import qualified Data.Array.Accelerate.Interpreter as Interp
import qualified Data.Array.Accelerate.LLVM.Native as CPU
--import qualified Data.Array.Accelerate.LLVM.PTX    as PTX


--instance Real a => Real (Dif e a) where toRational = toRational . dVal

neuralNetInputs :: Int
neuralNetInputs = 2

neuralNetOutputs :: Int
neuralNetOutputs = 1

neuralNetWidth :: Int
neuralNetWidth = 4

neuralNetDepth :: Int
neuralNetDepth = 2

numberOfParameters :: Int
numberOfParameters = (neuralNetInputs + 1) * neuralNetWidth + (neuralNetWidth + 1) * neuralNetWidth * (neuralNetDepth - 1) + neuralNetOutputs * (neuralNetWidth + 1)

-- https://en.wikipedia.org/wiki/Logistic_function

logistic :: Floating a => a -> a
logistic = recip (1+(exp negate))

logisticDerivative :: Floating a => a -> a
logisticDerivative = logistic * (1 - logistic)

neuralNetLogistic :: [A.Acc (A.Vector Double)] -> [A.Acc (Matrix Double)] -> A.Acc (A.Vector Double) -> Dif
neuralNetLogistic bs as x = neuralNet bs as (logistic,logisticDerivative) x

target u v = (u + 2*v) - 0.5

neuralNetError :: [A.Acc (A.Vector Double)] -> [A.Acc (Matrix Double)] -> (A.Exp Double, A.Acc (A.Vector Double))
neuralNetError bs as = (y A.! (A.lift (A.Z A.:. (0::Int))), A.slice y' (A.lift (A.Z A.:. (0::Int) A.:. A.All)))
 where
  input u v = A.fromList (A.Z A.:. (2::Int)) [u,v]
  D y y' = foldl1 difAdd [(dSquareError (A.constant (target u v)) . (neuralNetLogistic bs as)) (A.use (input u v)) | u <- [0::Double,1,2,3], v <- [0::Double,1,2,3]]

dot :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double) -> A.Exp Double
dot xs ys = A.the (A.sum (A.zipWith (*) xs ys))

normalise :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double)
normalise xs = A.map (/ (sqrt n)) xs
 where n = dot xs xs

bsas xs = ([b2,b1,b0],[a2,a1,a0])
 where
  a2_start = 0
  a2_size = neuralNetOutputs * neuralNetWidth
  b2_start = a2_start + a2_size
  b2_size = neuralNetOutputs
  
  a1_start = b2_start + b2_size
  a1_size = neuralNetWidth * neuralNetWidth
  b1_start = a1_start + a1_size
  b1_size = neuralNetWidth
  
  a0_start = b1_start + b1_size
  a0_size = neuralNetInputs * neuralNetWidth
  b0_start = a0_start + a0_size
  b0_size = neuralNetWidth
  
  a0 = A.reshape (A.lift (A.Z A.:. neuralNetWidth A.:. neuralNetInputs)) (A.slit (A.constant a0_start) (A.constant a0_size) xs) :: A.Acc (Matrix Double)
  b0 = (A.slit (A.constant b0_start) (A.constant b0_size) xs) :: A.Acc (A.Vector Double)
  a1 = A.reshape (A.lift (A.Z A.:. neuralNetWidth A.:. neuralNetWidth)) (A.slit (A.constant a1_start) (A.constant a1_size) xs) :: A.Acc (Matrix Double)
  b1 = (A.slit (A.constant b1_start) (A.constant b1_size) xs) :: A.Acc (A.Vector Double)
  a2 = A.reshape (A.lift (A.Z A.:. neuralNetOutputs A.:. neuralNetWidth)) (A.slit (A.constant a2_start) (A.constant a2_size) xs) :: A.Acc (Matrix Double)
  b2 = (A.slit (A.constant b2_start) (A.constant b2_size) xs) :: A.Acc (A.Vector Double)

neuralNet_update :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double)
neuralNet_update xs = A.zipWith (+) xs (A.map (stepSize *) searchDirection)
 where
  -- the gradient at current point in parameter space
  (err,grad) = (uncurry neuralNetError . bsas) xs
  
  -- use line search with Armijo condition to find best step size
  searchDirection = A.map negate (normalise grad)
--  startingStepSize = 10
  stepSize = 1e-2 --backtrackingLineSearch (fst . uncurry neuralNetError . bsas) xs grad searchDirection startingStepSize

backtrackingLineSearch
  :: (A.Acc (A.Vector Double) -> A.Exp Double)
     -> A.Acc (A.Vector Double)
     -> A.Acc (A.Vector Double)
     -> A.Acc (A.Vector Double)
     -> A.Exp Double
     -> A.Exp Double
backtrackingLineSearch f x g p a0 = head (filter (\a -> armijo a || (a < 1e-16)) (iterate (tau *) a0))
 where
  tau = A.constant (0.8 :: Double)
  c = A.constant (0.7 :: Double)
  fx = f x
  gp = dot g p
  armijo a = f x2 <= fx + c * a * gp
   where x2 = A.zipWith (+) x (A.map (a *) p)

neuralNetGradientDescent parameters = iterate neuralNet_update parameters

main = do
  -- generate some random numbers as our initial parameters
  let rs = mkNormals' (0,1) 123 :: [Double]
  
  let parameters = A.use (A.fromList (A.Z A.:. numberOfParameters) rs)
  
  -- run gradient descent
  let ps = neuralNetGradientDescent parameters
  
  -- obtain one of the iterations of gradient descent
  let p = (ps !! 1)
  
  -- print the new parameters
  print $ CPU.run p
  
  -- print the error
  let nne = (uncurry neuralNetError . bsas)
  print $ CPU.run $ A.unit $ fst $ nne p

{-}
neuralNet_update bs as = zipWith (+) params (map (stepSize *) searchDirection)
 where
  -- the gradient at current point in parameter space
  grad = neuralNetErrorGradient bs as
  
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
-}

{-}
neuralNet :: Floating t => [t] -> t -> t -> t
neuralNet params u v = (((transpose w2) * hiddenLayersOutput) + d) ! (1,1)
 where
  -- construct input vector
  input = fromList 2 1 [u,v]
  
  -- feed input through hidden layers
  applyHiddenLayer w c x = fmap activation ((w * x) + c)
  hiddenLayersOutput = foldr ($) input (zipWith applyHiddenLayer (w1s ++ [w0]) (bs ++ [c]))
  
  -- extract the various components
  [w0_param,c_param,w_params,b_params,w2_param,d_param] = splitPlaces [neuralNetInputs*neuralNetWidth,neuralNetWidth,(neuralNetDepth-1)*neuralNetWidth^2,(neuralNetDepth-1)*neuralNetWidth,neuralNetWidth,1] params
  
  -- construct matrices and vectors
  w0 = fromList neuralNetWidth neuralNetInputs w0_param
  c = fromList neuralNetWidth 1 c_param
  w1s = map (fromList neuralNetWidth neuralNetWidth) (chunksOf (neuralNetWidth^2) w_params)
  bs = map (fromList neuralNetWidth 1) (chunksOf neuralNetWidth b_params)
  w2 = fromList neuralNetWidth 1 w2_param
  d = fromList 1 1 d_param

myRound :: (Real a, Integral c) => a -> c
myRound = round . toRational

target :: (Real t, Num u) => t -> t -> u
target u v = let x = 4*u + v in if isPrime (myRound x) then 1 else 0

neuralNet_Error :: (Real a, Floating a) => [a] -> a
neuralNet_Error params = sum [let (u,v) = (fromIntegral a,fromIntegral b) in ((neuralNet params u v) - (target u v))^2 / 2 | (a,b) <- inputSpace]
 where inputSpace = [(u,v) | u <- [0..3], v <- [0..3]]

neuralNet_Error_Gradient :: (Real b, Floating b) => [b] -> [b]
neuralNet_Error_Gradient params = map dVal (gradient neuralNet_Error params)

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

normalise :: Floating b => [b] -> [b]
normalise xs = map (/ (sqrt n)) xs
 where n = dot xs xs

neuralNet_update :: (Real c, Floating c) => [c] -> [c]
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

neuralNet_gradient_descent :: (Real c, Floating c) => [c] -> [[c]]
neuralNet_gradient_descent startingParams = iterate neuralNet_update startingParams

main = do
  -- generate some random numbers as our initial parameters
  let rs = map ((2*) - 1) (randoms (mkStdGen 3) :: [Double])
  
  -- run gradient descent
  let ps = neuralNet_gradient_descent (take numberOfParameters rs)
  
  -- obtain one of the iterations of gradient descent
  let p = (ps !! 100000)
  
  -- print the new parameters
  print p
  
  -- print the new error
  print $ neuralNet_Error p
-}