module Main where

import Data.Random.Normal
import Data.List.Split

import Data.NumInstances
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Dif

--import qualified Data.Array.Accelerate.Interpreter as Backend
import qualified Data.Array.Accelerate.LLVM.Native as Backend
--import qualified Data.Array.Accelerate.LLVM.PTX    as Backend

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
neuralNetError bs as = (A.the (A.reshape (A.lift A.Z) y),A.reshape (A.lift (A.Z A.:. (A.constant numberOfParameters))) y')
 where
  input u v = A.fromList (A.Z A.:. (2::Int)) [u,v]
  D y y' = foldl1 difAdd [(dSquareError (A.constant (target u v)) . (neuralNetLogistic bs as)) (A.use (input u v)) | u <- [0::Double,1,2,3], v <- [0::Double,1,2,3]]

dot :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double) -> A.Exp Double
dot xs ys = A.the (A.sum (A.zipWith (*) xs ys))

normalise :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double)
normalise xs = A.map (/ (sqrt n)) xs
 where n = dot xs xs

splitParameters :: A.Acc (A.Vector Double) -> ([A.Acc (A.Vector Double)], [A.Acc (Matrix Double)])
splitParameters xs = ([b2,b1,b0],[a2,a1,a0])
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
  (err,grad) = (uncurry neuralNetError . splitParameters) xs
  
  -- use line search with Armijo condition to find best step size
  searchDirection = A.map negate (normalise grad)
--  startingStepSize = 10
  stepSize = 1e-2 --backtrackingLineSearch (fst . uncurry neuralNetError . splitParameters) xs grad searchDirection startingStepSize

{-}
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
-}

neuralNetGradientDescent parameters = iterate neuralNet_update parameters

main = do
  -- generate some random numbers as our initial parameters
  let rs = mkNormals' (0,1) 123 :: [Double]
  
  let parameters = A.use (A.fromList (A.Z A.:. numberOfParameters) rs)
  
  -- run gradient descent
  let ps = neuralNetGradientDescent parameters
  
  -- print the new parameters
--  print $ Backend.run (ps !! 1)
  
  -- neural network error function
  let nne = (uncurry neuralNetError . splitParameters)
  
  -- print the error at start
  print $ Backend.run $ A.unit $ fst $ nne (ps !! 0)
  
  -- print the error after one step of gradient descent
  print $ Backend.run $ A.unit $ fst $ nne (ps !! 1)
