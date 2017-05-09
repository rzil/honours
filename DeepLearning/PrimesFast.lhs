This code tries to train a neural net using the [Haskell Accelerate library](https://github.com/AccelerateHS/accelerate/) for parallel computing.
The hope is to be able to run stuff on the GPU for a huge speedup. Unfortunately it's not working
properly right now. Instead of speeding it up, runing this on the GPU seems to slow it down. Not
sure why at the moment.

For this to work I wrote a [module](https://github.com/rzil/honours/blob/master/DeepLearning/Data/Array/Accelerate/Dif.hs) that tries to lift the automatic differentiation idea
to vector valued functions and then also make it use Accelerate arrays.

Start out with declaring module name and importing some other modules.

```haskell
module Main where

import Data.Random.Normal
import Data.List.Split

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Dif

-- choose which backend we want to use here by commenting out
-- the relevant modules.
import qualified Data.Array.Accelerate.LLVM.Native as Backend
--import qualified Data.Array.Accelerate.LLVM.PTX    as Backend
--import qualified Data.Array.Accelerate.Interpreter as Backend
```

Our architecture

```haskell
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
```

The [logistic function](https://en.wikipedia.org/wiki/Logistic_function)

```haskell
logistic :: Floating a => a -> a
logistic x = recip (1+ (exp $ negate x))

logisticDerivative :: Floating a => a -> a
logisticDerivative x = logistic x * (1 - (logistic x))
```

The neural network evaluation function

```haskell
neuralNetLogistic :: [A.Acc (A.Vector Double)] -> [A.Acc (Matrix Double)] -> A.Acc (A.Vector Double) -> Dif
neuralNetLogistic bs as x = neuralNet bs as (logistic,logisticDerivative) x
```

The neural network error and gradient. Also the target function

```haskell
neuralNetError :: [A.Acc (A.Vector Double)] -> [A.Acc (Matrix Double)] -> (A.Acc (A.Scalar Double), A.Acc (A.Vector Double))
neuralNetError bs as = (A.reshape (A.lift A.Z) y,A.reshape (A.lift (A.Z A.:. (A.constant numberOfParameters))) y')
 where
  input u v = A.fromList (A.Z A.:. (2::Int)) [u,v]
  D y y' = foldl1 difAdd [(dSquareError (A.constant (target u v)) . (neuralNetLogistic bs as)) (A.use (input u v)) | u <- [0::Double,1,2,3], v <- [0::Double,1,2,3]]

-- our target function
target u v = (u + 2*v) - 0.5
```

One step of gradient descent, updating the paramaters

```haskell
neuralNet_update :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double)
neuralNet_update xs = A.zipWith (+) xs (A.map (stepSize *) searchDirection)
 where
  -- the gradient at current point in parameter space
  (err,grad) = (uncurry neuralNetError . splitParameters) xs
  
  -- use line search with Armijo condition to find best step size
  searchDirection = A.map negate (normalise grad)
  stepSize = 1e-2   -- using a fixed step size for simplicity

dot :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double) -> A.Exp Double
dot xs ys = A.the (A.sum (A.zipWith (*) xs ys))

normalise :: A.Acc (A.Vector Double) -> A.Acc (A.Vector Double)
normalise xs = A.map (/ (sqrt n)) xs
 where n = dot xs xs
```

Gradient descent.

```haskell
neuralNetGradientDescent :: A.Vector Double -> [A.Vector Double]
neuralNetGradientDescent parameters = iterate (Backend.run1 neuralNet_update) parameters
```

Our main function

```haskell
main = do
  -- generate some random numbers as our initial parameters
  let rs = mkNormals' (0,1) 123 :: [Double]
  
  let parameters = A.fromList (A.Z A.:. numberOfParameters) rs
  
  -- run gradient descent
  let ps = neuralNetGradientDescent parameters
  
  -- neural network error function
  let nne = (uncurry neuralNetError . splitParameters)
  
  -- print the error at start
  print $ Backend.run $ fst $ nne $ A.use (ps !! 0)
  
  -- print the error after n steps of gradient descent
  print $ Backend.run $ fst $ nne $ A.use (ps !! 2000)
```

This splits up an array into sub-arrays for passing into our neural net. It's not the nicest looking
code I'll admit. It does the job though.

```haskell
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
```

All source code files [here](https://github.com/rzil/honours/tree/master/DeepLearning).

This file is [Literate Haskell Markdown](https://github.com/sol/markdown-unlit). Use
`ghc -pgmL markdown-unlit --make -O2 -threaded PrimesFast.lhs`
to compile.