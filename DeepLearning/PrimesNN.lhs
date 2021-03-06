After my previous success training a very simple in Haskell, I have decided to move onto something slightly more complex,
though not by that much. This example is about training a neural net to recognise prime numbers. The main way in which this
differs from the last example is in that I'm using two hidden layers. Based on my experiments I found this worked better for
this problem.

Start out with declaring module name and importing some other modules.

```haskell
module Main where

import Data.List.Split
import Data.Numbers.Primes
import Data.Matrix as M
import Data.Dif

import Debug.Trace
import Data.Random.Normal
import Data.List as L (transpose)
import Graphics.EasyPlot
```

In this example I want to be able to round floating point numbers to integers. Normally this
would require an instance of the RealFrac class, but this is not really feasible for automatic
differentiation to work. Instead I'm using a trick which is to cast the number to a rational
number first. Hence we need an instance of the Real class.

```haskell
instance Real a => Real (Dif e a) where toRational = toRational . dVal
```

The next function takes a list of variables and makes them all differentiable.

```haskell
differentiable :: Num a => [a] -> [Dif Int a]
differentiable zs = zipWith (\i z -> dId i z) [1..] zs
```

And the next function computes the gradient of a differentiable function of several variables.

```haskell
gradient :: Num a => ([Dif Int a] -> Dif Int b) -> [a] -> [Dif Int b]
gradient f params = [deriv y i | i <- [1 .. (length params)]]
 where y = f (differentiable params)
```

Now for the architecture.

```haskell
neuralNetInputs :: Int
neuralNetInputs = 2

neuralNetWidth :: Int
neuralNetWidth = 4

neuralNetDepth :: Int
neuralNetDepth = 2

numberOfParameters :: Int
numberOfParameters = (neuralNetDepth - 1)*neuralNetWidth^2 + neuralNetInputs*neuralNetWidth + neuralNetWidth + neuralNetDepth*neuralNetWidth + 1
```

Here is the neural net itself. Everything is purely functional of course.

```haskell
logistic :: Floating a => a -> a
logistic = recip (1+(exp negate))

trig :: Floating a => a -> a
trig = (1+tanh) / 2

linear :: Num a => a -> a
linear = id

relu :: Fractional a => a -> a
relu = (1+signum) * linear / 2

activation :: Floating a => a -> a
activation = relu

neuralNet :: Floating t => [t] -> t -> t -> t
neuralNet params u v = (((M.transpose w2) * hiddenLayersOutput) + d) ! (1,1)
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
```

The target function. In this case we take two numbers as input, which we take to be the base-four digits of an integer.
We then check if this integer is prime and output 1 or 0, for true or false. This is where we need to round off the input,
before checking if it is a prime.

```haskell
myRound :: (Real a, Integral c) => a -> c
myRound = round . toRational

target :: (Real t, Num u) => t -> t -> u
target u v = let x = 4*u + v in if isPrime (myRound x) then 1 else 0
```

The neural net error function.

```haskell
neuralNet_Error :: (Real a, Floating a) => [a] -> a
neuralNet_Error params = sum [let (u,v) = (fromIntegral a,fromIntegral b) in ((neuralNet params u v) - (target u v))^2 / 2 | (a,b) <- inputSpace]
 where inputSpace = [(u,v) | u <- [0..3], v <- [0..3]]

l1_regularise amount params = sum (map (amount * abs) params)   -- L1 regularisation
l2_regularise amount params  = sum (map (amount * (^ 2)) params)   -- L2 regularisation
```

The gradient of the error function.

```haskell
neuralNet_Error_Gradient :: (Real b, Floating b) => [b] -> [b]
neuralNet_Error_Gradient params = map dVal (gradient neuralNet_Error params)
```

Update parameters using the gradient and line search.

```haskell
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
  startingStepSize = 16
  stepSize = backtrackingLineSearch neuralNet_Error params grad searchDirection startingStepSize

backtrackingLineSearch
  :: (Ord t, Fractional t) =>
     ([t] -> t) -> [t] -> [t] -> [t] -> t -> t
backtrackingLineSearch f x g p a0 = head (filter (\a -> armijo a || (a < 1e-9)) (iterate (tau *) a0))
 where
  tau = 0.8 -- between 0.5 to 0.8
  c = 0.1  -- between 0.001 to 0.1
  fx = f x
  gp = dot g p
  armijo a = f x2 <= fx + c * a * gp
   where x2 = zipWith (+) x (map (a *) p)
```

Gradient descent algorithm for solving the optimisation problem at hand.

```haskell
neuralNet_gradient_descent :: (Real c, Floating c) => [c] -> [[c]]
neuralNet_gradient_descent startingParams = iterate neuralNet_update startingParams
```

Finally our main function.

```haskell
main = do
  -- generate some random numbers as our initial parameters
  let rs = mkNormals' (0,1) 12412312 :: [Double]
  
  -- run gradient descent
  let ps = neuralNet_gradient_descent (take numberOfParameters rs)
  
  let params = take 10000 ps
  let errors = zipWith (\i x -> traceShow (i,x) x) [0..] (map neuralNet_Error params)
  writeFile "error.txt" (unlines (map show errors))
  plot (PNG "error.png") (Data2D [Style Lines, Title "Error"] [] (drop 10 $ (zip [0..] errors)))
  sequence_ [plot (PNG $ "param" ++ show i ++ ".png") (Data2D [Style Lines, Title (show i)] [] (zip [0..] qs)) | (i,qs) <- zip [0..] (L.transpose params)]
  print (neuralNet_Error $ head params)
  print (neuralNet_Error $ last params)
  
  -- obtain one of the iterations of gradient descent
--  let p = (ps !! 100000)
  
  -- print the new parameters
--  print p
  
  -- print the new error
--  print $ neuralNet_Error p
```

After training for around 100,000 iterations one gets some such as the following. On my MacBook Pro this takes around one hour to complete after I compile using the -O2 optimisation flag.

```
[-3.9266224455492296,6.522198990484406,-1.0021246933683658,3.8451427947456995,-0.9003822275833925,1.7836227701224396,-2.33006520900074,4.61729730375516,-1.1321482199498678,1.1599908891933983,-0.5540785702949831,-7.262251276454074,2.665411292987106,1.9308042179271059,-4.384320415419504,2.290415591998457,-1.1187299052383344,-0.7187059184160428,2.6589958433429075,-3.822721251249673e-2,-0.5395574604556627,0.2291361205535051,1.993314095745433,-0.5960245780843668,-0.8765835293344421,2.2186488620235743,-2.8343205979339756,0.2879342744843964,-1.5591082002061667,0.5183946478980096,-0.23127480392491637,1.2908510702352363,5.317397010909992,-3.0972498477575963,-2.1627614042870467,2.99283714806138,-0.5223557755704695]
6.469984678236639e-3
```

As one can see the error is in the order of `1e-2` which is quite acceptable.

All source code files [here](https://github.com/rzil/honours/tree/master/DeepLearning).

This file is [Literate Haskell Markdown](https://github.com/sol/markdown-unlit). Use
`ghc -pgmL markdown-unlit --make -O2 PrimesNN.lhs`
to compile.
