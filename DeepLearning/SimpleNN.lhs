Simple toy problem of training a neural net to compute some values of a non linear function.
For the example in question, it seems that the relu activation function does not perform well.
The logistic activation function yields good results.

Gradients are computed using the lovely automatic differentiation idea, see this [link](http://conal.net/blog/posts/what-is-automatic-differentiation-and-why-does-it-work) for details.
```haskell
module Main where

import System.Random
import Data.Matrix
import Data.List.Split
import Data.Dif
```

Some sample activation functions.

[Logistic](https://en.wikipedia.org/wiki/Logistic_function)

```haskell
logistic :: Floating a => a -> a
logistic = recip (1+(exp negate))
```

Some others

```haskell
trig :: Floating a => a -> a
trig = (1+tanh) / 2

linear :: Num a => a -> a
linear = id
```

[Rectified Linear](https://en.wikipedia.org/wiki/Rectifier_(neural_networks))

```haskell
relu :: Fractional a => a -> a
relu = (1+signum) * linear / 2
```

The function we are trying to model. This was originally the XOR function, but that was too easy. Hence I am using
something more interesting. Takes two inputs.

```haskell
target :: Num a => a -> a -> a
target x y = 3*x + y - 2*x*y
```

The Neural Net function. Takes as input the parameters, two input values, and returns a single number as output. The architecture here has a single hidden layer with non-linear activation functions.

```haskell
-- the number of hidden layer units we are using
neuralNetWidth :: Int
neuralNetWidth = 3

-- the neural network function itself
neuralNet :: Floating t => [t] -> t -> t -> t
neuralNet params u v = (((transpose w1) * (fmap activation ((w0 * x) + c))) + b) ! (1,1)
 where
  -- activation function
  activation = logistic
  
  -- construct input vector
  x = fromList 2 1 [u,v]
  
  -- extract the various components
  [w0_param,c_param,w1_param,b_param] = splitPlaces [2*neuralNetWidth,neuralNetWidth,neuralNetWidth,1] params
  
  -- construct matrices and vectors
  w0 = fromList neuralNetWidth 2 w0_param
  c = fromList neuralNetWidth 1 c_param
  w1 = fromList neuralNetWidth 1 w1_param
  b = fromList 1 1 b_param
```

Error function. Mean squared. Input space is the set {0,1,2} ⊕ {0,1,2}

```haskell
neuralNet_Error :: Floating a => [a] -> a
neuralNet_Error params = sum [((neuralNet params u v) - (target u v))^2 / 2 | u <- [0,1,2], v <- [0,1,2]]
```

It's also possible to do parameter norm regularisation.
One could add the following into the error function above.

```haskell
l1_regularise amount params = sum (map (amount * abs) params)   -- L1 regularisation
l2_regularise amount params  = sum (map (amount * (^2)) params)   -- L2 regularisation
```

Error function gradient, computed using automatic differentiation.

```haskell
neuralNet_Error_Gradient :: Floating a => [a] -> [a]
neuralNet_Error_Gradient params = [dVal (deriv (neuralNet_Error (zipWith dId [0..] params)) i) | i <- [0..length params - 1]]
```

Adaptive step sizes. Parameter update step, using gradient of error function.

```haskell
neuralNet_update :: (Ord b, Floating b) => [b] -> [b]
neuralNet_update params = zipWith (+) params (map (stepSize *) searchDirection)
 where
  -- the gradient at current point in parameter space
  gradient = neuralNet_Error_Gradient params
  
  -- use line search with Armijo condition to find best step size
  searchDirection = map negate (normalise gradient)
  startingStepSize = 10
  stepSize = backtrackingLineSearch neuralNet_Error params gradient searchDirection startingStepSize

-- dot product
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

-- normalise vector to unit length
normalise :: Floating b => [b] -> [b]
normalise xs = map (/ (sqrt n)) xs
 where n = dot xs xs
```

[Backtracking line search](https://en.wikipedia.org/wiki/Backtracking_line_search)

```haskell
backtrackingLineSearch :: (Ord a, Fractional a) => ([a] -> a) -> [a] -> [a] -> [a] -> a -> a
backtrackingLineSearch f x g p a0 = head (filter (\a -> armijo a || (a < 1e-16)) (iterate (tau *) a0))
 where
  tau = 0.8
  c = 0.7
  fx = f x
  gp = dot g p
  armijo a = f x2 <= fx + c * a * gp
   where x2 = zipWith (+) x (map (a *) p)
```

Iterate the update step

```haskell
neuralNet_gradient_descent :: (Ord b, Floating b) => [b] -> [[b]]
neuralNet_gradient_descent startingParams = iterate neuralNet_update startingParams
```

The main function

```haskell
main = do
  -- generate some random numbers as our initial parameters
  let rs = map ((2*) - 1) (randoms (mkStdGen 3) :: [Double])
  
  -- run gradient descent
  let ps = neuralNet_gradient_descent (take (4*neuralNetWidth + 1) rs)
  
  -- obtain one of the iterations of gradient descent
  let p = (ps !! 500)
  
  -- print the new parameters
  print p
  
  -- print the new error
  print $ neuralNet_Error p
```

The output of running this program is the following

```
[1.6667342079054994,0.8947867969956372,2.3302804096523895,-2.044139547659836,-2.3819208329376815,-2.610919794668604,-3.8234167184285135,-2.67984447448601,2.4994021137931797,-4.2301307083010045,5.950207133404795,-3.2165182814618487,2.6809964732279123]
1.165182007063679e-8
```

As one can see, the mean squared error after training is almost zero.

This file is [Literate Haskell Markdown](https://github.com/sol/markdown-unlit). Use `ghci -pgmL markdown-unlit SimpleNN.lhs` to run.
