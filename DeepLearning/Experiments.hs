module Main where

import System.Random
import Data.List.Split
import Data.Numbers.Primes
import Data.Matrix
import Data.Dif

instance Real a => Real (Dif e a) where toRational = toRational . dVal

differentiable :: Num a => [a] -> [Dif Int a]
differentiable zs = zipWith (\i z -> dId i z) [1..] zs

gradient :: Num a => ([Dif Int a] -> Dif Int b) -> [a] -> [Dif Int b]
gradient f params = [deriv y i | i <- [1 .. (length params)]]
 where y = f (differentiable params)

neuralNetInputs :: Int
neuralNetInputs = 2

neuralNetWidth :: Int
neuralNetWidth = 4

neuralNetDepth :: Int
neuralNetDepth = 2

numberOfParameters :: Int
numberOfParameters = (neuralNetDepth - 1)*neuralNetWidth^2 + neuralNetInputs*neuralNetWidth + neuralNetWidth + neuralNetDepth*neuralNetWidth + 1

logistic :: Floating a => a -> a
logistic = recip (1+(exp negate))

activation :: Floating a => a -> a
activation = logistic

neuralNet :: Floating t => [t] -> t -> t -> t
neuralNet params u v = (((transpose w2) * hiddenLayersOutput) + d) ! (1,1)
 where
  -- construct input vector
  input = fromList 2 1 [u,v]
  
  -- feed input through hidden layers
  applyHiddenLayer w c x = fmap activation ((w * x) + c)
  hiddenLayersOutput = foldr ($) input (zipWith applyHiddenLayer (w1s ++ [w0]) (bs ++ [c]))
  
  -- extract the various components
  [w0_param,c_param,w_params,b_params,w2_param,d_param] = splitPlaces [2*neuralNetWidth,neuralNetWidth,(neuralNetDepth-1)*neuralNetWidth^2,(neuralNetDepth-1)*neuralNetWidth,neuralNetWidth,1] params
  
  -- construct matrices and vectors
  w0 = fromList neuralNetWidth 2 w0_param
  c = fromList neuralNetWidth 1 c_param
  w1s = map (fromList neuralNetWidth neuralNetWidth) (chunksOf (neuralNetWidth^2) w_params)
  bs = map (fromList neuralNetWidth 1) (chunksOf neuralNetWidth b_params)
  w2 = fromList neuralNetWidth 1 w2_param
  d = fromList 1 1 d_param

myRound :: (Real a, Integral c) => a -> c
myRound = round . fromRational . toRational

target :: (Real t, Num u) => t -> t -> u
target u v = let x = 4*u + v in if isPrime (myRound x) then 1 else 0

neuralNet_Error :: (Real a, Floating a) => [a] -> a
neuralNet_Error params = sum [let (u,v) = (fromIntegral a,fromIntegral b) in ((neuralNet params u v) - (target u v))^2 / 2 | (a,b) <- inputSpace]
 where inputSpace = [(u,v) | u <- [0..3], v <- [0..3]]

neuralNet_Error_Gradient :: (Real b, Floating b) => [b] -> [b]
neuralNet_Error_Gradient params = map dVal (gradient neuralNet_Error params)
---

-- dot product
dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

-- normalise vector to unit length
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

------

neuralNet_gradient_descent :: (Real c, Floating c) => [c] -> [[c]]
neuralNet_gradient_descent startingParams = iterate neuralNet_update startingParams

------

{-
let q = [-3.9266224455492296,6.522198990484406,-1.0021246933683658,3.8451427947456995,-0.9003822275833925,1.7836227701224396,-2.33006520900074,4.61729730375516,-1.1321482199498678,1.1599908891933983,-0.5540785702949831,-7.262251276454074,2.665411292987106,1.9308042179271059,-4.384320415419504,2.290415591998457,-1.1187299052383344,-0.7187059184160428,2.6589958433429075,-3.822721251249673e-2,-0.5395574604556627,0.2291361205535051,1.993314095745433,-0.5960245780843668,-0.8765835293344421,2.2186488620235743,-2.8343205979339756,0.2879342744843964,-1.5591082002061667,0.5183946478980096,-0.23127480392491637,1.2908510702352363,5.317397010909992,-3.0972498477575963,-2.1627614042870467,2.99283714806138,-0.5223557755704695]
neuralNet_Error q --> 6.469984678236639e-3
-}

main = do
  -- generate some random numbers as our initial parameters
  let rs = map ((2*) - 1) (randoms (mkStdGen 3) :: [Double])
  
  -- run gradient descent
  let ps = neuralNet_gradient_descent (take numberOfParameters rs)
  
  -- obtain one of the iterations of gradient descent
  let p = (ps !! 100)
  
  -- print the new parameters
  print p
  
  -- print the new error
  print $ neuralNet_Error p
