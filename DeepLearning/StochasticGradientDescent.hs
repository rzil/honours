
module Main where

import Data.List.Split
import Data.Numbers.Primes
import Data.Matrix as M
import Data.Dif

import Debug.Trace
import Data.Random.Normal
import Data.List as L (transpose)
import Graphics.EasyPlot

differentiable :: Num a => [a] -> [Dif Int a]
differentiable zs = zipWith (\i z -> dId i z) [1..] zs

gradient :: Num a => ([Dif Int a] -> Dif Int b) -> [a] -> [Dif Int b]
gradient f params = [deriv y i | i <- [1 .. (length params)]]
 where y = f (differentiable params)

neuralNetInputs :: Int
neuralNetInputs = 2

neuralNetWidth :: Int
neuralNetWidth = 5

neuralNetDepth :: Int
neuralNetDepth = 2

numberOfParameters :: Int
numberOfParameters = (neuralNetDepth - 1)*neuralNetWidth^2 + neuralNetInputs*neuralNetWidth + neuralNetWidth + neuralNetDepth*neuralNetWidth + 1

logistic :: Floating a => a -> a
logistic = recip (1+(exp negate))
{-285918.05972447083
174136.57216625233-}

trig :: Floating a => a -> a
trig = (1+tanh) / 2

linear :: Num a => a -> a
linear = id

relu :: Fractional a => a -> a
relu = (1+signum) * linear / 2

activation :: Floating a => a -> a
activation = logistic

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

target u v = u*v + 3*u + v --(sin (pi*(u^2 + v))) + (cos (pi*u))

neuralNet_Error :: Floating a => [(a,a)] -> [a] -> a
neuralNet_Error batch params = sum [((neuralNet params u v) - (target u v))^2 / 2 | (u,v) <- batch]

l1_regularise amount params = sum (map (amount * abs) params)   -- L1 regularisation
l2_regularise amount params  = sum (map (amount * (^ 2)) params)   -- L2 regularisation

neuralNet_Error_Gradient :: Floating b => [(b,b)] -> [b] -> [b]
neuralNet_Error_Gradient batch params = map dVal (gradient (\xs -> {-l2_regularise 0.01 xs + -}neuralNet_Error [(dConst u, dConst v) | (u,v) <- batch] xs) params)

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

normalise :: Floating b => [b] -> [b]
normalise xs = map (/ (sqrt n)) xs
 where n = dot xs xs

neuralNet_update :: (Ord c, Floating c) => [(c,c)] -> [c] -> [c]
neuralNet_update batch params = zipWith (+) params (map (stepSize *) searchDirection)
 where
  -- the gradient at current point in parameter space
  grad = neuralNet_Error_Gradient batch params
  
  -- use line search with Armijo condition to find best step size
  searchDirection = map negate (normalise grad)
  startingStepSize = 16
  stepSize = backtrackingLineSearch (neuralNet_Error batch) params grad searchDirection startingStepSize

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

neuralNet_gradient_descent :: (Ord c, Floating c) => [[(c,c)]] -> [c] -> [[c]]
neuralNet_gradient_descent (batch:batches) params = params : neuralNet_gradient_descent batches (neuralNet_update batch params)

makeBatches seed size mu sd = map pairs (chunksOf (size * 2) (mkNormals' (mu,sd) seed))
 where
  pairs [] = []
  pairs (x:y:xs) = (x,y) : pairs xs

main = do
  let parameterSeed = 1523
  let batchSeed = 15
  let batchSize = 150
  
  -- generate some random numbers as our initial parameters
  let rs = mkNormals' (0,1) parameterSeed :: [Double]
  
  -- run gradient descent
  let batches = (makeBatches batchSeed batchSize 0 8) --repeat (head (makeBatches batchSeed batchSize 0 8))
  let ps = neuralNet_gradient_descent batches (take numberOfParameters rs)
  
  let params = take 4000 ps
  let errors = zipWith (\i x -> traceShow (i,x) x) [0..] (map (neuralNet_Error (head batches)) params)
  writeFile "error.txt" (unlines (map show errors))
  plot (PNG "error.png") (Data2D [Style Lines, Title "Error"] [] (drop 10 $ (zip [0..] errors)))
  sequence_ [plot (PNG $ "param" ++ show i ++ ".png") (Data2D [Style Lines, Title (show i)] [] (zip [0..] qs)) | (i,qs) <- zip [0..] (L.transpose params)]
  print ((neuralNet_Error (head batches)) $ head params)
  print ((neuralNet_Error (head batches)) $ last params)
  
  -- obtain one of the iterations of gradient descent
--  let p = (ps !! 100000)
  
  -- print the new parameters
--  print p
  
  -- print the new error
--  print $ neuralNet_Error p
