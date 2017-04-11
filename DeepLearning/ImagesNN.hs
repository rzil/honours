module ImagesNN where

import Data.Random.Normal
import Data.List.Split
import Data.Matrix

import Data.Dif
import Data.MNIST

differentiable :: Num a => [a] -> [Dif Int a]
differentiable zs = zipWith (\i z -> dId i z) [1..] zs

gradient :: Num a => ([Dif Int a] -> Dif Int b) -> [a] -> [Dif Int b]
gradient f params = [deriv y i | i <- [1 .. (length params)]]
 where y = f (differentiable params)

numberOfClasses = 6
imageDataLength = 28 * 28

numberOfParameters = (imageDataLength+1)*numberOfClasses

-- https://en.wikipedia.org/wiki/Softmax_function
softmax :: Floating b => Matrix b -> Matrix b
softmax vec = fmap ((/ divisor) . exp) vec
 where divisor = sum (map exp (toList vec))

-- linear transformation followed by a softmax
linearModel :: Floating a => [a] -> [a] -> [a]
linearModel params imageData = toList (softmax (w * x + b))
 where
  -- construct input vector
  x = fromList imageDataLength 1 imageData
  
  -- extract the various components
  [w_param,b_param] = splitPlaces [imageDataLength*numberOfClasses,numberOfClasses] params
  
  -- construct matrices and vectors
  w = fromList numberOfClasses imageDataLength w_param
  b = fromList numberOfClasses 1 b_param

-- one hot encoding
-- label 0 goes to [1,0,0,0,0,0]
-- label 1 goes to [0,1,0,0,0,0]
-- and so on...
target :: Num b => Int -> [b]
target label = map (\idx -> if idx == label then 1 else 0) [0..]

crossEntropy :: Floating a => [a] -> [a] -> a
crossEntropy s l = negate (sum (zipWith (\si li -> li * (log si)) s l))

linearModel_Error :: Floating a => [(Int, [a])] -> [a] -> a
linearModel_Error trainingData params = (sum [crossEntropy (linearModel params imageData) (target label) | (label,imageData) <- trainingData]) / trainingDataSize
 where trainingDataSize = fromIntegral (length trainingData)

linearModel_Error_Gradient :: Floating b => [(Int, [b])] -> [b] -> [b]
linearModel_Error_Gradient trainingData params = map dVal (gradient (linearModel_Error constantTrainingData) params)
 where constantTrainingData = map (fmap (map dConst)) trainingData

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

normalise :: Floating b => [b] -> [b]
normalise xs = map (/ (sqrt n)) xs
 where n = dot xs xs

linearModel_update:: (Ord c, Floating c) => [(Int, [c])] -> [c] -> [c]
linearModel_update trainingData params = zipWith (+) params (map (stepSize *) searchDirection)
 where
  -- the gradient at current point in parameter space
  grad = linearModel_Error_Gradient trainingData params
  
  -- use line search with Armijo condition to find best step size
  searchDirection = map negate (normalise grad)
  startingStepSize = 10
  stepSize = backtrackingLineSearch (linearModel_Error trainingData) params grad searchDirection startingStepSize

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

linearModel_gradient_descent :: (Ord c, Floating c) => [(Int, [c])] -> [c] -> [[c]]
linearModel_gradient_descent trainingData startingParams = iterate (linearModel_update trainingData) startingParams

normaliseImageClassifierData :: (Integral a, Fractional u) => (Int, [a]) -> (Int, [u])
normaliseImageClassifierData (label, words) = (label, [((fromIntegral word) - 128) / 128 | word <- words])

main = do
  -- Download the necessary data and change to data directory
  downloadData "MNIST_Data"
  
  -- generate some normally distributed random numbers as our initial parameters
  let seed = 123
  let mean = 0
  let sd = 0.5
  let rs = mkNormals' (mean,sd) seed :: [Double]
  
  -- load training data
  (rows,columns,trainingData) <- readTrainingData
  
  putStrLn $ "Number of rows: " ++ (show rows)
  putStrLn $ "Number of columns: " ++ (show columns)
  
  -- normalise training data
  let normalisedTrainingData = map normaliseImageClassifierData (take 10 trainingData)
  
  -- run gradient descent
  let ps = linearModel_gradient_descent normalisedTrainingData (take numberOfParameters rs)
  
  print $ linearModel_Error normalisedTrainingData (take numberOfParameters rs)
  print $ linearModel_Error_Gradient normalisedTrainingData (take numberOfParameters rs)

--  print $ linearModel_Error normalisedTrainingData (ps !! 0)
--  print $ linearModel_Error normalisedTrainingData (ps !! 1)
