{-# LANGUAGE TypeOperators #-}
module Main where

import Prelude hiding (zipWith)

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Dif

import Data.Random.Normal

import qualified Data.Array.Accelerate.Interpreter as Interp
import qualified Data.Array.Accelerate.LLVM.Native as CPU
--import qualified Data.Array.Accelerate.LLVM.PTX    as PTX

p = 5000
q = 20

--
testInput :: Vector Double
testInput = fromList (Z:.p) (mkNormals' (0,1) 1)

testMatrix :: Matrix Double
testMatrix = fromList (Z:.q:.p) (mkNormals' (0,1) 2)

testVector :: Vector Double
testVector = fromList (Z:.q) (mkNormals' (0,1) 3)

--testActivation :: Activation Double
--testActivation = (id, const 1)

testLayer :: Acc (Vector Double) -> Dif
testLayer = dAffine_wrt_parameters (use testVector) (use testMatrix)

testNet :: Int -> Dif
--testNet i = (dCrossEntropy (use (oneHotEncoding i q)). dSoftmax . testLayer) (use testInput)
testNet i = (dCrossEntropyOneHotEncoding (constant i) . dSoftmax . testLayer) (use testInput)
--

--run = Interp.run
run = CPU.run
--run = PTX.run

main = sequence [print $ run $ dVal $ testNet i | i <- [0..q-1]] >> return ()
