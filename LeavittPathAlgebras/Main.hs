module Main where

import qualified WeightedLPA as WLPA
import Example9
import FiniteFields

main = putStrLn $ unlines $ take 30 $ map show $ WLPA.projections weighted_example [Z2_1]
