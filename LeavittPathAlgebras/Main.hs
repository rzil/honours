module Main where

import qualified WeightedLPA as WLPA
import Example14

main = print $ map (checkIsZero weighted_graph_G) testHomomorphism
