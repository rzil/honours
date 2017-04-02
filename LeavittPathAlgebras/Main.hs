module Main where

import Graph
import GraphMonoid

f xs = length [1 | Nothing <- xs]

--main = print $ f $ map graphGroupSolve $ tournaments 7

ndags n = [dag | dag <- dags n, length (vertices dag) == n, weaklyConnected dag]

main = mapM (putStrLn . show . length . weakly_connected_n_graphs) [2..8]

-- weakly_connected_unsolvable_n_graphs
-- [2,9,93,2876]

-- https://oeis.org/search?q=2%2C13%2C199&language=english&go=Search
-- weakly_connected_n_graphs
-- [2,13,199]
