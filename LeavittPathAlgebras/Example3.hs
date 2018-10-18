module Example3 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u"))]) (M.fromList [("e",3)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = (buildGraphFromEdges [("e1",("u","v")), ("e2",("u","v")), ("e3",("u","v"))])

vertex = WLPA.atom . WLPA.vertex
edge = WLPA.atom . (flip WLPA.edge 1)
ghostEdge = WLPA.atom . (flip WLPA.ghostEdge 1)

u = vertex "v"
v = vertex "u"
e1 = WLPA.adjoint $ edge "e1"
e2 = WLPA.adjoint $ edge "e2"
e3 = WLPA.adjoint $ edge "e3"

twos = map (WLPA.pathToNormalForm weighted_example) $ S.toList $ paths (doubleGraph (directedGraphAssociatedToWeightedGraph weighted_example)) 2

zeros = filter (WLPA.equal_wrt_graph weighted_example WLPA.Zero . WLPA.convertTerm) twos
non_zeros = filter (not . WLPA.equal_wrt_graph weighted_example WLPA.Zero . WLPA.convertTerm) twos

nods = filter (WLPA.isNodPath weighted_example) twos

non_nods = filter (not . WLPA.isNodPath weighted_example) twos

non_nods_reduced = map (WLPA.convertToBasisForm weighted_example . WLPA.convertTerm) non_nods

showMapping = putStrLn $ unlines (zipWith (\a b -> a ++ " --> " ++ b) (map show non_nods) (map show non_nods_reduced))
