module Example6 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")), ("f",("v","w"))]) (M.fromList [("e",2),("f",1)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = buildGraphFromEdges [("a1",("v","u1")),("a2",("u1","u2")),("b1",("v","w")),("c1",("w","u11"))]

--

vertex = WLPA.atom . WLPA.vertex
edge = WLPA.atom . (flip WLPA.edge 1)
edge2 = WLPA.atom . (flip WLPA.edge 2)
ghostEdge = WLPA.atom . (flip WLPA.ghostEdge 1)

u = vertex "u1" + vertex "u2" + vertex "u11"
v = vertex "v"
w = vertex "w"
e1 = edge "a1"
e2 = edge "a1" * edge "a2" + edge "b1" * edge "c1"
f1 = edge "b1"

-- inverse map
v_ = vertex "v"
w_ = vertex "w"
a1_ = edge "e"
b_ = edge "f"
c_ = (ghostEdge "f") * (edge2 "e")
a2_ = (ghostEdge "e") * (edge2 "e")
u1_ = (WLPA.adjoint a1_) * a1_
u2_ = (WLPA.adjoint a2_) * a2_
u11_ = (WLPA.adjoint c_) * c_

convertToBasisForm = WLPA.convertToBasisForm (convertGraphToWeighted unweighted_equivalent_example)
convertToBasisFormWeighted = WLPA.convertToBasisForm weighted_example

showMapping = putStrLn $ unlines (zipWith (\a b -> a ++ " --> " ++ b) (map show non_nods) (map show non_nods_reduced))
 where
  twos = map (WLPA.pathToNormalForm weighted_example) $ S.toList $ paths (doubleGraph (directedGraphAssociatedToWeightedGraph weighted_example)) 2
  non_nods = filter (not . WLPA.isNodPath weighted_example) twos
  non_nods_reduced = map (WLPA.convertToBasisForm weighted_example . WLPA.convertTerm) non_nods

showInverseMapping = putStrLn $ unlines (zipWith (\a b -> a ++ " --> " ++ b) (map show non_nods) (map show non_nods_reduced))
 where
  twos = map (WLPA.pathToNormalForm (convertGraphToWeighted unweighted_equivalent_example)) $ S.toList $ paths (doubleGraph (directedGraphAssociatedToWeightedGraph (convertGraphToWeighted unweighted_equivalent_example))) 2
  non_nods = filter (not . WLPA.isNodPath (convertGraphToWeighted unweighted_equivalent_example)) twos
  non_nods_reduced = map (WLPA.convertToBasisForm (convertGraphToWeighted unweighted_equivalent_example) . WLPA.convertTerm) non_nods
