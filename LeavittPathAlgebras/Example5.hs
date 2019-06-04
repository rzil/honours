module Example5 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")), ("f",("u","w"))]) (M.fromList [("e",2),("f",1)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = (buildGraphFromEdges [("e1",("u","v")), ("e2",("u","v")), ("f",("w","u"))])

atom c = WLPA.Atom c
vertex = atom 1 . WLPA.vertex

edge = atom 1 . (flip WLPA.edge 1)
ghostEdge = atom 1 . (flip WLPA.ghostEdge 1)
edge2 = atom 1 . (flip WLPA.edge 2)
ghostEdge2 = atom 1 . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

u = vertex "u"
v = vertex "v"
w = vertex "w"
e1 = edge "e1"
e2 = edge "e2"
f = edge "f"

testHomomorphism = WLPA.wLPA_relations_present phi weighted_example (convertGraphToWeighted unweighted_equivalent_example)
 where
  phi (WLPA.AVertex "u") = u
  phi (WLPA.AVertex "v") = v
  phi (WLPA.AVertex "w") = w
  phi (WLPA.AEdge "e" 1) = s e1
  phi (WLPA.AEdge "e" 2) = s e2
  phi (WLPA.AEdge "f" 1) = s f
  phi (WLPA.AGhostEdge e w) = WLPA.adjoint (phi (WLPA.AEdge e w))
  phi _ = WLPA.Zero
