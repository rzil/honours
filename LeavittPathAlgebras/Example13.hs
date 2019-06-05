module Example13 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")), ("f",("v","w"))]) (M.fromList [("e",3),("f",1)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = buildGraphFromEdges [("a1",("v","u1")),("a2",("u1","u2")),("b",("v","w")),("c",("w","u3")),("d",("u3","u4")),("a3",("u2","u5"))]

testHomomorphism = WLPA.wLPA_relations_present phi weighted_example (convertGraphToWeighted unweighted_equivalent_example)
 where
  phi (WLPA.AVertex "u") = u1 + u2 + u3 + u4 + u5
  phi (WLPA.AVertex "v") = v
  phi (WLPA.AVertex "w") = w
  phi (WLPA.AEdge "e" 1) = a1
  phi (WLPA.AEdge "e" 2) = a1 * a2 + b * c
  phi (WLPA.AEdge "e" 3) = a1 * a2 * a3 + b * c * d
  phi (WLPA.AEdge "f" 1) = b
  phi (WLPA.AGhostEdge e w) = WLPA.adjoint (phi (WLPA.AEdge e w))
  phi _ = WLPA.Zero

atom c = WLPA.Atom c
vertex = atom 1 . WLPA.vertex

edge = atom 1 . (flip WLPA.edge 1)
ghostEdge = atom 1 . (flip WLPA.ghostEdge 1)
edge2 = atom 1 . (flip WLPA.edge 2)
ghostEdge2 = atom 1 . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

u1 = vertex "u1"
u2 = vertex "u2"
u3 = vertex "u3"
u4 = vertex "u4"
u5 = vertex "u5"

v = vertex "v"
w = vertex "w"

a1 = edge "a1"
a2 = edge "a2"
a3 = edge "a3"

b = edge "b"
c = edge "c"
d = edge "d"
