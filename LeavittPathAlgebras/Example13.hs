module Example13 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("g",("x","u")),("e",("v","u")), ("f",("v","w"))]) (M.fromList [("e",3),("f",1),("g",1)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = buildGraphFromEdges [("k1",("x","u1")),("k2",("x","u2")),("k3",("x","u3")),("k4",("x","u4")),("k5",("x","u5")),("a1",("v","u1")),("a2",("u1","u2")),("b",("v","w")),("c",("w","u4")),("d",("u4","u5")),("a3",("u2","u3"))]

testHomomorphism = WLPA.wLPA_relations_present phi weighted_example (convertGraphToWeighted unweighted_equivalent_example)
 where
  phi (WLPA.AVertex "u") = u1 + u2 + u3 + u4 + u5
  phi (WLPA.AVertex "v") = v
  phi (WLPA.AVertex "w") = w
  phi (WLPA.AVertex "x") = x
  phi (WLPA.AEdge "e" 1) = a1
  phi (WLPA.AEdge "e" 2) = a1 * a2 + b * c
  phi (WLPA.AEdge "e" 3) = a1 * a2 * a3 + b * c * d
  phi (WLPA.AEdge "f" 1) = b
  phi (WLPA.AEdge "g" 1) = k1 + k2 + k3 + k4 + k5
  phi (WLPA.AGhostEdge e w) = WLPA.adjoint (phi (WLPA.AEdge e w))
  phi _ = WLPA.Zero

testHomomorphism2 = WLPA.wLPA_relations_present phi (convertGraphToWeighted unweighted_equivalent_example) weighted_example
 where
  u1 = WLPA.AVertex "u1"
  u2 = WLPA.AVertex "u2"
  u3 = WLPA.AVertex "u3"
  u4 = WLPA.AVertex "u4"
  u5 = WLPA.AVertex "u5"
  a1 = WLPA.AEdge "a1" 1
  a2 = WLPA.AEdge "a2" 1
  a3 = WLPA.AEdge "a3" 1
  c = WLPA.AEdge "c" 1
  d = WLPA.AEdge "d" 1
  
  phi (WLPA.AVertex "v") = vertex "v"
  phi (WLPA.AVertex "w") = vertex "w"
  phi (WLPA.AVertex "x") = vertex "x"
  phi (WLPA.AEdge "a1" 1) = edge "e"
  phi (WLPA.AEdge "b" 1) = edge "f"
  phi (WLPA.AVertex "u1") = s (edge "e") * edge "e"
  phi (WLPA.AEdge "a2" 1) = s (edge "e") * edge2 "e"
  phi (WLPA.AVertex "u2") = s (phi a2) * (phi a2)
  phi (WLPA.AEdge "a3" 1) = s (phi a2) * s (phi a1) * edge3 "e"
  phi (WLPA.AVertex "u3") = s (phi a3) * (phi a3)
  phi (WLPA.AEdge "c" 1) = s (edge "f") * edge2 "e"
  phi (WLPA.AEdge "d" 1) = s (phi c) * s (edge "f") * edge3 "e"
  phi (WLPA.AVertex "u4") = s (phi c) * (phi c)
  phi (WLPA.AVertex "u5") = s (phi d) * (phi d)
  phi (WLPA.AEdge "k1" 1) = edge "g" * phi u1
  phi (WLPA.AEdge "k2" 1) = edge "g" * phi u2
  phi (WLPA.AEdge "k3" 1) = edge "g" * phi u3
  phi (WLPA.AEdge "k4" 1) = edge "g" * phi u4
  phi (WLPA.AEdge "k5" 1) = edge "g" * phi u5
  phi (WLPA.AGhostEdge e w) = WLPA.adjoint (phi (WLPA.AEdge e w))
  phi _ = WLPA.Zero

u1 = vertex "u1"
u2 = vertex "u2"
u3 = vertex "u3"
u4 = vertex "u4"
u5 = vertex "u5"

x = vertex "x"
v = vertex "v"
w = vertex "w"

a1 = edge "a1"
a2 = edge "a2"
a3 = edge "a3"

k1 = edge "k1"
k2 = edge "k2"
k3 = edge "k3"
k4 = edge "k4"
k5 = edge "k5"

b = edge "b"
c = edge "c"
d = edge "d"

atom c = WLPA.Atom c
vertex = atom 1 . WLPA.vertex

edge = atom 1 . (flip WLPA.edge 1)
ghostEdge = atom 1 . (flip WLPA.ghostEdge 1)
edge2 = atom 1 . (flip WLPA.edge 2)
edge3 = atom 1 . (flip WLPA.edge 3)
ghostEdge2 = atom 1 . (flip WLPA.ghostEdge 2)
ghostEdge3 = atom 1 . (flip WLPA.ghostEdge 3)

s = WLPA.adjoint

