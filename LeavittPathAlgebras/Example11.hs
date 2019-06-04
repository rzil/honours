module Example11 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_example0 :: WeightedGraph String String
weighted_example0 = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

weighted_example2 :: WeightedGraph String String
weighted_example2 = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","w")),("h",("u","w"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

weighted_example3 :: WeightedGraph String String
weighted_example3 = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","v")),("h",("u","v"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

weighted_example4 :: WeightedGraph String String
weighted_example4 = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","w")),("h",("u","w")),("i",("w","v")),("j",("w","v"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2),("i",1),("j",2)])

weighted_example5 :: WeightedGraph String String
weighted_example5 = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","w")),("h",("u","w")),("i",("w","x")),("j",("w","x")),("k",("x","v")),("l",("x","v"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2),("i",1),("j",2),("k",1),("l",2)])

atom = WLPA.Atom 1
vertex = atom . WLPA.vertex
edge = atom . (flip WLPA.edge 1)
ghostEdge = atom . (flip WLPA.ghostEdge 1)

edge2 = atom . (flip WLPA.edge 2)
ghostEdge2 = atom . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

v = vertex "v"
u = vertex "u"
w = vertex "w"
x = vertex "x"

e1 = edge "e"
f1 = edge "f"
f2 = edge2 "f"

g1 = edge "g"
h1 = edge "h"
h2 = edge2 "h"

i1 = edge "i"
j1 = edge "j"
j2 = edge2 "j"

k1 = edge "k"
l1 = edge "l"
l2 = edge2 "l"

testMonomorphism1 = WLPA.wLPA_relations_present f weighted_example weighted_example2
 where
  f (WLPA.AVertex "v") = v
  f (WLPA.AVertex "u") = u
  f (WLPA.AEdge "e" 1) = e1
  f (WLPA.AEdge "f" 1) = f1
  f (WLPA.AEdge "f" 2) = f2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testMonomorphism2 = WLPA.wLPA_relations_present f weighted_example weighted_example2
 where
  f (WLPA.AVertex "v") = u
  f (WLPA.AVertex "u") = w
  f (WLPA.AEdge "e" 1) = g1
  f (WLPA.AEdge "f" 1) = h1
  f (WLPA.AEdge "f" 2) = h2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testMonomorphism3 = WLPA.wLPA_relations_present f weighted_example weighted_example3
 where
  f (WLPA.AVertex "v") = v
  f (WLPA.AVertex "u") = u
  f (WLPA.AEdge "e" 1) = e1
  f (WLPA.AEdge "f" 1) = f1
  f (WLPA.AEdge "f" 2) = f2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testOriginalHomomorphism = WLPA.wLPA_relations_present f weighted_example0 weighted_example3
 where
  f (WLPA.AVertex "v") = v + u
  f (WLPA.AEdge "e" 1) = e1 + g1
  f (WLPA.AEdge "f" 1) = f1 + h1
  f (WLPA.AEdge "f" 2) = f2 + h2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testOriginalEpimorphism = WLPA.wLPA_relations_present f weighted_example3 weighted_example
 where
  f (WLPA.AEdge "e" 1) = e1
  f (WLPA.AEdge "f" 1) = f1
  f (WLPA.AEdge "f" 2) = f2
  f (WLPA.AVertex "u") = u
  f (WLPA.AVertex "v") = v
  f (WLPA.AEdge "g" 1) = s f2
  f (WLPA.AEdge "h" 1) = s f1
  f (WLPA.AEdge "h" 2) = s e1
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testOriginalHomomorphismComposition = WLPA.wLPA_relations_present f weighted_example0 weighted_example
 where
  f (WLPA.AVertex "v") = v + u
  f (WLPA.AEdge "e" 1) = e1 + s f2
  f (WLPA.AEdge "f" 1) = f1 + s f1
  f (WLPA.AEdge "f" 2) = f2 + s e1
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testAutomorphism = WLPA.wLPA_relations_present f weighted_example3 weighted_example3
 where
  f (WLPA.AVertex "v") = u
  f (WLPA.AVertex "u") = v
  f (WLPA.AEdge "e" 1) = g1
  f (WLPA.AEdge "f" 1) = h1
  f (WLPA.AEdge "f" 2) = h2
  f (WLPA.AEdge "g" 1) = e1
  f (WLPA.AEdge "h" 1) = f1
  f (WLPA.AEdge "h" 2) = f2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero


testNewHomomorphism = WLPA.wLPA_relations_present f weighted_example0 weighted_example4
 where
  f (WLPA.AVertex "v") = v + u + w
  f (WLPA.AEdge "e" 1) = e1 + g1 + i1
  f (WLPA.AEdge "f" 1) = f1 + h1 + j1
  f (WLPA.AEdge "f" 2) = f2 + h2 + j2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testNewHomomorphism2 = WLPA.wLPA_relations_present f weighted_example0 weighted_example5
 where
  f (WLPA.AVertex "v") = v + u + w + x
  f (WLPA.AEdge "e" 1) = e1 + g1 + i1 + k1
  f (WLPA.AEdge "f" 1) = f1 + h1 + j1 + l1
  f (WLPA.AEdge "f" 2) = f2 + h2 + j2 + l2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testNewHomomorphism3 = WLPA.wLPA_relations_present f weighted_example2 weighted_example4
 where
  f (WLPA.AVertex "u") = u
  f (WLPA.AVertex "v") = v
  f (WLPA.AVertex "w") = w
  f (WLPA.AEdge "e" 1) = e1
  f (WLPA.AEdge "f" 1) = f1
  f (WLPA.AEdge "f" 2) = f2
  f (WLPA.AEdge "g" 1) = g1
  f (WLPA.AEdge "h" 1) = h1
  f (WLPA.AEdge "h" 2) = h2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testNewHomomorphism4 = WLPA.wLPA_relations_present f weighted_example2 weighted_example4
 where
  f (WLPA.AVertex "u") = w
  f (WLPA.AVertex "v") = u
  f (WLPA.AVertex "w") = v
  f (WLPA.AEdge "e" 1) = g1
  f (WLPA.AEdge "f" 1) = h1
  f (WLPA.AEdge "f" 2) = h2
  f (WLPA.AEdge "g" 1) = i1
  f (WLPA.AEdge "h" 1) = j1
  f (WLPA.AEdge "h" 2) = j2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

v_ = v + u + w
e1_ = e1 + g1 + i1
f1_ = f1 + h1 + j1
f2_ = f2 + h2 + j2

x_ = s f1 * f2
y_ = s f2 * e1
z_ = s f2 * f2

