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

e1 = edge "e"
f1 = edge "f"
f2 = edge2 "f"

g1 = edge "g"
h1 = edge "h"
h2 = edge2 "h"

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

testOriginalMonomorphism = WLPA.wLPA_relations_present f weighted_example0 weighted_example3
 where
  f (WLPA.AVertex "v") = v + u
  f (WLPA.AEdge "e" 1) = e1 + g1
  f (WLPA.AEdge "f" 1) = f1 + h1
  f (WLPA.AEdge "f" 2) = f2 + h2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

v_ = v + u
e1_ = e1 + g1
f1_ = f1 + h1
f2_ = f2 + h2

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

testOriginalHomomorphism = WLPA.wLPA_relations_present f weighted_example0 weighted_example
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
