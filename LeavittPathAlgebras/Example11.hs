module Example11 where

-- Example 19 from arXiv 1807.11675

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S
import FiniteFields
import GraphMonoid

weighted_example0 :: WeightedGraph String String
weighted_example0 = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

weighted_example2 :: WeightedGraph String String
weighted_example2 = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","w")),("h",("u","w"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

weighted_example3 :: WeightedGraph String String
weighted_example3 = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","v")),("h",("u","v"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

atom c = WLPA.Atom c
vertex c = atom c . WLPA.vertex
edge c = atom c . (flip WLPA.edge 1)
ghostEdge c = atom c . (flip WLPA.ghostEdge 1)

edge2 c = atom c . (flip WLPA.edge 2)
ghostEdge2 c = atom c . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

v = vertex 1 "v"
u = vertex 1 "u"
w = vertex 1 "w"

e1 = edge 1 "e"
f1 = edge 1 "f"
f2 = edge2 1 "f"

g1 = edge 1 "g"
h1 = edge 1 "h"
h2 = edge2 1 "h"

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

testMonomorphism4 = WLPA.wLPA_relations_present f weighted_example0 weighted_example3
 where
  f (WLPA.AVertex "v") = v + u
  f (WLPA.AEdge "e" 1) = e1 + g1
  f (WLPA.AEdge "f" 1) = f1 + h1
  f (WLPA.AEdge "f" 2) = f2 + h2
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero
