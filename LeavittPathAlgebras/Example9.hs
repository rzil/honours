module Example9 where

-- Example 19 from arXiv 1807.11675

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S
import FiniteFields
import GraphMonoid
import Data.List (nubBy)

-- this tests all relations under the mapping
testAutomorphism = WLPA.wLPA_relations_present f weighted_example weighted_example
 where
  -- the automorphism mapping
  f (WLPA.AEdge "e" 1) = ghostEdge2 1 "f"
  f (WLPA.AEdge "f" 1) = ghostEdge 1 "f"
  f (WLPA.AEdge "f" 2) = ghostEdge 1 "e"
  f (WLPA.AVertex "v") = vertex 1 "v"
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testZero = WLPA.wLPA_relations_present (const WLPA.Zero) weighted_example weighted_example

-- there is a monomorphism from this lpa to the weighted lpa
unweighted_example = buildGraphFromEdges [ ("g",("u2","u1")), ("h",("u3","u2")), ("i",("u3","u1")), ("j",("u3","u3")) ]

-- this tests we have a well defined homomorphism
testHomomorphism = WLPA.wLPA_relations_present f (convertGraphToWeighted unweighted_example) weighted_example
 where
  f (WLPA.AVertex "u1") = u1
  f (WLPA.AVertex "u2") = u2
  f (WLPA.AVertex "u3") = u3
  f (WLPA.AEdge "g" 1) = g
  f (WLPA.AEdge "h" 1) = h
  f (WLPA.AEdge "i" 1) = i
  f (WLPA.AEdge "j" 1) = j
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero 

-- this tests that the homomorphism is injective
testMonomorphism = WLPA.wLPA_relations_present f (convertGraphToWeighted unweighted_example) (convertGraphToWeighted unweighted_example)
 where
  f (WLPA.AVertex "u1") = u1_
  f (WLPA.AVertex "u2") = u2_
  f (WLPA.AVertex "u3") = u3_
  f (WLPA.AEdge "g" 1) = g_
  f (WLPA.AEdge "h" 1) = h_
  f (WLPA.AEdge "i" 1) = i_
  f (WLPA.AEdge "j" 1) = j_
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero 

-- this tests for a homomorphism from the Toeplitz algebra
testToeplitzHomomorphism = WLPA.wLPA_relations_present f (convertGraphToWeighted toeplitz_example) (convertGraphToWeighted unweighted_example)
 where
  f (WLPA.AVertex "u1") = vertex 1 "u1" + vertex 1 "u2"
  f (WLPA.AVertex "u2") = vertex 1 "u3"
  f (WLPA.AEdge "a" 1) = edge 1 "j"
  f (WLPA.AEdge "b" 1) = edge 1 "h" + edge 1 "i"
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero 

-- these are mutually orthogonal idempotents
idems k = (edge 1 "e")^k * (edge 1 "f") * (ghostEdge 1 "f") * (ghostEdge 1 "e")^k

toeplitz_example :: Graph String String
toeplitz_example = buildGraphFromEdges [("a",("u2","u2")),("b",("u2","u1"))]

-- GK dim = infinity
weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

atom c = WLPA.Atom c
vertex c = atom c . WLPA.vertex
edge c = atom c . (flip WLPA.edge 1)
ghostEdge c = atom c . (flip WLPA.ghostEdge 1)

edge2 c = atom c . (flip WLPA.edge 2)
ghostEdge2 c = atom c . (flip WLPA.ghostEdge 2)

v = vertex 1 "v"
e1 = edge 1 "e"
f1 = edge 1 "f"
f2 = edge2 1 "f"

s = WLPA.adjoint

-- these three elements from the wLPA generate the unweighted LPA sub-algebra
x = (s f1) * f2
y = (s f2) * e1
z = (s f2) * f2

x' = f1*s e1
y' = e1*s f2
z' = e1*s e1

-- these are the vertices of the unweighted LPA
u1 = (s x) * x
u2 = x * (s x)
u3 = (s y) * y - (s x) * x - x * (s x)

-- these are the edges of the unweighted LPA
g = x
h = y - y*z
j = y^2 * (s y)
i = y*(z - y * (s y))

-- these are the generators x,y,z expressed in terms of the edges of the LPA
-- this shows the map is invertible, thus injective
x_ = edge 1 "g"
y_ = edge 1 "h" + edge 1 "i" + edge 1 "j"
z_ = (s (edge 1 "h") + s (edge 1 "i") + s (edge 1 "j")) * (edge 1 "i" + edge 1 "j")
v_ = s y_ * y_

-- these are the vertices of the unweighted LPA
u1_ = (s x_) * x_
u2_ = x_ * (s x_)
u3_ = (s y_) * y_ - (s x_) * x_ - x_ * (s x_)

-- these are the edges of the unweighted LPA
g_ = x_
h_ = y_ - y_*z_
j_ = y_^2 * (s y_)
i_ = y_*(z_ - y_ * (s y_))

present s = (putStrLn . unlines . map show) s

