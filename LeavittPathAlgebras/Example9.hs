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

x' = f1*s e1
y' = e1*s f2
z' = e1*s e1

present s = (putStrLn . unlines . map show) s

