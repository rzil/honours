module Example9 where

-- Example 19 from arXiv 1807.11675

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S
import FiniteFields

import Control.Monad (liftM2, liftM3)
import Control.Monad.Omega

{-
*Example9> putStrLn $ unlines $ take 30 $ map show $ WLPA.projections weighted_example [Z2_1]
0
v
v.("e",1).("e",1)*

Each vertex in the LPA would generate a projection via the equation:
e1* e1 = v

simply substitute e1 --> e1 w, where w is any vertex in the LPA.
to get (e1 w)* (e1 w) = w
and hence the projection ((e1 w) (e1 w)*) ((e1 w) (e1 w)*)* = ((e1 w) (e1 w)*)
-}

{-
v.("e",1)*.("e",1) --> [v]
v.("e",1)*.("f",1) --> []
v.("f",1)*.("e",1) --> []
v.("f",1)*.("f",1) --> [v,-1.v.("f",2)*.("f",2)]
v.("f",1).("f",1)* --> [v,-1.v.("e",1).("e",1)*]
v.("f",1).("f",2)* --> []
v.("f",2).("f",1)* --> []
v.("f",2).("f",2)* --> [v]

automorphism:
v --> v
e1 --> f2*
f1 --> f1*
f2 --> e1*
-}

-- this tests all relations under the mapping
test = putStrLn $ unlines $ map show $ WLPA.wLPA_relations f weighted_example weighted_example
 where
  -- the automorphism mapping
  f (WLPA.AEdge "e" 1) = ghostEdge2 1 "f"
  f (WLPA.AEdge "f" 1) = ghostEdge 1 "f"
  f (WLPA.AEdge "f" 2) = ghostEdge 1 "e"
  f (WLPA.AVertex "v") = vertex 1 "v"
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

testZero = putStrLn $ unlines $ map show $ WLPA.wLPA_relations (const WLPA.Zero) weighted_example weighted_example

unweighted_example = buildGraphFromEdges [ ("g",("u2","u1")), ("h",("u3","u2")), ("i",("u3","u1")), ("j",("u3","u3")) ]

test2 = putStrLn $ unlines $ map show $ WLPA.wLPA_relations f (convertGraphToWeighted unweighted_example) weighted_example
 where
  -- the automorphism mapping
  f (WLPA.AEdge "u1" 1) = u1
  f (WLPA.AEdge "u2" 1) = u2
  f (WLPA.AEdge "u3" 1) = u3
  f (WLPA.AEdge "g" 2) = g
  f (WLPA.AEdge "h" 2) = h
  f (WLPA.AEdge "i" 2) = i
  f (WLPA.AEdge "j" 2) = j
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero 

-- these are mutually orthogonal idempotents
idems k = (edge 1 "e")^k * (edge 1 "f") * (ghostEdge 1 "f") * (ghostEdge 1 "e")^k
finalIdem = (vertex 1 "v") - (idems 1) - (idems 2) - (idems 3) - (idems 4)
--

allPairs xs ys = runOmega $ liftM2 (,) (each xs) (each ys)
allTriples xs ys zs = runOmega $ liftM3 (\a b c -> (a,b,c)) (each xs) (each ys) (each zs)

-- GK dim = infinity
weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

--unweighted_equivalent_example :: Graph String String
--unweighted_equivalent_example = buildGraphFromEdges [("a1",("v","u1")),("a2",("u1","u2")),("b1",("v","w")),("c1",("w","u11")),("k1",("u1","x")),("k2",("u2","x")),("k3",("u11","x"))]

-- GK dim = 2
toeplitzGraph = buildGraphFromEdges [("e",("u","u")),("f",("u","v"))]

-- GK dim = infinity
rosePetalGraph n = buildGraphFromEdges [(k,("u","u")) | k <- [1..n]]

--

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

u1 = (s f2) * f1 * (s f1) * f2
u2 = (s f1) * f2 * (s f2) * f1
u3 = v - u1 - u2
g = (s f1) * f2
h = (s f2) * e1 * u2

a = (s (e1 - e1*u2)) * f2
b = u3
c = e1*u2

j = s (b*a)
i = (s f2)*(e1 - c - f2*(s (b*a)))
