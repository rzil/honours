module Example1 where

-- Example 18 from arXiv 1807.11675

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S
import FiniteFields
import GraphMonoid

import Control.Monad (liftM2, liftM3)
import Control.Monad.Omega

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = Graph (S.fromList ["u1","u2","u3","v"]) (M.fromList [("j",("u3","u3")),("i",("u3","u1")),("h",("u3","u2")),("g",("u2","u1")),("e1",("v","u1")),("e2",("v","u2")),("e3",("v","u3")),("f",("v","u2"))])

solvedGraph = graphGroupSolve unweighted_equivalent_example

-- this checks the isomorphism L_K(E,w) ~= L_K(F)
testIsomorphism = WLPA.wLPA_relations_present f weighted_example (convertGraphToWeighted unweighted_equivalent_example)
 where
  f (WLPA.AEdge "e" 1) = (edge "e1") + (edge "e2") + (edge "e3")
  f (WLPA.AEdge "f" 1) = edge "f"
  f (WLPA.AEdge "f" 2) = (edge "f") * (edge "g") + (edge "e1") * (ghostEdge "i") + (edge "e2") * (ghostEdge "h") + (edge "e3") * (ghostEdge "j")
  f (WLPA.AVertex "u") = (vertex "u1") + (vertex "u2") + (vertex "u3")
  f (WLPA.AVertex "v") = vertex "v"
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero

-- this checks the automorphism involution L_K(E,w) ~= L_K(E,w)
testAutomorphism = WLPA.wLPA_relations_present f weighted_example weighted_example
 where
  f (WLPA.AEdge "e" 1) = ghostEdge2 "f"
  f (WLPA.AEdge "f" 1) = ghostEdge "f"
  f (WLPA.AEdge "f" 2) = ghostEdge "e"
  f (WLPA.AVertex "u") = vertex "v"
  f (WLPA.AVertex "v") = vertex "u"
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero


-- this checks that the identity map is a automorphism
testId = WLPA.wLPA_relations_present f weighted_example weighted_example
 where
  f (WLPA.AEdge "e" 1) = edge "e"
  f (WLPA.AEdge "f" 1) = edge "f"
  f (WLPA.AEdge "f" 2) = edge2 "f"
  f (WLPA.AVertex "u") = vertex "u"
  f (WLPA.AVertex "v") = vertex "v"
  f (WLPA.AGhostEdge e w) = WLPA.adjoint (f (WLPA.AEdge e w))
  f _ = WLPA.Zero


-- search for endomorphisms
searchEndos = [(e,f1,f2) | (e,f1,f2) <- allTriples es es es, and (WLPA.wLPA_relations_check (f e f1 f2) weighted_example weighted_example)]
 where
  es = WLPA.elements weighted_example (tail z2)
  f e _  _  (WLPA.AEdge "e" 1) = e
  f _ f1 _  (WLPA.AEdge "f" 1) = f1
  f _ _  f2 (WLPA.AEdge "f" 2) = f2
  f _ _  _  (WLPA.AVertex "u") = vertex "u"
  f _ _  _  (WLPA.AVertex "v") = vertex "v"
  f e f1 f2 (WLPA.AGhostEdge ed w) = WLPA.adjoint (f e f1 f2 (WLPA.AEdge ed w))
  f _ _  _  _ = WLPA.Zero

allTriples xs ys zs = runOmega $ liftM3 (\a b c -> (a,b,c)) (each xs) (each ys) (each zs)

atom :: (Num k) => WLPA.AtomType edge vertex -> WLPA.Term edge vertex k
atom = WLPA.Atom 1

vertex :: (Num k) => vertex -> WLPA.Term edge vertex k
vertex = atom . WLPA.vertex

edge :: (Num k) => edge -> WLPA.Term edge vertex k
edge = atom . (flip WLPA.edge 1)

ghostEdge :: (Num k) => edge -> WLPA.Term edge vertex k
ghostEdge = atom . (flip WLPA.ghostEdge 1)

edge2 :: (Num k) => edge -> WLPA.Term edge vertex k
edge2 = atom . (flip WLPA.edge 2)

ghostEdge2 :: (Num k) => edge -> WLPA.Term edge vertex k
ghostEdge2 = atom . (flip WLPA.ghostEdge 2)

{-
    u = (vertex "u1") + (vertex "u2") + (vertex "u3")
    v = vertex "v"
    e1 = (edge "e1") + (edge "e2") + (edge "e3")
    f1 = edge "f"
    f2 = (edge "f") * (edge "g") + (edge "e1") * (ghostEdge "i") + (edge "e2") * (ghostEdge "h") + (edge "e3") * (ghostEdge "j")
-}

v__ = (vertex "u1") + (vertex "u2") + (vertex "u3") + (vertex "v")
e1__ = (edge "e1") + (edge "e2") + (edge "e3")
f1__ = edge "f"
f2__ = (edge "f") * (edge "g") + (edge "e1") * (ghostEdge "i") + (edge "e2") * (ghostEdge "h") + (edge "e3") * (ghostEdge "j")

u = vertex "u"
v = vertex "v"
e1 = edge "e"
f1 = edge "f"
f2 = edge2 "f"

s = WLPA.adjoint

v_ = v
u1_ = (s f2) * f1 * (s f1) * f2
u2_ = (s f1) * f2 * (s f2) * f1
u3_ = u - u1_ - u2_
e1_ = e1 - e2_ - f2 * j_
e2_ = e1 * u2_
e3_ = f2 * j_
f_ = f1
g_ = (s f1) * f2
h_ = (s f2) * e2_
i_ = (s f2) * (e1 - e2_ - f2 * j_)
j_ = s (u3_ * a)
 where a = (s (e1 - e2_)) * f2

{-
-- this gives the automorphism on the unweighted LPA
v_ = u
u1_ = e1 * (s f1) * f1 * (s e1)
u2_ = f1 * (s e1) * e1 * (s f1)
u3_ = v - u1_ - u2_
e1_ = (s f2) - e2_ - (s e1) * j_
e2_ = (s f2) * u2_
e3_ = (s e1) * j_
f_ = s f1
g_ = f1 * (s e1)
h_ = e1 * e2_
i_ = e1 * ((s f2) - e2_ - (s e1) * j_)
j_ = s (u3_ * a)
 where a = (s ((s f2) - e2_)) * (s e1)
-}

{-
u --> u + v
v --> u + v
e1 --> e1 + f2*
f1 --> f1 + f1*
f2 --> f2 + e1*
-}

-- almost an endomophism
-- kernel: u - v, e1 - f2*, f1 - f1*
uu = u + v
vv = u + v
e1e1 = e1 + (s f2)
f1f1 = f1 + (s f1)
f2f2 = f2 + (s e1)

convertToBasisForm = WLPA.convertToBasisForm (convertGraphToWeighted unweighted_equivalent_example)
