module Example1 where

-- Example 18 from arXiv 1807.11675

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S
import FiniteFields
import GraphMonoid

{-
u.("e",1)*.("e",1) --> [u]
u.("e",1)*.("f",1) --> []
u.("f",1)*.("e",1) --> []
u.("f",1)*.("f",1) --> [u,-1.u.("f",2)*.("f",2)]
v.("f",1).("f",1)* --> [v,-1.v.("e",1).("e",1)*]
v.("f",1).("f",2)* --> []
v.("f",2).("f",1)* --> []
v.("f",2).("f",2)* --> [v]

automorphism:
u --> v
v --> u
e1 --> f2*
f1 --> f1*
f2 --> e1*

homomorphism to Example9
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

u --> v
v --> v
e1 --> e1
f1 --> f1
f2 --> f2

identity maps to 2*identity. So nullary operations not preserved.
u + v  -->  v + v = 2v

u --> 1/2 v
v --> 1/2 v
e1 --> sqrt(1/2) e1
f1 --> sqrt(1/2) f1
f2 --> sqrt(1/2) f2

Now u + v --> 1/2 v + 1/2 v = v
Other relations still hold.
works!

Doesn't work for Z2 (or any field of characteristic 2) because 1/2 doesn't exist.

Try Z7

u --> 4v
v --> 4v
So u + v  -->  8v = v
e1 --> 2 e1
f1 --> 2 f1
f2 --> 2 f2

what is the kernel?
u - v --> 1/2 v - 1/2 v = 0
v - u --> 0
S = {a u + b v | a + b = 0} --> a v + b v = (a + b) v = 0
S A --> 0
A S --> 0
-}

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = Graph (S.fromList ["u1","u2","u3","v"]) (M.fromList [("j",("u3","u3")),("i",("u3","u1")),("h",("u3","u2")),("g",("u2","u1")),("e1",("v","u1")),("e2",("v","u2")),("e3",("v","u3")),("f",("v","u2"))])

solvedGraph = graphGroupSolve unweighted_equivalent_example

atom = WLPA.Atom 1
vertex = atom . WLPA.vertex
edge = atom . (flip WLPA.edge 1)
ghostEdge = atom . (flip WLPA.ghostEdge 1)
edge2 = atom . (flip WLPA.edge 2)
ghostEdge2 = atom . (flip WLPA.ghostEdge 2)

{-
    u = (vertex "u1") + (vertex "u2") + (vertex "u3")
    v = vertex "v"
    e1 = (edge "e1") + (edge "e2") + (edge "e3")
    f1 = edge "f"
    f2 = (edge "f") * (edge "g") + (edge "e1") * (ghostEdge "i") + (edge "e2") * (ghostEdge "h") + (edge "e3") * (ghostEdge "j")
-}

u = vertex "u"
v = vertex "v"
e1 = edge "e"
f1 = edge "f"
f2 = edge2 "f"

v__ = vertex "v"
u1__ = ghostEdge2 "f" * edge "f" * ghostEdge "f" * edge2 "f"
u2__ = ghostEdge "f" * edge2 "f" * ghostEdge2 "f" * edge "f"
u3__ = (vertex "u") - u1_ - u2_
e1__ = edge "e" - e2_ - (edge2 "f") * j_
e2__ = (edge "e") * u2_
e3__ = edge2 "f" * j_
f__ = edge "f"
g__ = ghostEdge "f" * edge2 "f"
h__ = ghostEdge2 "f" * e2_
i__ = ghostEdge2 "f" * ((edge "e") - e2_ - (edge2 "f") * j_)
j__ = WLPA.adjoint (u3_ * a)
 where a = (WLPA.adjoint ((edge "e") - e2_)) * (edge2 "f")

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

convertToBasisForm = WLPA.convertToBasisForm (convertGraphToWeighted unweighted_equivalent_example)
