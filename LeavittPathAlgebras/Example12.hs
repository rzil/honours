module Example12 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S


atom c = WLPA.Atom c
vertex = atom 1 . WLPA.vertex

edge = atom 1 . (flip WLPA.edge 1)
ghostEdge = atom 1 . (flip WLPA.ghostEdge 1)
edge2 = atom 1 . (flip WLPA.edge 2)
ghostEdge2 = atom 1 . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

graph_F :: Graph String String
graph_F = Graph (S.fromList ["u1","u2","u3","v"]) (M.fromList [("j",("u3","u3")),("i",("u3","u1")),("h",("u3","u2")),("g",("u2","u1")),("e1",("v","u1")),("e2",("v","u2")),("e3",("v","u3")),("f",("v","u2"))])

-- Below here are the generators for the weighted graph (H,w_h)

e1_ = edge "e1" + edge "e2" + edge "e3"
f1_ = edge "f"
f2_ = edge "f" * edge "g" + edge "e1" * ghostEdge "i" + edge "e2" * ghostEdge "h" + edge "e3" * ghostEdge "j"

-- Below here are the generators for the weighted graph (E,w)

e1 = e1_ + s f2_
f1 = f1_ + s f1_
f2 = f2_ + s e1_

--- Below here are the generators for the unweighted graph F

-- these three elements from the wLPA generate the unweighted LPA sub-algebra
x = (s f1) * f2
y = (s f2) * e1
z = (s f2) * f2

-- these are the vertices of the unweighted LPA
u1 = (s x) * x
u2 = x * (s x)
u3 = (s y) * y - (s x) * x - x * (s x)

-- these are the edges of the unweighted LPA
g = x
h = y - y*z
j = y^2 * (s y)
i = y*(z - y * (s y))

test = map (WLPA.convertToBasisForm (convertGraphToWeighted graph_F)) [u1,u2,u3,g,h,j,i]
