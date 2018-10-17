module Examples where

import WeightedLPA hiding (atom, vertex, edge, ghostEdge)
import LPA (atom, vertex, edge, ghostEdge)
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = Graph (S.fromList ["u1","u2","u3","v"]) (M.fromList [("j",("u3","u3")),("i",("u3","u1")),("h",("u3","u2")),("g",("u2","u1")),("e1",("v","u1")),("e2",("v","u2")),("e3",("v","u3")),("f",("v","u2"))])

u = (atom (vertex "u1")) + (atom (vertex "u2")) + (atom (vertex "u3"))
v = atom (vertex "v")
e1 = (atom (edge "e1")) + (atom (edge "e2")) + (atom (edge "e3"))
f1 = atom (edge "f")
f2 = (atom (edge "f")) * (atom (edge "g")) + (atom (edge "e1")) * (atom (ghostEdge "i")) + (atom (edge "e2")) * (atom (ghostEdge "h")) + (atom (edge "e3")) * (atom (ghostEdge "j"))
