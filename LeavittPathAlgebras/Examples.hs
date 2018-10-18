module Examples where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_example :: WeightedGraph String String
weighted_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

unweighted_equivalent_example :: Graph String String
unweighted_equivalent_example = Graph (S.fromList ["u1","u2","u3","v"]) (M.fromList [("j",("u3","u3")),("i",("u3","u1")),("h",("u3","u2")),("g",("u2","u1")),("e1",("v","u1")),("e2",("v","u2")),("e3",("v","u3")),("f",("v","u2"))])

vertex = WLPA.atom . WLPA.vertex
edge = WLPA.atom . (flip WLPA.edge 1)
ghostEdge = WLPA.atom . (flip WLPA.ghostEdge 1)

u = (vertex "u1") + (vertex "u2") + (vertex "u3")
v = vertex "v"
e1 = (edge "e1") + (edge "e2") + (edge "e3")
f1 = edge "f"
f2 = (edge "f") * (edge "g") + (edge "e1") * (ghostEdge "i") + (edge "e2") * (ghostEdge "h") + (edge "e3") * (ghostEdge "j")
