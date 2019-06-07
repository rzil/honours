module Example15 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

import Polynomial
import LaurentPolynomial

weighted_graph_F :: WeightedGraph String String
weighted_graph_F = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","v")),("h",("u","v"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

weighted_graph_G :: WeightedGraph String String
weighted_graph_G = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

atom = WLPA.Atom 1
vertex = atom . WLPA.vertex
edge = atom . (flip WLPA.edge 1)
ghostEdge = atom . (flip WLPA.ghostEdge 1)

edge2 = atom . (flip WLPA.edge 2)
ghostEdge2 = atom . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

v = vertex "v"
u = vertex "u"

e1 = edge "e"
f1 = edge "f"
f2 = edge2 "f"

g1 = edge "g"
h1 = edge "h"
h2 = edge2 "h"

z = LaurentPolynomial (constant WLPA.Zero) 0

c y = LaurentPolynomial (constant y) 0
x y = LaurentPolynomial (constant y) 1

adjoint isZero m = (reverseLaurentPolynomial isZero . fmap (WLPA.adjoint)) m

--phi :: WLPA.AtomType String String -> Matrix2x2 (LaurentPolynomial (WLPA.Term String String Integer))
phi (WLPA.AVertex "u") = c u
phi (WLPA.AVertex "v") = c v
phi (WLPA.AEdge "e" 1) = x e1
phi (WLPA.AEdge "f" 1) = x f1
phi (WLPA.AEdge "f" 2) = x f2
phi (WLPA.AEdge "g" 1) = x (s f2)
phi (WLPA.AEdge "h" 1) = x (s f1)
phi (WLPA.AEdge "h" 2) = x (s e1)
phi (WLPA.AGhostEdge e w) = (adjoint isZero) (phi (WLPA.AEdge e w))
 where isZero = WLPA.equal_wrt_graph weighted_graph_G WLPA.Zero
phi _ = z

testHomomorphism :: [LaurentPolynomial (WLPA.Term String String Integer)]
testHomomorphism = WLPA.wLPA_relations_map phi weighted_graph_F

fmapBasisForm wgraph = fmap (WLPA.convertToBasisForm wgraph)

check = map (fmapBasisForm weighted_graph_G) testHomomorphism

main = putStrLn$unlines$map show check

{-
wLPA_relations_show wg = wLPA_relations_map (Atom 1) wg

wLPA_relations_present f wgraph rgraph = do
  let check = wLPA_relations_check f wgraph rgraph
  let relations = wLPA_relations_show wgraph
  putStr $ unlines $ map show $ zip check relations
  print (and check)
-}
