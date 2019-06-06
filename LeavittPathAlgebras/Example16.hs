{-# LANGUAGE DataKinds, TypeApplications #-}
module Example16 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Matrix.Static

weighted_graph_E :: WeightedGraph String String
weighted_graph_E = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

weighted_graph_G :: WeightedGraph String String
weighted_graph_G = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","t")),("h",("u","t"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

atom = WLPA.Atom 1
vertex = atom . WLPA.vertex
edge = atom . (flip WLPA.edge 1)
ghostEdge = atom . (flip WLPA.ghostEdge 1)

edge2 = atom . (flip WLPA.edge 2)
ghostEdge2 = atom . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

v = vertex "v"
u = vertex "u"
t = vertex "t"

e1 = edge "e"
f1 = edge "f"
f2 = edge2 "f"

adjoint m = fmap s (transpose m)

phi :: WLPA.AtomType String String -> Matrix 3 3 (WLPA.Term String String Integer)
phi (WLPA.AVertex "v") = unsafeSet v (1,1) zero
phi (WLPA.AVertex "u") = unsafeSet v (2,2) zero
phi (WLPA.AVertex "t") = unsafeSet v (3,3) zero
phi (WLPA.AEdge "e" 1) = unsafeSet e1 (1,2) zero
phi (WLPA.AEdge "f" 1) = unsafeSet f1 (1,2) zero
phi (WLPA.AEdge "f" 2) = unsafeSet f2 (1,2) zero
phi (WLPA.AEdge "g" 1) = unsafeSet e1 (2,3) zero
phi (WLPA.AEdge "h" 1) = unsafeSet f1 (2,3) zero
phi (WLPA.AEdge "h" 2) = unsafeSet f2 (2,3) zero
phi (WLPA.AGhostEdge e w) = adjoint (phi (WLPA.AEdge e w))
phi _ = zero

testHomomorphism :: [Matrix 3 3 (WLPA.Term String String Integer)]
testHomomorphism = WLPA.wLPA_relations_map' zero phi weighted_graph_G

fmapBasisForm wgraph = fmap (WLPA.convertToBasisForm wgraph)

check = map (fmapBasisForm weighted_graph_E) testHomomorphism

main = putStrLn$unlines$map show check

{-
wLPA_relations_show wg = wLPA_relations_map (Atom 1) wg

wLPA_relations_present f wgraph rgraph = do
  let check = wLPA_relations_check f wgraph rgraph
  let relations = wLPA_relations_show wgraph
  putStr $ unlines $ map show $ zip check relations
  print (and check)
-}


