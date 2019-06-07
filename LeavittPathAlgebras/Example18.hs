module Example18 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Matrix

weighted_graph_E :: WeightedGraph String String
weighted_graph_E = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

weighted_graph_G :: Int -> WeightedGraph String String
weighted_graph_G n = WeightedGraph (buildGraphFromEdges edges) (M.fromList weights)
 where
  edges = concat [[("e"++show i,("v"++show i,"v"++show (i+1))),("f"++show i,("v"++show i,"v"++show (i+1)))] | i <- [1..n-1]]
  weights = concat [[("e"++show i,1),("f"++show i,2)] | i <- [1..n-1]]

atom = WLPA.Atom 1
vertex = atom . WLPA.vertex
edge = atom . (flip WLPA.edge 1)
ghostEdge = atom . (flip WLPA.ghostEdge 1)

edge2 = atom . (flip WLPA.edge 2)
ghostEdge2 = atom . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

v = vertex "v"

e1 = edge "e"
f1 = edge "f"
f2 = edge2 "f"

adjoint m = fmap s (transpose m)

phi :: Int -> WLPA.AtomType String String -> Matrix (WLPA.Term String String Integer)
phi n (WLPA.AVertex ('v':num)) = let i = read num in setElem v (i,i) (zero n n)
phi n (WLPA.AEdge ('e':num) 1) = let i = read num in setElem e1 (i,i+1) (zero n n)
phi n (WLPA.AEdge ('f':num) 1) = let i = read num in setElem f1 (i,i+1) (zero n n)
phi n (WLPA.AEdge ('f':num) 2) = let i = read num in setElem f2 (i,i+1) (zero n n)
phi n (WLPA.AGhostEdge e w) = adjoint (phi n (WLPA.AEdge e w))
phi n _ = zero n n

testHomomorphism :: Int -> [Matrix (WLPA.Term String String Integer)]
testHomomorphism n = WLPA.wLPA_relations_map' (zero n n) (phi n) (weighted_graph_G n)

fmapBasisForm wgraph = fmap (WLPA.convertToBasisForm wgraph)

check n = map (fmapBasisForm weighted_graph_E) (testHomomorphism n)

main n = do
   putStr $ unlines $ map show (zip checks rels)
   putStrLn (show $ and checks)
 where
  checks = map (matrix n n (const []) ==) (check n)
  rels = WLPA.wLPA_relations_show (weighted_graph_G n)
