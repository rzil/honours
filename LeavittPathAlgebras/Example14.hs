module Example14 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

import Polynomial
import LaurentPolynomial

newtype Matrix2x2 a = Matrix2x2 (a,a,a,a)

mat2x2 a b c d = Matrix2x2 (a,b,c,d)

instance Eq a => Eq (Matrix2x2 a) where
  (Matrix2x2 xs) == (Matrix2x2 ys) = xs == ys

instance Show a => Show (Matrix2x2 a) where
  show (Matrix2x2 (a,b,c,d)) = show a++"\t"++show b++"\n"++show c++"\t"++show d

instance Functor Matrix2x2 where
  fmap f (Matrix2x2 (a,b,c,d)) = mat2x2 (f a) (f b) (f c) (f d)

instance Num a => Num (Matrix2x2 a) where
  (Matrix2x2 (a,b,c,d)) + (Matrix2x2 (e,f,g,h)) = mat2x2 (a+e) (b+f) (c+g) (d+h)
  negate = fmap negate
  (Matrix2x2 (a,b,c,d)) * (Matrix2x2 (e,f,g,h)) = mat2x2 (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)
  fromInteger n = mat2x2 (fromInteger n) (fromInteger 0) (fromInteger 0) (fromInteger n)
  abs m = undefined
  signum m = undefined

transpose (Matrix2x2 (a,b,c,d)) = mat2x2 a c b d

--weighted_graph_E :: WeightedGraph String String
--weighted_graph_E = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

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

--adjoint :: Matrix2x2 (LaurentPolynomial (WLPA.Term e v k)) -> Matrix2x2 (LaurentPolynomial (WLPA.Term e v k))
adjoint isZero m = fmap (reverseLaurentPolynomial isZero . fmap (WLPA.adjoint)) (transpose m)

--phi :: WLPA.AtomType String String -> Matrix2x2 (LaurentPolynomial (WLPA.Term String String Integer))
phi (WLPA.AVertex "u") = mat2x2 z z z (c u)
phi (WLPA.AVertex "v") = mat2x2 (c v) z z z
phi (WLPA.AEdge "e" 1) = mat2x2 z (x e1) z z
phi (WLPA.AEdge "f" 1) = mat2x2 z (x f1) z z
phi (WLPA.AEdge "f" 2) = mat2x2 z (x f2) z z
phi (WLPA.AEdge "g" 1) = mat2x2 z z (x (s f2)) z
phi (WLPA.AEdge "h" 1) = mat2x2 z z (x (s f1)) z
phi (WLPA.AEdge "h" 2) = mat2x2 z z (x (s e1)) z
phi (WLPA.AGhostEdge e w) = (adjoint isZero) (phi (WLPA.AEdge e w))
 where isZero = WLPA.equal_wrt_graph weighted_graph_G WLPA.Zero
phi _ = mat2x2 z z z z

testHomomorphism :: [Matrix2x2 (LaurentPolynomial (WLPA.Term String String Integer))]
testHomomorphism = WLPA.wLPA_relations_map phi weighted_graph_F

fmapBasisForm wgraph = fmap (fmap (WLPA.convertToBasisForm wgraph))

check = map (fmapBasisForm weighted_graph_G) testHomomorphism

{-
wLPA_relations_show wg = wLPA_relations_map (Atom 1) wg

wLPA_relations_present f wgraph rgraph = do
  let check = wLPA_relations_check f wgraph rgraph
  let relations = wLPA_relations_show wgraph
  putStr $ unlines $ map show $ zip check relations
  print (and check)
-}
