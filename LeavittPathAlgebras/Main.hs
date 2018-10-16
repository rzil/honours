module Main where

import Graph
import qualified LPA as LPA
import qualified WeightedLPA as WLPA
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort)

u = (LPA.atom (LPA.vertex "u1")) + (LPA.atom (LPA.vertex "u2")) + (LPA.atom (LPA.vertex "u3"))
v = LPA.atom (LPA.vertex "v")
e1 = (LPA.atom (LPA.edge "e1")) + (LPA.atom (LPA.edge "e2")) + (LPA.atom (LPA.edge "e3"))
f1 = LPA.atom (LPA.edge "f")
f2 = (LPA.atom (LPA.edge "f")) * (LPA.atom (LPA.edge "g")) + (LPA.atom (LPA.edge "e1")) * (LPA.atom (LPA.ghostEdge "i")) + (LPA.atom (LPA.edge "e2")) * (LPA.atom (LPA.ghostEdge "h")) + (LPA.atom (LPA.edge "e3")) * (LPA.atom (LPA.ghostEdge "j"))

iso_graph_lpa = Graph (S.fromList ["u1","u2","u3","v"]) (M.fromList [("j",("u3","u3")),("i",("u3","u1")),("h",("u3","u2")),("g",("u2","u1")),("e1",("v","u1")),("e2",("v","u2")),("e3",("v","u3")),("f",("v","u2"))])

eval = LPA.convertTermToBasis iso_graph_lpa

isoMapEdge (WLPA.NormalFormEdge "e" False 1) = e1
isoMapEdge (WLPA.NormalFormEdge "e" True 1) = LPA.adjoint e1
isoMapEdge (WLPA.NormalFormEdge "f" False 1) = f1
isoMapEdge (WLPA.NormalFormEdge "f" True 1) = LPA.adjoint f1
isoMapEdge (WLPA.NormalFormEdge "f" False 2) = f2
isoMapEdge (WLPA.NormalFormEdge "f" True 2) = LPA.adjoint f2

isoMapVertex "v" = v
isoMapVertex "u" = u

isoMapPath :: (Num a, Eq a) => WLPA.NormalFormAtom String String a -> LPA.NormalForm String String Int
isoMapPath (WLPA.NormalFormAtom 1 vertex path) = LPA.convertTermToBasis iso_graph_lpa ((isoMapVertex vertex) * (foldl1 (*) (map isoMapEdge path)))

main = do
   print x
   print y
   print xy
   print "--------"
   print fx
   print fy
   print fxy
   print fxfy
   print "--------"
   print $ sort fxy == sort fxfy
   
 where
  bs = WLPA.basis WLPA.iso_example
  x = bs !! 200
  y = bs !! 300
  xy = WLPA.convertToBasisForm WLPA.iso_example ((WLPA.convertTerm x) * (WLPA.convertTerm y))
  
  fx = isoMapPath x
  fy = isoMapPath y
  fxy = concatMap isoMapPath xy
  fxfy = LPA.convertTermToBasis iso_graph_lpa $ (sum $ map LPA.convertTerm $ isoMapPath x) * (sum $ map LPA.convertTerm $ isoMapPath y)
