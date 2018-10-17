module Main where

import Graph
import qualified LPA as LPA
import qualified WeightedLPA as WLPA
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sort)

import Control.Monad (liftM2)
import Control.Monad.Omega (runOmega, each)

u = (LPA.atom (LPA.vertex "u1")) + (LPA.atom (LPA.vertex "u2")) + (LPA.atom (LPA.vertex "u3"))
v = LPA.atom (LPA.vertex "v")
e1 = (LPA.atom (LPA.edge "e1")) + (LPA.atom (LPA.edge "e2")) + (LPA.atom (LPA.edge "e3"))
f1 = LPA.atom (LPA.edge "f")
f2 = (LPA.atom (LPA.edge "f")) * (LPA.atom (LPA.edge "g")) + (LPA.atom (LPA.edge "e1")) * (LPA.atom (LPA.ghostEdge "i")) + (LPA.atom (LPA.edge "e2")) * (LPA.atom (LPA.ghostEdge "h")) + (LPA.atom (LPA.edge "e3")) * (LPA.atom (LPA.ghostEdge "j"))

isoMapEdge (WLPA.NormalFormEdge "e" False 1) = e1
isoMapEdge (WLPA.NormalFormEdge "e" True 1) = LPA.adjoint e1
isoMapEdge (WLPA.NormalFormEdge "f" False 1) = f1
isoMapEdge (WLPA.NormalFormEdge "f" True 1) = LPA.adjoint f1
isoMapEdge (WLPA.NormalFormEdge "f" False 2) = f2
isoMapEdge (WLPA.NormalFormEdge "f" True 2) = LPA.adjoint f2

isoMapVertex "v" = v
isoMapVertex "u" = u

isoMapPath :: WLPA.NormalFormAtom String String Int -> LPA.NormalForm String String Int
isoMapPath (WLPA.NormalFormAtom 1 vertex []) = LPA.convertTermToBasis WLPA.unweighted_equivalent_example (isoMapVertex vertex)
isoMapPath (WLPA.NormalFormAtom 1 vertex path) = LPA.convertTermToBasis WLPA.unweighted_equivalent_example ((isoMapVertex vertex) * (foldl1 (*) (map isoMapEdge path)))
isoMapPath (WLPA.NormalFormAtom c vertex path) = map (\(LPA.NormalFormAtom k v es fs) -> LPA.NormalFormAtom (c*k) v es fs) (isoMapPath (WLPA.NormalFormAtom 1 vertex path))

verify
  :: WeightedGraph String String
     -> Graph String String
     -> WLPA.NormalFormAtom String String Int
     -> WLPA.NormalFormAtom String String Int
     -> Bool
verify weightedGraph unweightedGraph x y = sort fxy == sort fxfy
 where
  xy = WLPA.convertToBasisForm weightedGraph ((WLPA.convertTerm x) * (WLPA.convertTerm y))
  fx = isoMapPath x
  fy = isoMapPath y
  fxy = LPA.collectNormalFormAtoms $ concatMap isoMapPath xy
  fxfy = LPA.convertTermToBasis unweightedGraph $ (sum $ map LPA.convertTerm $ isoMapPath x) * (sum $ map LPA.convertTerm $ isoMapPath y)

findFails weightedGraph unweightedGraph = filter (not . fst) $ runOmega $ liftM2 (\(i,x) (j,y) -> (verify weightedGraph unweightedGraph x y,(i,j))) (each bs) (each bs)
 where bs = zip [0..] (WLPA.basis weightedGraph)

test weightedGraph unweightedGraph x y = do
   putStrLn $ "x: " ++ show x
   putStrLn $ "y: " ++ show y
   putStrLn $ "xy: " ++ show xy
   putStrLn $ "--------"
   putStrLn $ "fx: " ++ show fx
   putStrLn $ "fy: " ++ show fy
   putStrLn $ "fxy: " ++ show fxy
   putStrLn $ "fxfy: " ++ show fxfy
   putStrLn $ "--------"
   print $ sort fxy == sort fxfy
   
 where
  xy = WLPA.convertToBasisForm weightedGraph ((WLPA.convertTerm x) * (WLPA.convertTerm y))
  fx = isoMapPath x
  fy = isoMapPath y
  fxy = LPA.collectNormalFormAtoms $ concatMap isoMapPath xy
  fxfy = LPA.convertTermToBasis unweightedGraph $ (sum $ map LPA.convertTerm $ isoMapPath x) * (sum $ map LPA.convertTerm $ isoMapPath y)

main = test WLPA.weighted_example WLPA.unweighted_equivalent_example x y
   
 where
  bs = WLPA.basis WLPA.weighted_example
  x = bs !! 30
  y = bs !! 20
