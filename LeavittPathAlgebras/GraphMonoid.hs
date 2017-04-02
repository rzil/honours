module GraphMonoid where

import Graph
import Matrix hiding (solve)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ratio
import Data.List (delete,maximumBy,find)
import Control.Exception (assert)
import Data.Maybe (isJust)
import Data.Bifunctor

type SolutionState v = M.Map v Integer
type TransformationsState v = M.Map v Integer

-- Uses a "greedy" type of algorithm

-- finds a solution to the graph monoid problem, if it exists
graphGroupSolve :: (Ord v, Integral a) => Graph k v -> Maybe (a, [a])
graphGroupSolve graph = do
  let ggMatrix = graphGroupMatrix graph
  let columnOnes = take (nrows ggMatrix) (repeat 1)
  solution <- solve1 ggMatrix columnOnes
  let lcd = foldl1 lcm (Prelude.map denominator solution)
  let toInt r = (numerator r) `div` (denominator r)
  return (lcd,(Prelude.map (toInt . ((fromIntegral lcd) *)) solution))

-- the system has no solutions if the rank of the augmented matrix is greater
-- than the rank of the coefficient matrix
graphGroupMatrix :: (Ord v, Num c) => Graph k v -> Matrix c
graphGroupMatrix graph = transformationMatrix
 where
  adj = adjacencyMatrix graph
  minus = elementwise (-)
  transformationMatrix = (transpose adj) `minus` (fromMainDiagonal [if isSink graph v then 0 else 1 | v <- S.toList (vertices graph)])

solvable :: Ord v => Graph k v -> Bool
solvable graph = isJust (graphGroupSolve graph)

startState :: (Ord k, Num b, Integral a) => Graph e k -> (M.Map k a, M.Map k b)
startState graph = (M.fromList (zip vs vec),M.fromList (zip vs (repeat 1)))
 where
  vs = S.toList (vertices graph)
  Just (n,vec) = graphGroupSolve graph

applyVertex :: Ord v => M.Map v [v] -> (TransformationsState v,SolutionState v) -> v -> (TransformationsState v,SolutionState v)
applyVertex mapping (transformationState,state) vertex = (updateTransformations transformationState vertex,foldr (M.adjust inc) (M.adjust dec vertex state) vs)
 where
  forward = transformationState M.! vertex > 0
  inc x = if forward then x + 1 else x - 1
  dec x = if forward then x - 1 else x + 1
  vs = mapping M.! vertex

stateMinimum :: Ord a => M.Map k a -> a
stateMinimum state = minimum (M.elems state)

found :: (Num a, Eq a) => M.Map k a -> Bool
found state = all (== 0) (M.elems state)

chooseNextVertex :: Ord k => M.Map k [k] -> TransformationsState k -> SolutionState k -> k
chooseNextVertex mapping transformations state = maximumBy comparison (filter (\k -> (transformations M.! k) /= 0) (M.keys mapping))
 where comparison x y = compare (stateMinimum (snd $ applyVertex mapping (transformations,state) x)) (stateMinimum (snd $ applyVertex mapping (transformations,state) y))

updateTransformations :: Ord k => TransformationsState k -> k -> TransformationsState k
updateTransformations transformations vertex = M.adjust inc vertex transformations
 where inc x | x > 0 = x - 1
             | x < 0 = x + 1
             | otherwise = x

step mapping (transformations,state) = (vertex,applyVertex mapping (transformations,state) vertex)
 where
  vertex = chooseNextVertex mapping transformations state

solve :: Ord v => Graph e v -> [v]
solve graph = solve' (vertexForm graph) (startState graph)
 where
  check state = assert (all (>= 0) (M.elems state))
  solve' mapping (transformations,state)
   | found transformations = []
   | otherwise = check state (let (v,next) = step mapping (transformations,state) in v : solve' mapping next)

checkSolution :: Ord v => Graph e v -> [v] -> (TransformationsState v,SolutionState v)
checkSolution graph vs = foldl (applyVertex mapping) state vs
 where
  mapping = vertexForm graph
  state = startState graph

-- | Construct the graph whose Leavitt Path Algebra is isomorphic to the Cohn algebra of the original
-- | Cohn algebras always have the IBN property
cohn :: (Ord k, Ord a) => Graph k a -> Graph (k, Bool) (a, Bool)
cohn graph = updateEdges ff j
 where
  h = vertexMap (flip (,) True) graph
  ns = S.map (fmap (const False)) (S.filter (not . isSink h) (vertices h))
  j = h {vertices = S.union (vertices h) ns}
  ff es = M.fromList $ [((k,True),es M.! k) | k <- M.keys es, isSink j (snd (es M.! k))] ++ concat [[((k,False),fmap (fmap (const False)) (es M.! k)), ((k,True),es M.! k)] | k <- M.keys es, not (isSink j (snd (es M.! k)))]

-- Examples

weakly_connected_n_graphs n = nubGraphs [g | g <- [fromAdjacencyMatrix m | m <- logicalMatricesConstantDiagonal 0 n], weaklyConnected g]

weakly_connected_unsolvable_n_graphs n = nubGraphs [g | g <- [fromAdjacencyMatrix m | m <- logicalMatricesConstantDiagonal 0 n], weaklyConnected g, not (solvable g)]

star_inner_scc = graphUnion (buildGraphFromEdges [(("e"++show v,True),(v,v+3)) | v <- [1,2,3]]) (makeUndirected (cycleGraph 3))

n_loop_graph n = buildGraphFromEdges [("e" ++ show i,("v","v")) | i <- [1..n]]

my_graph = buildGraphFromEdges [("a",("x","y")),("b",("x","z")),("c",("x","z"))]

six_cycle_with_two_chords = updateEdges (M.insert "h" (2,5)) $ updateEdges (M.insert "f" (1,3)) (cycleGraph 6)

hamil1 = randomHamiltonianGraph 1 5 0.8

hamil2 = randomHamiltonianGraph 2 5 0.7

hamil3 = randomHamiltonianGraph 3 8 0.61

rand1 = randomGraph 10 11 0.1

rand2 = randomGraph 11 13 0.3

rand3 = randomGraph 31 23 0.2

rand4 = randomGraph 121 17 0.2

rand5 = randomGraph 1211 17 0.3

-- this one fails!
rand6 = randomGraph 2 11 0.1
