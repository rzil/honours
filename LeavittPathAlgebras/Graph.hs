module Graph where

import qualified Data.Map as M
import Data.List (nub,(\\),delete,intercalate,permutations,nubBy,sort,sortBy,groupBy,findIndex)
import Matrix
import Data.Ratio
import Polynomial hiding (isZero)
import System.Random
import System.Process (callCommand)
import Data.Function (on)
import Data.Bifunctor
import Data.Maybe
import qualified Data.Set as S

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | A graph is a set of vertices and a map from edges to ordered pairs of vertices
data Graph e v = Graph {vertices :: S.Set v, edges :: M.Map e (v, v)}   deriving Show
type Weighting = Int
data WeightedGraph e v = WeightedGraph {graph :: Graph e v, weightings :: M.Map e Weighting}   deriving Show

convertGraphToWeighted :: Ord k => Graph k v -> WeightedGraph k v
convertGraphToWeighted g = WeightedGraph g (M.fromList (zip (M.keys (edges g)) (repeat 1)))

vertexWeight :: (Ord v, Ord k) => WeightedGraph k v -> v -> Weighting
vertexWeight weightedGraph v = maximum (0 : [(weightings weightedGraph) M.! e | e <- M.keys (edges (graphAtSource (graph weightedGraph) v))])

vertexMap :: Ord d => (v -> d) -> Graph e v -> Graph e d
vertexMap f graph = graph {vertices = S.map f (vertices graph), edges = M.map (bimap f f) (edges graph)}

edgeMap :: Ord e => (k -> e) -> Graph k v -> Graph e v
edgeMap f graph = graph {edges = M.mapKeys f (edges graph)}

updateEdges :: Ord v => (M.Map f (v, v) -> M.Map e (v, v)) -> Graph f v -> Graph e v
updateEdges f graph = graph {vertices = S.union (vertices graph) (verticesFromEdgesList newEdges), edges = newEdges}
 where
  newEdges = f (edges graph)
  verticesFromEdgesList edges = S.fromList $ concat [[a,b] | (a,b) <- M.elems edges]

graphUnion :: (Ord v, Ord e) => Graph e v -> Graph e v -> Graph e v
graphUnion g h = Graph {vertices = S.union (vertices g) (vertices h), edges = M.union (edges g) (edges h)}

directedGraphAssociatedToWeightedGraph :: (Ord v, Ord k) => WeightedGraph k v -> Graph (k, Weighting) v
directedGraphAssociatedToWeightedGraph (WeightedGraph g w) = buildGraphFromEdges [(f,edges g M.! e) | f@(e,_) <- fs]
 where es = M.keys (edges g)
       fs = concat [zip (repeat e) [1 .. w M.! e] | e <- es]

doubleGraph :: (Ord v, Ord k) => Graph k v -> Graph (k, Bool) v
doubleGraph g = buildGraphFromEdges [let (u,v) = edges g M.! e in (f,if forward then (u,v) else (v,u)) | f@(e, forward) <- fs]
 where es = M.keys (edges g)
       fs = (map (flip (,) True) es) ++ (map (flip (,) False) es)

order :: Graph e a -> Int
order graph = length (vertices graph)

size :: Graph e v -> Int
size graph = length (edges graph)

-- | Build a graph from a list of edges.
buildGraphFromEdges :: (Ord v, Ord e) => [(e, (v, v))] -> Graph e v
buildGraphFromEdges edges = Graph (S.fromList $ concat [[a,b] | (_,(a,b)) <- edges]) (M.fromList edges)

-- | Build a graph from a list of vertices with adjacencies
buildGraphFromAdjacencies :: Ord v => [(v, [v])] -> Graph [Char] v
buildGraphFromAdjacencies vs = buildGraphFromEdges (adjacenciesToEdges [1..] vs)
 where
  adjacenciesToEdges _ [] = []
  adjacenciesToEdges ns ((v,as):vs) = [("e" ++ show n,(v,a)) | (n,a) <- zip ns as] ++ (adjacenciesToEdges (drop (length as) ns) vs)

logicalMatrices :: Num a => Int -> [Matrix a]
logicalMatrices n = [matrix n n (\(r,c) -> xs!!(n*(r-1)+(c-1))) | xs <- listsLengthNFrom (n*n) [0,1]]

--logicalMatricesConstantDiagonal :: Num a => Int -> [Matrix a]
--logicalMatricesConstantDiagonal k n = [matrix n n (\(r,c) -> if r == c then k else xs!!(n*(r-1 - (if r>c then 1 else 0))+(c-1))) | xs <- listsLengthNFrom (n * (n-1)) [0,1]]
logicalMatricesConstantDiagonal k n = [matrix n n (\(r,c) -> if r == c then k else xs!!(n*(r-1 - (if r>c then 1 else 0))+(c-1))) | xs <- listsLengthNFrom (n * (n-1)) [0..2]]

-- | https://en.wikipedia.org/wiki/Directed_acyclic_graph#Combinatorial_enumeration
labelledDAGs :: Int -> [Graph (Int, Int, Int) Int]
--labelledDAGs n = map fromAdjacencyMatrix (map (flip (-) (identity n)) (filter (eigenvaluesRealPositive . fmap toRational) (logicalMatricesConstantDiagonal 0 n)))
labelledDAGs n = map fromAdjacencyMatrix (filter isNilpotent (logicalMatricesConstantDiagonal 0 n))

dags :: Int -> [Graph (Int, Int, Int) Int]
dags = nubGraphs . labelledDAGs

-- | Converts an adjacency matrix to a graph
fromAdjacencyMatrix :: Integral a => Matrix a -> Graph (Int, Int, Int) Int
fromAdjacencyMatrix m = Graph (S.fromList [1..ncols m]) (M.fromList [((k,r,c),(r,c)) | r <- [1..nrows m], c <- [1..ncols m], k <- [1..(fromIntegral (m ! (r,c)))]])

-- | The graph obtained by reversing all edges.
transposeGraph :: Ord t => Graph e t -> Graph e t
transposeGraph g = updateEdges (M.map (\(a,b) -> (b,a))) g

graphAtSource :: Ord v => Graph e v -> v -> Graph e v
graphAtSource graph s = Graph {vertices = S.insert s (S.fromList (map snd (M.elems newEdges))), edges = newEdges}
 where newEdges = M.filter ((s ==) . fst) (edges graph)

graphAtRange :: Ord v => Graph e v -> v -> Graph e v
graphAtRange graph r = Graph {vertices = S.insert r (S.fromList (map fst (M.elems newEdges))), edges = newEdges}
 where newEdges = M.filter ((r ==) . snd) (edges graph)

graphRemoveVertex :: Ord a => Graph e a -> a -> Graph e a
graphRemoveVertex graph v =
  let newGraph = updateEdges (M.filter (\(a,b) -> a /= v && b /= v)) graph
  in newGraph {vertices = S.delete v (vertices newGraph)}

isSink :: Ord v => Graph k v -> v -> Bool
isSink graph v = M.null (edges (graphAtSource graph v))

isSource :: Ord v => Graph k v -> v -> Bool
isSource graph v = M.null (edges (graphAtRange graph v))

sinks :: Ord v => Graph e v -> S.Set v
sinks g = S.filter (isSink g) (vertices g)

sources :: Ord v => Graph e v -> S.Set v
sources g = S.filter (isSource g) (vertices g)

graphRestricted :: Ord v => Graph e v -> S.Set v -> Graph e v
graphRestricted graph vertices = Graph {vertices = vertices, edges = newEdges}
 where newEdges = M.filter (\(u,v) -> S.member u vertices && S.member v vertices) (edges graph)

-- finds all paths in a graph of length n starting at a vertex
pathsFrom :: (Ord u, Ord t) => Graph t u -> Int -> u -> S.Set [t]
pathsFrom _     0 vertex = S.singleton []
pathsFrom graph n vertex = S.unions [S.map (e :) (pathsFrom graph (n-1) (snd ((edges graph) M.! e))) | e <- M.keys (edges (graphAtSource graph vertex))]

-- finds all paths in a graph of length n
paths :: (Ord v, Ord e) => Graph e v -> Int -> S.Set [e]
paths graph n = S.unions (map (pathsFrom graph n) (S.toList $ vertices graph))

-- | Construct the graph whose Leavitt Path Algebra is isomorphic to the Cohn algebra of the original
-- | Cohn algebras always have the IBN property
cohnGraph :: (Ord e, Ord a) => Graph e a -> Graph (e, Bool) (a, Bool)
cohnGraph g = Graph v' e'
 where
  v' = (S.map (flip (,) False) (vertices g)) `S.union` (S.map (flip (,) True) (vertices g))
  e' = (M.fromList [((e,False),((x,False),(y,False))) | (e,(x,y)) <- M.toList (edges g)]) `M.union` (M.fromList [((e,True),((x,False),(y,True))) | (e,(x,y)) <- M.toList (edges g)])

-- | determines if the given graph is acyclic (contains no cycles)
-- | a directed graph is acyclic iff the adjacency matrix is nilpotent
isAcyclic :: Eq t => Graph k t -> Bool
isAcyclic graph = isNilpotent (adjacencyMatrix graph)

outNeighbours :: Ord v => Graph e v -> v -> S.Set v
outNeighbours g v = S.delete v (vertices (graphAtSource g v))

inNeighbours :: Ord v => Graph e v -> v -> S.Set v
inNeighbours g v = S.delete v (vertices (graphAtRange g v))

-- finds all vertices reachable from the given vertex
reachableVerticesFrom :: Ord a => a -> Graph e a -> S.Set a
reachableVerticesFrom v graph = reachableVerticesFrom' S.empty (S.singleton v) graph
 where
  reachableVerticesFrom' us vs _ | S.null vs = us
  reachableVerticesFrom' us vs graph = reachableVerticesFrom' (S.union us vs) ws (S.fold (flip graphRemoveVertex) graph vs)
   where ws = S.unions [outNeighbours graph v | v <- S.toList vs]

-- | Convert graph to map of vertices to other vertices.
vertexForm :: Ord k => Graph e k -> M.Map k [k]
vertexForm graph = M.fromList $ zip vs (map (map snd . M.elems . edges . graphAtSource graph) vs)
 where vs = S.toList (vertices graph)

adjacencyMatrix :: (Num a, Eq t) => Graph k t -> Matrix a
adjacencyMatrix graph = matrix (length vs) (length vs) (\(x,y) -> fromIntegral $ length $ filter ((vs!!(x-1),vs!!(y-1)) ==) (M.elems (edges graph)))
 where vs = S.toList (vertices graph)

cycleGraph :: Int -> Graph String Int
cycleGraph n = buildGraphFromEdges [("f" ++ show i,(i, 1 + mod i n)) | i <- [1..n]]

pathGraph :: Int -> Graph String Int
pathGraph n = buildGraphFromEdges [("f" ++ show i,(i, i + 1)) | i <- [1..n-1]]

listsLengthNFrom :: Int -> [t] -> [[t]]
listsLengthNFrom 0 _ = [[]]
listsLengthNFrom _ [] = []
listsLengthNFrom n xs = concat [map (x:) (listsLengthNFrom (n-1) xs) | x <- xs]

-- http://users.cecs.anu.edu.au/~bdm/data/digraphs.html
tournaments :: Int -> [Graph (Int, Int) Int]
tournaments 2 = [buildGraphFromEdges [((2,1),(1,2))]]
tournaments n = nubGraphs [updateEdges (extend n ds) t | t <- tournaments (n-1), ds <- listsLengthNFrom (n-1) [True,False]]
 where extend n ds es = M.union (M.fromList [((n,k),if d then (n,k) else (k,n)) | (k,d) <- zip [1..] ds]) es

-- | Remove duplicates up to graph isomorphism
nubGraphs :: Ord k => [Graph e k] -> [Graph e k]
nubGraphs gs = concat $ map (nubBy isomorphic) $ groupBy (on (==) invariants) $ sortBy (on compare invariants) gs

-- | This is a graph invariant
degreeSequence :: Ord v => Graph k v -> [(Int, Int)]
degreeSequence graph = sort [(inDegree graph v,outDegree graph v) | v <- S.toList (vertices graph)]

inDegree :: Ord v => Graph k v -> v -> Int
inDegree graph v = M.size (edges (graphAtRange graph v))

outDegree :: Ord v => Graph k v -> v -> Int
outDegree graph v = M.size (edges (graphAtSource graph v))

graphCharacteristicPolynomial :: Eq t => Graph k t -> Polynomial Integer
graphCharacteristicPolynomial graph = toPrimitive (characteristicPolynomial (adjacencyMatrix graph))

pathSequence :: (Ord a, Num a, Eq b) => Int -> Graph e b -> [a]
--pathSequence n g = sort [length (pathsFrom g n v) | v <- vertices g]
pathSequence n g = sort [sum ((content a)!!k) | (k,v) <- zip [0..] (S.toList (vertices g))]
 where a = (adjacencyMatrix g)^n

sccSequence :: Ord a => Graph e a -> [Int]
sccSequence g = sort $ map length $ sccs g

-- | The range index of a vertex.
-- | This is only gauranteed to be finite if the graph is a DAG. This condition is not checked, however.
rangeIndex :: (Num a, Eq t, Eq a) => Graph k t -> t -> a
rangeIndex dag v = sum [sum ((content (transpose m))!!k) | m <- takeWhile (not . isZero) (iterate (adj *) (identity (ncols adj)))]
 where
  adj = adjacencyMatrix dag
  Just k = findIndex (v ==) (S.toList (vertices dag))

-- | This list gives the range indices for all sinks in the graph.
-- | The graph is assumed to be a DAG, but this condition is not checked.
-- | These numbers allow one to classify Leavitt Path Algebras for DAGs.
sinkRangeIndices :: (Ord t, Ord a, Num a) => Graph k t -> [a]
sinkRangeIndices dag = sort [rangeIndex dag v | v <- S.toList (sinks dag)]

-- | This is a graph invariant
condensationSinkRangeIndices :: (Ord t, Ord k, Ord a, Num a) => Graph k t -> [a]
condensationSinkRangeIndices = sinkRangeIndices . condensation

-- | A tuple of graph invariants
invariants g = (order g,size g,sccSequence g,pathSequence 3 g,graphCharacteristicPolynomial g)

-- | Counts the occurences of x in xs
count :: Eq a => a -> [a] -> Int
count x xs = length (filter (x ==) xs)

isIsomorphism :: (Ord e, Eq t) => Graph k e -> Graph j t -> M.Map e t -> Bool
isIsomorphism g h iso = and [((a,b) `count` (M.elems (edges g))) == ((iso M.! a,iso M.! b) `count` (M.elems (edges h))) | a <- vs, b <- vs]
 where vs = S.toList (vertices g)

-- | brute force search for an isomorphism
isomorphic :: (Ord k, Eq a) => Graph e k -> Graph e1 a -> Bool
isomorphic g h = (not . null) (filter (isIsomorphism g h) [M.fromList (zip p (S.toList (vertices h))) | p <- permutations (S.toList (vertices g))])

-- | Quadratic time function for finding a Hamiltonian path in a DAG, if it exists
-- | Could easily be made linear, but not worth the effort.

something :: S.Set c -> c
something set = (head . S.toList) set

dagHamiltonianPath :: Ord t => Graph e t -> Maybe [t]
dagHamiltonianPath dag = return . (++ [something (sinks dag)]) =<< dagHamiltonianPath' dag
 where
  dagHamiltonianPath' dag
   | null (vertices dag) = Just []
   | length (sinks dag) == 1 && length (sources dag) == 1 =
     let src = something (sources dag)
     in return . (src :) =<< dagHamiltonianPath' (graphRemoveVertex dag src)
   | otherwise = Nothing

dagHasHamiltonianPath :: Ord t => Graph e t -> Bool
dagHasHamiltonianPath = isJust . dagHamiltonianPath

-- | Connectivity
-- | https://en.wikipedia.org/wiki/Connectivity_(graph_theory)#Definitions_of_components.2C_cuts_and_connectivity
weaklyConnected :: (Ord a, Ord t) => Graph a t -> Bool
weaklyConnected g = length (sccs (makeUndirected g)) == 1

-- | Is this correct? I made up this function myself but I think it is right.
connected :: (Ord t, Ord e) => Graph e t -> Bool
connected = dagHasHamiltonianPath . condensation

stronglyConnected :: Ord t => Graph e t -> Bool
stronglyConnected g = length (sccs g) == 1

makeUndirected :: (Ord a, Ord t) => Graph a t -> Graph (a, Bool) t
makeUndirected graph = updateEdges (\g -> M.union (M.fromList [((e,True),(b,a)) | (e,(a,b)) <- M.assocs g, (b,a) `notElem` (M.elems g)]) (M.mapKeys (flip (,) False) g)) graph

-- | This gives the acyclic graph that results from contracting all SCCs to supervertices
-- | https://en.wikipedia.org/wiki/Strongly_connected_component
condensation :: (Ord t, Ord e) => Graph e t -> Graph e [t]
condensation g = (buildGraphFromEdges [(e,(cs,ds)) | cs <- css, ds <- css, cs /= ds, e <- edgesBetweenComponents g cs ds]) {vertices = S.fromList css}
 where
  css = sccs g
  edgesBetweenComponents g cs ds = concat [M.keys (M.filter ((c,d) ==) (edges g)) | c <- cs, d <- ds]

-- | Strongly connected components
-- | https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
sccs :: Ord t => Graph e t -> [[t]]
sccs g = [[v | v <- S.toList (vertices g), assignedResult M.! v == root] | root <- nub (M.elems assignedResult)]
 where assignedResult = kosaraju g

kosaraju :: Ord a => Graph e a -> M.Map a a
kosaraju g = foldl (\assigned u -> assign g u assigned u) M.empty l
 where
  vs = S.toList (vertices g)
  visited0 = M.fromList [(u,False) | u <- vs]
  (_,l) = foldl (visit g) (visited0,[]) vs

visit g (visited,l) u
  | visited M.! u = (visited,l)
  | otherwise = fmap (u :) (foldl (visit g) (M.insert u True visited,l) (outNeighbours g u))

assign g root assigned u
  | M.member u assigned = assigned
  | otherwise = foldl (assign g root) (M.insert u root assigned) (inNeighbours g u)

randomGraph :: Int -> Int -> Float -> Graph String Int
randomGraph seed n p = Graph (S.fromList [1..n]) (M.fromList $ zip ["e"++show k | k <- [1..]] [v | (k,v) <- zip (randomRs (0.0,1.0) (mkStdGen seed)) [(x,y) | x <- [1..n], y <- [1..n], x /= y], k < p])

randomHamiltonianGraph :: Int -> Int -> Float -> Graph String Int
randomHamiltonianGraph seed n p = graphUnion (cycleGraph n) (randomGraph seed n p)

-- creates a GraphViz .dot file string for the graph
dotString :: (Show a, Show e, Ord a, Ord e) => Graph e a -> String
dotString graph = "digraph {\n" ++ unlines ([let (u,v) = ((edges graph) M.! e) in " " ++ (show.show) u ++ " -> " ++ (show.show) v ++ " [ label=" ++ (show.(" " ++).(++ " ").show) e ++ " ]" | e <- M.keys (edges graph)] ++ [" " ++ (show.show) u | (u,[]) <- M.assocs (vertexForm graph)]) ++ "}\n"

visualiseGraph :: (Show a, Show e, Ord a, Ord e) => Graph e a -> IO ()
visualiseGraph graph = writeFile "test.dot" (dotString graph) >> callCommand "dot test.dot -Tpng > test.png; open test.png"

weightedDotString :: (Show a, Show e, Ord e, Ord a) => WeightedGraph e a -> String
weightedDotString (WeightedGraph graph weighting) = "digraph {\n" ++ unlines ([let (u,v) = ((edges graph) M.! e) in " " ++ (show.show) u ++ " -> " ++ (show.show) v ++ " [ label=" ++ (show.(" " ++).(++ " ").show) (e,(weighting M.! e)) ++ " ]" | e <- M.keys (edges graph)] ++ [" " ++ (show.show) u | (u,[]) <- M.assocs (vertexForm graph)]) ++ "}\n"

visualiseWeightedGraph :: (Ord a, Ord e, Show e, Show a) => WeightedGraph e a -> IO ()
visualiseWeightedGraph weightedGraph = writeFile "test.dot" (weightedDotString weightedGraph) >> callCommand "dot test.dot -Tpng > test.png; open test.png"
