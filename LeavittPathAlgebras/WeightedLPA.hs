-- Leavitt Path Algebras
module WeightedLPA where

import Graph
import Data.Maybe
import Data.List (sort,groupBy,intercalate,maximumBy,nub,group,subsequences,elemIndex)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import FiniteFields
import Data.Function (on)

data AtomType edge vertex = AEdge edge Weighting | AGhostEdge edge Weighting | AVertex vertex   deriving Show
data OpType = Add | Product
data Term edge vertex k = Op OpType (Term edge vertex k) (Term edge vertex k) | Atom k (AtomType edge vertex) | Zero

type Mat = [[(Int, Int)]]

instance (Num k) => Num (Term e v k) where
  x + y = Op Add x y
  x * y = Op Product x y
  negate (Op Add x y) = Op Add (negate x) (negate y)
  negate (Op Product x y) = Op Product (negate x) y
  negate (Atom c a) = Atom (negate c) a
  negate Zero = Zero
  abs = undefined
  signum = undefined
  fromInteger 0 = Zero
  fromInteger n = error ("can't convert integer " ++ show n)

-- http://stackoverflow.com/questions/6277747/pretty-print-expression-with-as-few-parentheses-as-possible

operatorPrecedence Add = 1
operatorPrecedence Product = 2

operatorIsRightAssociative = const False
operatorIsLeftAssociative = const False

shouldUseLeftParen (Op op (Op leftOp _ _) y) = operatorPrecedence op > operatorPrecedence leftOp || (operatorPrecedence op == operatorPrecedence leftOp && operatorIsRightAssociative op)
shouldUseLeftParen (Op op (Atom _ _) y) = False
shouldUseLeftParen _ = False

shouldUseRightParen (Op op x (Op rightOp _ _)) = operatorPrecedence op > operatorPrecedence rightOp || (operatorPrecedence op == operatorPrecedence rightOp && operatorIsLeftAssociative op)
shouldUseRightParen (Op op x (Atom _ _)) = False
shouldUseRightParen _ = False

applyParens False string = string
applyParens True string = "(" ++ string ++ ")"

opString Add = " + "
opString Product = "."

present t = case show t of
  '"' : xs | last xs == '"' -> init xs
  xs -> xs

printTerm x@(Op op l r) =
   let (useLeftParen,useRightParen) = (shouldUseLeftParen x,shouldUseRightParen x)
   in (applyParens useLeftParen (printTerm l)) ++ opString op ++ (applyParens useRightParen (printTerm r))
printTerm (Atom k (AEdge e w)) = if k == 1 then (present (e,w)) else (present k) ++ "." ++ (present (e,w))
printTerm (Atom k (AGhostEdge g w)) = (if k == 1 then (present (g,w)) else (present k) ++ "." ++ (present (g,w))) ++ "*"
printTerm (Atom k (AVertex v)) = if k == 1 then (present v) else (present k) ++ "." ++ (present v)
printTerm Zero = "0"

instance (Show e, Show v, Show k, Eq k, Num k) => Show (Term e v k) where
   show = printTerm

data NormalFormEdge edge = NormalFormEdge {normalFormEdge :: edge, normalFormEdgeIsGhost :: Bool, normalFormEdgeWeighting :: Weighting}  deriving (Eq,Ord)
data NormalFormAtom edge vertex k = NormalFormAtom k vertex [NormalFormEdge edge]

instance (Show e, Show v, Show k, Eq k, Num k) => Show (NormalFormAtom e v k) where
   show = printTerm . convertTerm

instance (Eq e, Eq v, Eq k) => Eq (NormalFormAtom e v k) where
   NormalFormAtom a b c == NormalFormAtom w x y = (b,c,a) == (x,y,w)

instance (Ord e, Ord v, Ord k) => Ord (NormalFormAtom e v k) where
   NormalFormAtom a b c `compare` NormalFormAtom w x y = (b,c,a) `compare` (x,y,w)

type NormalForm edge vertex k = [NormalFormAtom edge vertex k]

adjoint :: Term e v k -> Term e v k
adjoint (Op Add u v) = Op Add (adjoint u) (adjoint v)
adjoint (Op Product u v) = Op Product (adjoint v) (adjoint u)
adjoint (Atom k (AEdge e w)) = Atom k (AGhostEdge e w)
adjoint (Atom k (AGhostEdge g w)) = Atom k (AEdge g w)
adjoint x = x

-- scalar multiplication
(*:) :: Num k => k -> Term e v k -> Term e v k
(*:) k (Op Add u v) = Op Add (k *: u) (k *: v)
(*:) k (Op Product u v) = Op Product (k *: u) v
(*:) k (Atom l a) = Atom (k * l) a
(*:) _ Zero = Zero

normalFormAtomProduct _ (NormalFormAtom c v []) (NormalFormAtom d u fs)
   | v == u = Just (NormalFormAtom (c*d) u fs)
   | otherwise = Nothing
normalFormAtomProduct weightedGraph (NormalFormAtom c v es) (NormalFormAtom d u fs)
   | (if (normalFormEdgeIsGhost (last es)) then fst else snd) ((edges (graph weightedGraph)) M.! (normalFormEdge (last es))) == u = Just (NormalFormAtom (c*d) v (es ++ fs))
   | otherwise = Nothing

coefficient :: NormalFormAtom e v t -> t
coefficient (NormalFormAtom c _ _) = c

convertNormalForm :: (Num k, Ord edge, Ord vertex, Ord k) => WeightedGraph edge vertex -> Term edge vertex k -> [NormalFormAtom edge vertex k]
convertNormalForm weightedGraph term = map combine (groupBy (on (==) dropCoefficient) (sort (convertNormalForm' term)))
 where
  dropCoefficient (NormalFormAtom _ v es) = (v,es)
  combine (NormalFormAtom c v es : ts) = NormalFormAtom (c + sum (map coefficient ts)) v es
  
  convertNormalForm' (Op Add u v) = (convertNormalForm' u) ++ (convertNormalForm' v)
  convertNormalForm' (Op Product u v) = catMaybes [normalFormAtomProduct weightedGraph x y | x <- convertNormalForm' u, y <- convertNormalForm' v]
  convertNormalForm' (Atom c (AVertex v)) = [NormalFormAtom c v []]
  convertNormalForm' (Atom c (AEdge e w)) = [NormalFormAtom c (fst ((edges (graph weightedGraph)) M.! e)) [NormalFormEdge e False w]]
  convertNormalForm' (Atom c (AGhostEdge e w)) = [NormalFormAtom c (snd ((edges (graph weightedGraph)) M.! e)) [NormalFormEdge e True w]]
  convertNormalForm' Zero = []

vertex n = AVertex n
edge e w = AEdge e w
ghostEdge g w = AGhostEdge g w
atom a = Atom 1 a

-- wLVP Basis
-- following nod-path definition in https://arxiv.org/abs/1806.06139
type Basis edge vertex k = NormalForm edge vertex k

convertEdge :: Num k => NormalFormEdge edge -> Term edge vertex k
convertEdge (NormalFormEdge e False w) = atom (edge e w)
convertEdge (NormalFormEdge e True w) = atom (ghostEdge e w)

convertTerm :: Num k => NormalFormAtom e v k -> Term e v k
convertTerm (NormalFormAtom c v es) = foldl1 (*) ((Atom c (vertex v)) : (map convertEdge es))

isSpecialEdge :: (Ord a, Ord v) => WeightedGraph a v -> a -> Bool
isSpecialEdge weightedGraph edge =
   let (u,v) = (edges (graph weightedGraph)) M.! edge
       uWeight = vertexWeight weightedGraph u
   in head (filter (\e -> (weightings weightedGraph) M.! e == uWeight) (M.keys (edges (graphAtSource (graph weightedGraph) u)))) == edge

isForbidden :: (Ord a, Ord b) => WeightedGraph a b -> NormalFormEdge a -> NormalFormEdge a -> Bool
isForbidden weightedGraph (NormalFormEdge e True 1) (NormalFormEdge f False 1) | (fst (es M.! e) == fst (es M.! f)) = True
 where es = (edges (graph weightedGraph))
isForbidden weightedGraph (NormalFormEdge e False ew) (NormalFormEdge f True fw)
   | isSpecialEdge weightedGraph e && e == f && ew <= fw = True
isForbidden _ _ _ = False

isNodPath :: (Ord b, Ord a) => WeightedGraph a b -> NormalFormAtom a t1 t -> Bool
isNodPath _ (NormalFormAtom _ _ []) = True
isNodPath weightedGraph (NormalFormAtom _ _ es) =
   and [weightings weightedGraph M.! normalFormEdge e >= normalFormEdgeWeighting e | e <- es]
   && not (or (zipWith (isForbidden weightedGraph) es (tail es)))

isBasisForm :: (Foldable u, Ord e, Ord b) => WeightedGraph e b -> u (NormalFormAtom e v t) -> Bool
isBasisForm graph = all (isNodPath graph)

nodify :: (Ord v, Ord edge, Num k) => WeightedGraph edge v -> NormalFormAtom edge v k -> Term edge v k

nodify weightedGraph t@(NormalFormAtom c v es) | isNodPath weightedGraph t = convertTerm t

nodify weightedGraph (NormalFormAtom c v (NormalFormEdge e True 1 : NormalFormEdge f False 1 : es))
   | e == f = ((atom (vertex u)) - x) * (nodify weightedGraph (NormalFormAtom c v es))
   | otherwise = (negate x) * (nodify weightedGraph (NormalFormAtom c v es))
 where u = fst ((edges (graph weightedGraph)) M.! f)
       x = sum [(atom (ghostEdge e n)) * (atom (edge f n)) | n <- [2 .. vertexWeight weightedGraph u]]

nodify weightedGraph (NormalFormAtom c v (NormalFormEdge e False ew : NormalFormEdge f True fw : es))
   | isSpecialEdge weightedGraph e && e == f && ew <= fw =
      if ew == fw
      then ((atom (vertex u)) - x) * (nodify weightedGraph (NormalFormAtom c v es))
      else (negate x) * (nodify weightedGraph (NormalFormAtom c v es))
 where u = fst ((edges (graph weightedGraph)) M.! e)
       x = sum [(atom (edge g ew)) * (atom (ghostEdge g fw)) | g <- (M.keys (edges (graphAtSource (graph weightedGraph) u))), g /= e]

nodify weightedGraph (NormalFormAtom c v (e:es))
   | (weightings weightedGraph M.! normalFormEdge e < normalFormEdgeWeighting e) = 0
   | otherwise = (convertEdge e) * (nodify weightedGraph (NormalFormAtom c u es))
 where u = (if normalFormEdgeIsGhost e then fst else snd) (edges (graph weightedGraph) M.! normalFormEdge e)

convertToBasisForm :: (Ord vertex, Ord edge, Ord t, Num t) => WeightedGraph edge vertex -> Term edge vertex t -> [NormalFormAtom edge vertex t]
convertToBasisForm weightedGraph term = until (all (isNodPath weightedGraph)) (f weightedGraph) (convertNormalForm weightedGraph term)

f weightedGraph = concatMap ((convertNormalForm weightedGraph) . (nodify weightedGraph))

-- are two terms equal with respect to the given weighted graph
equal_wrt_graph wg x y = sort (convertToBasisForm wg x) == sort (convertToBasisForm wg y)

-- is the term an idempotent?
isIdempotent wg x = equal_wrt_graph wg x (x * x)

-- is the term a projection?
isProjection wg x = equal_wrt_graph wg x ((adjoint x) * x)

-- the identity element is the sum of all vertices
identity wg = foldl1 (+) (map (atom . vertex) (S.toList (vertices (graph wg))))

iso_example :: WeightedGraph String String
iso_example = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

{-
convertBasis g = stripZeroes . convertBasis' g
 where
  convertBasis' graph normal
     | isBasisForm graph normal = normal
     | otherwise = convertBasis' graph (convertNormalForm graph (sum [convertBasisTerm graph t | t <- normal]))

stripZeroes :: (Eq k, Num k) => NormalForm e v k -> NormalForm e v k
stripZeroes [] = []
stripZeroes ((NormalFormAtom 0 _ _ _) : xs) = stripZeroes xs
stripZeroes (x : xs) = x : (stripZeroes xs)

convertTermToBasis graph = convertBasis graph . convertNormalForm graph

-- logical implies
(==>) x y = (not x) || y

-- lists all elements in the basis for the LVP of the graph
-- If the graph is acyclic then this will be a finite list
-- otherwise it will be an infinite list.
basis graph = (Prelude.map (atom . vertex) (S.toList (vertices graph))) ++ pathsBasis
 where
  pathsBasis = Prelude.map (foldl1 (*)) $ concat $ concat [[(Prelude.map ([atom$edge e | e <- es] ++) (ghostPaths k es)) | k <- [0..n], es <- S.toList $ paths graph (n-k)] | n <- ns]
  ns = if isAcyclic graph then [1 .. 2*(length (edges graph))] else [1..]
  ghostPaths 0 _ = [[]]
  ghostPaths k [] = [[atom$ghostEdge g | g <- gs] | gs <- S.toList $ paths (transposeGraph graph) k, not (null gs)]
  ghostPaths k es =
    let e = last es
        v = snd ((edges graph) M.! e) in [[atom$ghostEdge g | g <- gs] | gs <- S.toList $ pathsFrom (transposeGraph graph) k v, not (null gs), (isSpecialEdge graph e) ==> (head gs /= e)]

vectorForm graph left right = [(fromJust $ elemIndex (NormalFormAtom 1 v es gs) bs,k) | NormalFormAtom k v es gs <- convertTermToBasis graph (left * right)]
 where
  f [x] = x
  bs = map (f . convertTermToBasis graph) (basis graph)

matrixForm graph left = map (vectorForm graph left) (basis graph)

transpose :: Mat -> Mat
transpose cs = [[(c,x) | (Just x, c) <- zip (map (Prelude.lookup r) cs) [0..]] | r <- [0..]]

showRow :: Int -> [(Int,Int)] -> String
showRow n row = intercalate "\t" [show (maybe 0 id (Prelude.lookup i row)) | i <- [0..n]]

showMat :: Int -> Mat -> IO ()
showMat n mat = putStr $ unlines (map (showRow n) (take (2*n) $ transpose $ take n mat))

-- lists all elements having coefficients from a given list
-- NB: do not include 0 in the list of coefficients
elements graph coefficients = [foldl1 (+) (zipWith (*:) cs xs) | xs <- subsequences (basis graph), not (null xs), cs <- listsLengthNFrom (length xs) coefficients]

-- lists all idempotents having coefficients from a given list
idempotents graph coefficients = Zero : Prelude.filter (isIdempotent graph) (elements graph coefficients)

-- lists all projections having coefficients from a given list
projections graph coefficients = Zero : Prelude.filter (isProjection graph) (elements graph coefficients)

-- elements having coefficients from a given list in the right annihilator of the given element
annihilatorRight g coefficients x = Prelude.filter ((equal_wrt_graph g Zero) . (x *)) (elements g coefficients)

-- Examples

n_loop_graph n = buildGraphFromEdges [("e" ++ show i,("v","v")) | i <- [1..n]]

my_graph = buildGraphFromEdges [("a",("x","y")),("b",("x","z")),("c",("x","z"))]

six_cycle_with_two_chords =
   updateEdges (M.insert "h" ("2","5")) $ updateEdges (M.insert "f" ("1","3"))
   (buildGraphFromEdges $ M.toList (M.map (uncurry $ on (,) show) (edges $ cycleGraph 6)))

two_loop_edge1_matrix = showMat 20 (matrixForm (n_loop_graph 2) (atom$edge "e1"))

three_loop_edge1_matrix = showMat 20 (matrixForm (n_loop_graph 3) (atom$edge "e1"))

toeplitz_graph = buildGraphFromEdges [("f",("v","v")),("e",("v","w"))]
-}
