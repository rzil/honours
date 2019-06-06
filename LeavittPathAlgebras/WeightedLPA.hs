-- Weighted Leavitt Path Algebras
module WeightedLPA where

import Graph
import Data.Maybe
import Data.List (sort,groupBy,subsequences)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function (on)

data AtomType edge vertex = AEdge edge Weighting | AGhostEdge edge Weighting | AVertex vertex   deriving Show
data OpType = Add | Product
data Term edge vertex k = Op OpType (Term edge vertex k) (Term edge vertex k) | Atom k (AtomType edge vertex) | Zero

data NormalFormEdge edge = NormalFormEdge {normalFormEdge :: edge, normalFormEdgeIsGhost :: Bool, normalFormEdgeWeighting :: Weighting}  deriving (Eq,Ord,Show)
data NormalFormAtom edge vertex k = NormalFormAtom {normalFormAtomCoefficient :: k, normalFormAtomVertex :: vertex, normalFormAtomPath :: [NormalFormEdge edge]}

instance (Show e, Show v, Show k, Eq k, Num k) => Show (NormalFormAtom e v k) where
   show = printTerm . convertTerm

instance (Eq e, Eq v, Eq k) => Eq (NormalFormAtom e v k) where
   NormalFormAtom a b c == NormalFormAtom w x y = (b,c,a) == (x,y,w)

instance (Ord e, Ord v, Ord k) => Ord (NormalFormAtom e v k) where
   NormalFormAtom a b c `compare` NormalFormAtom w x y = (b,c,a) `compare` (x,y,w)

type NormalForm edge vertex k = [NormalFormAtom edge vertex k]

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

scalarProduct :: Num k => k -> Term edge vertex k -> Term edge vertex k
scalarProduct k (Op Add u v) = Op Add (scalarProduct k u) (scalarProduct k v)
scalarProduct k (Op Product u v) = Op Product (scalarProduct k u) v
scalarProduct k (Atom l x) = Atom (k * l) x
scalarProduct _ Zero = Zero

gmap :: (Num k) => (AtomType edge1 vertex1 -> Term edge vertex k) -> Term edge1 vertex1 k -> Term edge vertex k
gmap f (Op o u v) = Op o (gmap f u) (gmap f v)
gmap f (Atom k x) = scalarProduct k (f x)
gmap _ Zero = Zero

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

convertNormalForm :: (Num k, Ord edge, Ord vertex, Ord k) => WeightedGraph edge vertex -> Term edge vertex k -> [NormalFormAtom edge vertex k]
convertNormalForm weightedGraph term = map combine (groupBy (on (==) dropCoefficient) (sort (convertNormalForm' term)))
 where
  dropCoefficient (NormalFormAtom _ v es) = (v,es)
  combine (NormalFormAtom c v es : ts) = NormalFormAtom (c + sum (map normalFormAtomCoefficient ts)) v es
  
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
   | isSpecialEdge weightedGraph e && e == f = True

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
   | e == f = ((atom (vertex v)) - x) * (nodify weightedGraph (NormalFormAtom c v es))
   | otherwise = (negate x) * (nodify weightedGraph (NormalFormAtom c v es))
 where (u,_) = ((edges (graph weightedGraph)) M.! f)
       x = sum [(atom (ghostEdge e n)) * (atom (edge f n)) | n <- [2 .. vertexWeight weightedGraph u]]

nodify weightedGraph (NormalFormAtom c v (NormalFormEdge e False ew : NormalFormEdge f True fw : es))
   | isSpecialEdge weightedGraph e && e == f =
      if ew == fw
      then ((atom (vertex u)) - x) * (nodify weightedGraph (NormalFormAtom c v es))
      else (negate x) * (nodify weightedGraph (NormalFormAtom c v es))
 where u = fst ((edges (graph weightedGraph)) M.! e)
       x = sum [(atom (edge g ew)) * (atom (ghostEdge g fw)) | g <- (M.keys (edges (graphAtSource (graph weightedGraph) u))), g /= e]

nodify weightedGraph (NormalFormAtom c v (e:es))
   | (weightings weightedGraph M.! normalFormEdge e < normalFormEdgeWeighting e) = 0
   | otherwise = (convertEdge e) * (nodify weightedGraph (NormalFormAtom c u es))
 where u = (if normalFormEdgeIsGhost e then fst else snd) (edges (graph weightedGraph) M.! normalFormEdge e)

collectNormalFormAtoms nf = filter ((/= 0) . normalFormAtomCoefficient) (map (\xs -> (head xs) {normalFormAtomCoefficient = sum (map normalFormAtomCoefficient xs)}) (groupBy (\a b -> normalFormAtomVertex a == normalFormAtomVertex b && normalFormAtomPath a == normalFormAtomPath b) (sort nf)))

convertToBasisForm :: (Ord vertex, Ord edge, Ord t, Num t) => WeightedGraph edge vertex -> Term edge vertex t -> [NormalFormAtom edge vertex t]
convertToBasisForm weightedGraph term = collectNormalFormAtoms $ until (all (isNodPath weightedGraph)) (nodifyStep weightedGraph) (convertNormalForm weightedGraph term)

nodifyStep weightedGraph = concatMap ((convertNormalForm weightedGraph) . (nodify weightedGraph))

-- are two terms equal with respect to the given weighted graph
equal_wrt_graph wg x y = sort (convertToBasisForm wg x) == sort (convertToBasisForm wg y)

-- is the term an idempotent?
isIdempotent wg x = equal_wrt_graph wg x (x * x)

-- is the term a projection?
isProjection wg x = equal_wrt_graph wg x ((adjoint x) * x)

-- the identity element is the sum of all vertices
identity wg = foldl1 (+) (map (atom . vertex) (S.toList (vertices (graph wg))))

pathToNormalForm :: (Num k, Ord t) => WeightedGraph t vertex -> [((t, Weighting), Bool)] -> NormalFormAtom t vertex k
pathToNormalForm weightedGraph p = NormalFormAtom 1 (let ((e,_),f) = head p in (let (u,v) = edges (graph weightedGraph) M.! e in if f then u else v)) [NormalFormEdge e (not f) k | ((e,k),f) <- p]

-- lists all elements in the basis for the wLPA of the graph
-- this may or may not be finite
basis :: (Ord u, Ord edge, Num t) => WeightedGraph edge u -> [NormalFormAtom edge u t]
basis weightedGraph = vs ++ concat [filter (isNodPath weightedGraph) $ [pathToNormalForm weightedGraph p | p <- S.toList $ paths (doubleGraph (directedGraphAssociatedToWeightedGraph weightedGraph)) len] | len <- [1..]]
 where vs = map (flip (NormalFormAtom 1) []) (S.toList $ vertices (graph weightedGraph))

d_v :: (Ord vertex, Ord edge) => WeightedGraph edge vertex -> Int -> Int
d_v wg n = length $ takeWhile ((n >=) . length . normalFormAtomPath) $ basis wg

-- the Gelfand-Kirillov dimension (GK dimension) is the lim sup of this function as n -> infinity
gelfandKirillov :: (Floating a, Ord edge, Ord vertex) => WeightedGraph edge vertex -> Int -> a
gelfandKirillov wg n = logBase (fromIntegral n) $ fromIntegral $ d_v wg n

isNod2Path :: (Ord t1, Ord edge, Num t) => WeightedGraph edge t1 -> NormalFormAtom edge t1 t -> Bool
isNod2Path wg p = isNodPath wg p && maybe False (isNodPath wg) (normalFormAtomProduct wg p p)

subpathsOfLength 0 _ = [[]]
subpathsOfLength k xs
   | length xs >= k = take k xs : (subpathsOfLength k (tail xs))
   | otherwise = []

edgesPathToAtom wg (p:ps) = NormalFormAtom 1 ((if normalFormEdgeIsGhost p then snd else fst) ((edges (graph wg)) M.! (normalFormEdge p))) (p:ps)

atomSubpathsOfLength wg k atom = [edgesPathToAtom wg p | p <- subpathsOfLength k (normalFormAtomPath atom)]

isQuasiCycle :: (Ord vertex, Ord edge, Num k) => WeightedGraph edge vertex -> NormalFormAtom edge vertex k -> Bool
isQuasiCycle wg p | isNod2Path wg p = not (any (isNod2Path wg) $ concat [atomSubpathsOfLength wg k p2 | k <- [1..length (normalFormAtomPath p) - 1]])
 where Just p2 = normalFormAtomProduct wg p p
isQuasiCycle _ _ = False

isSubPath _ [] = False
isSubPath xs ys
   | xs == take (length xs) ys = True
   | otherwise = isSubPath xs (tail ys)

normalFormAtomDivides p q = isSubPath (normalFormAtomPath p) (normalFormAtomPath q)

isNodArrow wg p o q | isNodPath wg p && isNodPath wg o && isNodPath wg q && not (normalFormAtomDivides p o) = maybe False id $ do
   po <- normalFormAtomProduct wg p o
   poq <- normalFormAtomProduct wg po q
   return (isNodPath wg poq)
isNodArrow _ _ _ _ = False

-- what is an upper bound for the length of a quasicycle?
quasicycles wg = filter (isQuasiCycle wg) (takeWhile ((1+n >=) . length . normalFormAtomPath) (basis wg))
 where n = S.size (vertices (graph wg))

-- lists all elements having coefficients from a given list
-- NB: do not include 0 in the list of coefficients
elements graph coefficients = [foldl1 (+) (zipWith (*:) cs xs) | xs <- subsequences (map convertTerm $ basis graph), not (null xs), cs <- listsLengthNFrom (length xs) coefficients]

-- lists all idempotents having coefficients from a given list
idempotents graph coefficients = Zero : Prelude.filter (isIdempotent graph) (elements graph coefficients)

-- lists all projections having coefficients from a given list
projections graph coefficients = Zero : Prelude.filter (isProjection graph) (elements graph coefficients)

-- elements having coefficients from a given list in the right annihilator of the given element
annihilatorRight g coefficients x = Prelude.filter ((equal_wrt_graph g Zero) . (x *)) (elements g coefficients)

showMapping graph = putStrLn $ unlines (zipWith (\a b -> a ++ " --> " ++ b) (map show non_nods) (map show non_nods_reduced))
 where
  twos = map (pathToNormalForm graph) $ S.toList $ paths (doubleGraph (directedGraphAssociatedToWeightedGraph graph)) 2
  non_nods = filter (not . isNodPath graph) twos
  non_nods_reduced = map (convertToBasisForm graph . convertTerm) non_nods

-- sum without zero
sum' [x] = x
sum' (x:xs) = x + sum' xs

wLPA_relations_map' :: (Eq vertex, Num b, Ord edge) => b -> (AtomType edge vertex -> b) -> WeightedGraph edge vertex -> [b]
wLPA_relations_map' zero f wgraph = one ++ two1 ++ two2 ++ two3 ++ two4 ++ three ++ four
 where
    vs = S.toList $ vertices (graph wgraph)
    es = M.keys $ edges (graph wgraph)
    s e = fst $ (edges (graph wgraph)) M.! e
    r e = snd $ (edges (graph wgraph)) M.! e
    w e = (weightings wgraph) M.! e
    edgesAt v = filter ((v ==) . s) es
    maxEdgeWeightAt v = maximum (0 : map w (edgesAt v))
    one = [(f$vertex v) * (f$vertex w) - (if v == w then f$vertex v else zero) | v <- vs, w <- vs]
    two1 = [(f$vertex$s e) * (f$edge e i) - (f$edge e i) | e <- es, i <- [1 .. (weightings wgraph) M.! e]]
    two2 = [(f$edge e i) * (f$vertex$r e) - (f$edge e i) | e <- es, i <- [1 .. (weightings wgraph) M.! e]]
    two3 = [(f$vertex$r e) * (f$ghostEdge e i) - (f$ghostEdge e i) | e <- es, i <- [1 .. w e]]
    two4 = [(f$ghostEdge e i) * (f$vertex$s e) - (f$ghostEdge e i) | e <- es, i <- [1 .. w e]]
    three = [sum' [(f$edge e i) * (f$ghostEdge e j) | e <- edgesAt v] - (if i == j then f$vertex v else zero) | v <- vs, i <- [1 .. maxEdgeWeightAt v], j <- [1 .. maxEdgeWeightAt v]]
    four = [let m = max (w e) (w e') in sum' [(f$ghostEdge e i) * (f$edge e' i) | i <- [1 .. m]] - (if e == e' then f$vertex$r e else zero) | e <- es, e' <- es]

wLPA_relations_map :: (Eq vertex, Num b, Ord edge) => (AtomType edge vertex -> b) -> WeightedGraph edge vertex -> [b]
wLPA_relations_map = wLPA_relations_map' (fromInteger 0)

wLPA_relations_show wg = wLPA_relations_map (Atom 1) wg

-- checks if a mapping from a wLPA to an LPA is well defined
wLPA_relations_check f wgraph rgraph = map (equal_wrt_graph rgraph Zero) (wLPA_relations_map f wgraph)

wLPA_relations_present f wgraph rgraph = do
  let check = wLPA_relations_check f wgraph rgraph
  let relations = wLPA_relations_show wgraph
  putStr $ unlines $ map show $ zip check relations
  print (and check)
