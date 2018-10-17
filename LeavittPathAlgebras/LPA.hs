-- Leavitt Path Algebras
module LPA where

import Graph
import Data.Maybe
import Data.List (sort,groupBy,intercalate,maximumBy,nub,group,subsequences,elemIndex)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import FiniteFields
import Data.Function (on)

data AtomType edge vertex = AEdge edge | AGhostEdge edge | AVertex vertex   deriving Show
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
printTerm (Atom k (AEdge e)) = if k == 1 then (present e) else (present k) ++ "." ++ (present e)
printTerm (Atom k (AGhostEdge g)) = (if k == 1 then (present g) else (present k) ++ "." ++ (present g)) ++ "*"
printTerm (Atom k (AVertex v)) = if k == 1 then (present v) else (present k) ++ "." ++ (present v)
printTerm Zero = "0"

instance (Show e, Show v, Show k, Eq k, Num k) => Show (Term e v k) where
   show = printTerm

data NormalFormAtom edge vertex k = NormalFormAtom {normalFormAtomCoefficient :: k, normalFormAtomVertex :: vertex, normalFormAtomPath :: [edge], normalFormAtomGhostPath :: [edge]}

instance (Show e, Show v, Show k, Eq k, Num k) => Show (NormalFormAtom e v k) where
   show = printTerm . convertTerm

instance (Eq e, Eq v, Eq k) => Eq (NormalFormAtom e v k) where
   NormalFormAtom a b c d == NormalFormAtom w x y z = (b,c,d,a) == (x,y,z,w)

instance (Ord e, Ord v, Ord k) => Ord (NormalFormAtom e v k) where
   NormalFormAtom a b c d `compare` NormalFormAtom w x y z = (b,c,d,a) `compare` (x,y,z,w)

type NormalForm edge vertex k = [NormalFormAtom edge vertex k]

adjoint :: Term e v k -> Term e v k
adjoint (Op Add u v) = Op Add (adjoint u) (adjoint v)
adjoint (Op Product u v) = Op Product (adjoint v) (adjoint u)
adjoint (Atom k (AEdge e)) = Atom k (AGhostEdge e)
adjoint (Atom k (AGhostEdge g)) = Atom k (AEdge g)
adjoint x = x

-- scalar multiplication
(*:) :: Num k => k -> Term e v k -> Term e v k
(*:) k (Op Add u v) = Op Add (k *: u) (k *: v)
(*:) k (Op Product u v) = Op Product (k *: u) v
(*:) k (Atom l a) = Atom (k * l) a
(*:) _ Zero = Zero

normalFormAtomProduct _ (NormalFormAtom c v [] []) (NormalFormAtom d u fs hs)
   | v == u = Just (NormalFormAtom (c*d) u fs hs)
   | otherwise = Nothing
normalFormAtomProduct graph (NormalFormAtom c v es []) (NormalFormAtom d u fs hs)
   | snd ((edges graph) M.! (last es)) == u = Just (NormalFormAtom (c*d) v (es ++ fs) hs)
   | otherwise = Nothing
normalFormAtomProduct graph (NormalFormAtom c v es gs) (NormalFormAtom d u [] hs)
   | fst ((edges graph) M.! (last gs)) == u = Just (NormalFormAtom (c*d) v es (gs ++ hs))
   | otherwise = Nothing
normalFormAtomProduct graph (NormalFormAtom c v es gs) (NormalFormAtom d u (f:fs) hs)
   | fst ((edges graph) M.! (last gs)) == u =
      if last gs == f
      then normalFormAtomProduct graph (NormalFormAtom c v es (init gs)) (NormalFormAtom d (snd ((edges graph) M.! (last gs))) fs hs)
      else Nothing
   | otherwise = Nothing

coefficient :: NormalFormAtom e v t -> t
coefficient (NormalFormAtom c _ _ _) = c

convertNormalForm graph term = map combine (groupBy (on (==) dropCoefficient) (sort (convertNormalForm' term)))
 where
  dropCoefficient (NormalFormAtom _ v es gs) = (v,es,gs)
  combine (NormalFormAtom c v es gs : ts) = NormalFormAtom (c + sum (map coefficient ts)) v es gs
  
  convertNormalForm' (Op Add u v) = (convertNormalForm' u) ++ (convertNormalForm' v)
  convertNormalForm' (Op Product u v) = catMaybes [normalFormAtomProduct graph x y | x <- convertNormalForm' u, y <- convertNormalForm' v]
  convertNormalForm' (Atom c (AVertex v)) = [NormalFormAtom c v [] []]
  convertNormalForm' (Atom c (AEdge e)) = [NormalFormAtom c (fst ((edges graph) M.! e)) [e] []]
  convertNormalForm' (Atom c (AGhostEdge e)) = [NormalFormAtom c (snd ((edges graph) M.! e)) [] [e]]
  convertNormalForm' Zero = []

vertex n = AVertex n
edge e = AEdge e
ghostEdge g = AGhostEdge g
atom a = Atom 1 a

-- LVP Basis
-- following http://arxiv.org/abs/1204.5258v1
type Basis edge vertex k = NormalForm edge vertex k

convertTerm :: Num k => NormalFormAtom e v k -> Term e v k
convertTerm (NormalFormAtom c v es gs) = foldl1 (*) ((Atom c (vertex v)) : ((map (atom . edge) es) ++ (map (atom . ghostEdge) gs)))

isSpecialEdge graph edge = let (u,v) = (edges graph) M.! edge in head (M.keys (edges (graphAtSource graph u))) == edge
 where (u,_) = (edges graph) M.! edge

convertBasisTerm _ t@(NormalFormAtom c v [] gs) = convertTerm t
convertBasisTerm _ t@(NormalFormAtom c v es []) = convertTerm t
convertBasisTerm graph t@(NormalFormAtom c v es gs)
   | (last es) == (head gs) && isSpecialEdge graph (head gs) =
      let g = head gs
          a = fst ((edges graph) M.! g)
          expansion = foldl1 (+) ((atom$vertex a) : [negate ((atom$edge e) * (atom$ghostEdge e)) | e <- M.keys (edges (graphAtSource graph a)), e /= g])
      in foldl1 (*) ((Atom c (vertex v)) : ((map (atom . edge) (init es)) ++ [expansion] ++ (map (atom . ghostEdge) (tail gs))))
   | otherwise = convertTerm t

isBasisTerm _ (NormalFormAtom _ _ [] _) = True
isBasisTerm _ (NormalFormAtom _ _ _ []) = True
isBasisTerm graph (NormalFormAtom _ _ es gs)
   | (last es) == (head gs) && isSpecialEdge graph (last es) = False
   | otherwise = True

isBasisForm graph = all (isBasisTerm graph)

convertBasis g = stripZeroes . convertBasis' g
 where
  convertBasis' graph normal
     | isBasisForm graph normal = normal
     | otherwise = convertBasis' graph (convertNormalForm graph (sum [convertBasisTerm graph t | t <- normal]))

stripZeroes :: (Eq k, Num k) => NormalForm e v k -> NormalForm e v k
stripZeroes [] = []
stripZeroes ((NormalFormAtom 0 _ _ _) : xs) = stripZeroes xs
stripZeroes (x : xs) = x : (stripZeroes xs)

collectNormalFormAtoms nf = filter ((/= 0) . normalFormAtomCoefficient) (map (\xs -> (head xs) {normalFormAtomCoefficient = sum (map normalFormAtomCoefficient xs)}) (groupBy (\a b -> normalFormAtomVertex a == normalFormAtomVertex b && normalFormAtomPath a == normalFormAtomPath b && normalFormAtomGhostPath a == normalFormAtomGhostPath b) (sort nf)))

convertTermToBasis graph = collectNormalFormAtoms . convertBasis graph . convertNormalForm graph

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

-- are two terms equal with respect to the given graph
equal_wrt_graph g x y = convertTermToBasis g x == convertTermToBasis g y

-- is the term an idempotent?
isIdempotent g x = equal_wrt_graph g x (x * x)

-- is the term a projection?
isProjection g x = equal_wrt_graph g x ((adjoint x) * x)

-- the identity element is the sum of all vertices
identity graph = foldl1 (+) (map (atom . vertex) (S.toList (vertices graph)))

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
