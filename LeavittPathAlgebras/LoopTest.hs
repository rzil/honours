module LoopTest where

import Data.List hiding (transpose)
import Data.Maybe

loop :: Eq a => Int -> [a] -> [[a]]
loop 0 _ = [[]]
loop _ [] = []
loop n xs = concat [map (x:) (loop (n-1) xs) | x <- xs]

loop2 n xs = [(as,bs) | k <- [0..n], as <- loop k xs, bs <- loop (n-k) xs, f as bs]
 where
  f (a:as) (b:bs) | last (a:as) == b && b == (head xs) = False
  f _ _ = True

loop3 xs = concat [loop2 n xs | n <- [0..]]

type Term = (String,String)

terms = loop3 "ab"

a :: Term -> [(Int,Term)]
a ("","") = [(1,("a",""))]
a ('a':xs,ys) = [(1,("aa"++xs,ys))]
a ('b':xs,ys) = [(1,("ab"++xs,ys))]
a ("",'a':ys) = [(1,("",ys)),(-1,("b",'b':ys))]
a ("",'b':ys) = [(1,("a",'b':ys))]

as :: Term -> [(Int,Term)]
as ("","") = [(1,("","a"))]
as ('a':xs,ys) = [(1,(xs,ys))]
as ('b':xs,ys) = []
as ("",'a':ys) = [(1,("","aa"++ys))]
as ("",'b':ys) = [(1,("","ab"++ys))]

b :: Term -> [(Int,Term)]
b ("","") = [(1,("b",""))]
b ('a':xs,ys) = [(1,("ba"++xs,ys))]
b ('b':xs,ys) = [(1,("bb"++xs,ys))]
b ("",'a':ys) = [(1,("b",'a':ys))]
b ("",'b':ys) = [(1,("b",'b':ys))]

bs :: Term -> [(Int,Term)]
bs ("","") = [(1,("","b"))]
bs ('a':xs,ys) = []
bs ('b':xs,ys) = [(1,(xs,ys))]
bs ("",'a':ys) = [(1,("","ba"++ys))]
bs ("",'b':ys) = [(1,("","bb"++ys))]

vectorForm :: (t -> [(u, Term)]) -> t -> [(Int, u)]
vectorForm f t = [(termIndex s,n) | (n,s) <- f t]

termIndex :: Term -> Int
termIndex t = fromJust $ elemIndex t terms

-- list of column vectors
-- each column vector is given as ordered pair of row numbers with entry values
type Mat = [[(Int, Int)]]

amat :: Mat
amat = map (vectorForm a) terms

asmat :: Mat
asmat = map (vectorForm as) terms

bmat :: Mat
bmat = map (vectorForm b) terms

bsmat :: Mat
bsmat = map (vectorForm bs) terms

idmat :: Mat
idmat = [[(i,1)] | i <- [0..]]

row k cs = maybe 0 id (lookup k cs)

transpose :: Mat -> Mat
transpose cs = [[(c,x) | (Just x, c) <- zip (map (lookup r) cs) [0..]] | r <- [0..]]

vectorProduct :: [(Int, Int)] -> [(Int, Int)] -> Int
vectorProduct [] _ = 0
vectorProduct ((i,x):xs) ys = (maybe 0 (x *) (lookup i ys)) + (vectorProduct xs ys)

multiply :: Mat -> Mat -> Mat
multiply rs cs = [filter ((0 /=) . snd) [(i,vectorProduct r c) | (i,r) <- zip [0..] rs] | c <- cs]

add xs ys = [filter ((0 /=) . snd) [(fst (head gr),sum (map snd gr)) | gr <- groupBy (\(a,_) (b,_) -> a == b) (sort (x++y))] | (x,y) <- zip xs ys]

multiplyN :: Int -> Mat -> Mat -> Mat
multiplyN n xs ys = multiply (take n $ transpose $ take n xs) (take n ys)

asa n = multiplyN n asmat amat

aas n = multiplyN n amat asmat

bsb n = multiplyN n bsmat bmat

bbs n = multiplyN n bmat bsmat

test1 n = take n (add (aas (n*n)) (bbs (n*n))) == (take n idmat)

test2 n = multiplyN n (aas n) (aas n) == aas n

test3 n = multiplyN n (bbs n) (bbs n) == bbs n

showRow n row = intercalate "\t" [show (maybe 0 id (lookup i row)) | i <- [0..n]]

showMat n mat = putStr $ unlines (map (showRow n) (take n $ transpose $ take n mat))

concatMat n mat = concat [[(maybe 0 id (lookup i row)) | i <- [0..n]] | row <- mat]
