module Main where

import Graph
import GraphMonoid
import LPA
import qualified Data.Set as S
import qualified Data.Map as M

u = (atom (vertex "u1")) + (atom (vertex "u2")) + (atom (vertex "u3"))
v = atom (vertex "v")
e1 = (atom (edge "e1")) + (atom (edge "e2")) + (atom (edge "e3"))
f1 = atom (edge "f")
f2 = (atom (edge "f")) * (atom (edge "g")) + (atom (edge "e1")) * (atom (ghostEdge "i")) + (atom (edge "e2")) * (atom (ghostEdge "h")) + (atom (edge "e3")) * (atom (ghostEdge "j"))

{-
a k = e1 * (adjoint e1) * (f2 * (adjoint e1))^k
b k = e1 * (adjoint f1) * (f2 * (adjoint e1))^k
c k = (e1 * (adjoint f2))^k
d k = e1 * (adjoint f2) * f1 * (adjoint e1) * (f2 * (adjoint e1))^k
e k = e1 * (adjoint f2) * (f2 * (adjoint e1))^k
f k = f1 * (adjoint e1) * (f2 * (adjoint e1))^k
g k = (f2 * (adjoint e1))^k
-}

as = [e1 * (adjoint e1), e1 * (adjoint f1), e1 * (adjoint f2) * f1 * (adjoint e1), f1 * (adjoint e1), u + v]

form n a m = (e1 * (adjoint f2))^n * a * (f2 * (adjoint e1))^m

graph = Graph (S.fromList ["u1","u2","u3","v"]) (M.fromList [("j",("u3","u3")),("i",("u3","u1")),("h",("u3","u2")),("g",("u2","u1")),("e1",("v","u1")),("e2",("v","u2")),("e3",("v","u3")),("f",("v","u2"))])

eval = convertTermToBasis graph

main = print $ eval $ form 1 (as !! 0) 1
