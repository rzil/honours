module BoxTrack where

import Data.List (permutations, maximumBy, sortBy, groupBy, nub, sort, sortBy, (\\))
import Data.List.Split (chunksOf)
import Data.Function (on)
import Data.Random.Normal
import Types

boxFromCentreWH x y w h = defaultBox {left = x - w/2, right = x + w/2, up = y - h/2, down = y + h/2}

intersect boxA boxB = boxA {left = max (left boxA) (left boxB), right = min (right boxA) (right boxB), up = max (up boxA) (up boxB), down = min (down boxA) (down boxB)}

area box = (max ((right box) - (left box)) 0) * (max ((down box) - (up box)) 0)

intersectionArea boxA boxB = area (intersect boxA boxB)

unionArea boxA boxB = area boxA + area boxB - (intersectionArea boxA boxB)

iou boxA boxB = (intersectionArea boxA boxB) / (unionArea boxA boxB)

labelBoxesBrute :: [Box] -> [Box] -> [Int]
labelBoxesBrute xs ys = snd (maximumBy (on compare fst) (zip (map (sum . (zipWith iou xs)) (permutations ys)) (permutations [0..length ys-1])))

deleteAt :: Int -> [a] -> [a]
deleteAt n xs = take n xs ++ drop (n+1) xs

labelBoxesGreedy :: [Box] -> [Box] -> [Int]
labelBoxesGreedy xs ys = labelBoxesGreedy' xs (zip [0..] ys)
 where
  labelBoxesGreedy' [] _ = []
  labelBoxesGreedy' (x:xs) ys = let (idx,(jdx,_)) = maximumBy (on compare (iou x . snd . snd)) (zip [0..] ys) in jdx : labelBoxesGreedy' xs (deleteAt idx ys)

labelBoxesGreedyPairs :: [Box] -> [Box] -> [Int]
labelBoxesGreedyPairs xs ys = map snd (sort (labelBoxesGreedyPairs' (zip [0..] xs) (zip [0..] ys)))
 where
  labelBoxesGreedyPairs' [] _ = []
  labelBoxesGreedyPairs' _ [] = []
  labelBoxesGreedyPairs' xs ys = let (i,j,xn,yn) = snd (maximumBy (on compare fst) [(iou x y,(i,j,xn,yn)) | (i,(xn,x)) <- zip [0..] xs, (j,(yn,y)) <- zip [0..] ys]) in (xn, yn) : labelBoxesGreedyPairs' (deleteAt i xs) (deleteAt j ys)

permute :: [Int] -> [b] -> [b]
permute p xs = map (xs !!) p

labelBoxes :: [Box] -> [Box] -> [Box]
labelBoxes xs ys = labelledYs ++ remain
 where
  permutation = labelBoxesGreedyPairs xs ys
  permutedYs = permute permutation ys
  labelledYs = zipWith (\y l -> y {label = l}) permutedYs (map label xs)
  remain = zipWith (\b n -> b {label = Just n}) (map (ys !!) ([0..length ys-1] \\ permutation)) [length labelledYs ..]

groupBoxesByKind :: [Box] -> [(Maybe String,[Box])]
groupBoxesByKind boxes = map (\x -> ((kind . head) x,x)) (groupBy (on (==) kind) (sortBy (on compare kind) boxes))

obtain :: Eq a => a -> [(a, [t])] -> [t]
obtain i xs = case (lookup i xs) of
  Just x  -> x
  Nothing -> []

groupAndLabelBoxes :: [Box] -> [Box] -> [Box]
groupAndLabelBoxes xs ys = concat [labelBoxes (obtain k gxs) (obtain k gys) | k <- kinds]
 where
  gxs = groupBoxesByKind xs
  gys = groupBoxesByKind ys
  kinds = nub (map fst gxs ++ map fst gys)

-----

testA = let bs = randomBoxes 4231 in labelBoxesBrute   (take 8 bs) (take 8 $ drop 8 bs)
testB = let bs = randomBoxes 4231 in labelBoxesGreedy  (take 8 bs) (take 8 $ drop 8 bs)
testC = let bs = randomBoxes 4231 in labelBoxesGreedyPairs  (take 8 bs) (take 8 $ drop 8 bs)

randomBoxes :: Int -> [Box]
randomBoxes seed = map (\[x,y,w,h] -> boxFromCentreWH x y w h) (chunksOf 4 rs)
 where rs = mkNormals' (4,1) seed :: [Coord]
