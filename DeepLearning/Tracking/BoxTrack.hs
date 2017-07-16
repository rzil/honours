module BoxTrack where

import Data.List (permutations, maximumBy, sortBy, groupBy, nub, sort, sortBy, (\\))
import Data.List.Split (chunksOf)
import Data.Function (on)
import Data.Maybe (catMaybes)
import Types

-- box from centre, width, height
boxFromCentreWH x y w h = defaultBox {left = x - w/2, right = x + w/2, up = y - h/2, down = y + h/2}

-- intersection of boxes
intersect boxA boxB = boxA {left = max (left boxA) (left boxB), right = min (right boxA) (right boxB), up = max (up boxA) (up boxB), down = min (down boxA) (down boxB)}

-- area of a box
area box = (max ((right box) - (left box)) 0) * (max ((down box) - (up box)) 0)

-- area of intersection of boxes
intersectionArea boxA boxB = area (intersect boxA boxB)

-- area of union of boxes
unionArea boxA boxB = area boxA + area boxB - (intersectionArea boxA boxB)

-- intersection divided by union of boxes
iou boxA boxB = (intersectionArea boxA boxB) / (unionArea boxA boxB)

-- finds a labelling that maximises the sum of IOUs
labelBoxesBrute :: [Box] -> [Box] -> [Int]
labelBoxesBrute xs ys = snd (maximumBy (on compare fst) (zip (map (sum . (zipWith iou xs)) (permutations ys)) (permutations [0..length ys-1])))

-- delete element at index n
deleteAt :: Int -> [a] -> [a]
deleteAt n xs = take n xs ++ drop (n+1) xs

-- finds a labelling that maximises the sum of IOUs, approximately using greedy algorithm
labelBoxesGreedy :: [Box] -> [Box] -> [Int]
labelBoxesGreedy xs ys = labelBoxesGreedy' xs (zip [0..] ys)
 where
  labelBoxesGreedy' [] _ = []
  labelBoxesGreedy' (x:xs) ys = let (idx,(jdx,_)) = maximumBy (on compare (iou x . snd . snd)) (zip [0..] ys) in jdx : labelBoxesGreedy' xs (deleteAt idx ys)

-- finds a labelling that maximises the sum of IOUs, approximately using improved greedy algorithm that looks at all pairs at each step
labelBoxesGreedyPairs :: [Box] -> [Box] -> [(Int,Int)]
labelBoxesGreedyPairs xs ys = labelBoxesGreedyPairs' (zip [0..] xs) (zip [0..] ys)
 where
  labelBoxesGreedyPairs' [] _ = []
  labelBoxesGreedyPairs' _ [] = []
  labelBoxesGreedyPairs' xs ys = let (i,j,xn,yn) = snd (maximumBy (on compare fst) [(iou x y,(i,j,xn,yn)) | (i,(xn,x)) <- zip [0..] xs, (j,(yn,y)) <- zip [0..] ys]) in (xn, yn) : labelBoxesGreedyPairs' (deleteAt i xs) (deleteAt j ys)

-- attempt to label second list of boxes to maximum sum of IOUs
labelBoxes :: [Box] -> [Box] -> [Box]
labelBoxes xs ys = [let (a,b) = (xs!!i,ys!!j) in b {label = label a} | (i,j) <- permutation] ++ remain
 where
  permutation = labelBoxesGreedyPairs xs ys
  remainingYs = [0..length ys-1] \\ (map snd permutation)
  nextAvailableLabel = maximum (0 : (map (1 +) (catMaybes (map label xs))))
  remain = zipWith (\b n -> b {label = Just n}) (map (ys !!) remainingYs) [nextAvailableLabel ..]

-- groups the boxes by kind
groupBoxesByKind :: [Box] -> [(Maybe String,[Box])]
groupBoxesByKind boxes = map (\x -> ((kind . head) x,x)) (groupBy (on (==) kind) (sortBy (on compare kind) boxes))

-- lookup list element by label
obtain :: Eq a => a -> [(a, [t])] -> [t]
obtain i xs = case (lookup i xs) of
  Just x  -> x
  Nothing -> []

-- group boxes by kind, then label each group
groupAndLabelBoxes :: [Box] -> [Box] -> [Box]
groupAndLabelBoxes xs ys = concat [labelBoxes (obtain k gxs) (obtain k gys) | k <- kinds]
 where
  gxs = groupBoxesByKind xs
  gys = groupBoxesByKind ys
  kinds = nub (map fst gxs ++ map fst gys)
