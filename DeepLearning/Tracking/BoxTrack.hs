module BoxTrack where

import Data.List (permutations, maximumBy, sortBy, groupBy, nub, sort, sortBy, (\\))
import Data.List.Split (chunksOf)
import Data.Function (on)
import Data.Random.Normal
import Data.Maybe (catMaybes)
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

labelBoxesGreedyPairs :: [Box] -> [Box] -> [(Int,Int)]
labelBoxesGreedyPairs xs ys = labelBoxesGreedyPairs' (zip [0..] xs) (zip [0..] ys)
 where
  labelBoxesGreedyPairs' [] _ = []
  labelBoxesGreedyPairs' _ [] = []
  labelBoxesGreedyPairs' xs ys = let (i,j,xn,yn) = snd (maximumBy (on compare fst) [(iou x y,(i,j,xn,yn)) | (i,(xn,x)) <- zip [0..] xs, (j,(yn,y)) <- zip [0..] ys]) in (xn, yn) : labelBoxesGreedyPairs' (deleteAt i xs) (deleteAt j ys)

labelBoxes :: [Box] -> [Box] -> [Box]
labelBoxes xs ys = [let (a,b) = (xs!!i,ys!!j) in b {label = label a} | (i,j) <- permutation] ++ remain
 where
  permutation = labelBoxesGreedyPairs xs ys
  remainingYs = [0..length ys-1] \\ (map snd permutation)
  nextAvailableLabel = maximum (0 : (map (1 +) (catMaybes (map label xs))))
  remain = zipWith (\b n -> b {label = Just n}) (map (ys !!) remainingYs) [nextAvailableLabel ..]

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



bs = [Box {left = 912.0, right = 1064.0, up = 184.0, down = 291.0, kind = Just "aeroplane", label = Just 0},Box {left = 89.0, right = 302.0, up = 497.0, down = 645.0, kind = Just "aeroplane", label = Just 1},Box {left = 1040.0, right = 1205.0, up = 513.0, down = 609.0, kind = Just "aeroplane", label = Just 2},Box {left = 902.0, right = 1039.0, up = 588.0, down = 674.0, kind = Just "aeroplane", label = Just 3},Box {left = 559.0, right = 735.0, up = 768.0, down = 887.0, kind = Just "aeroplane", label = Just 4},Box {left = 716.0, right = 875.0, up = 483.0, down = 612.0, kind = Just "aeroplane", label = Just 5},Box {left = 699.0, right = 854.0, up = 161.0, down = 277.0, kind = Just "aeroplane", label = Just 6},Box {left = 717.0, right = 853.0, up = 574.0, down = 697.0, kind = Just "aeroplane", label = Just 7},Box {left = 1061.0, right = 1217.0, up = 189.0, down = 293.0, kind = Just "aeroplane", label = Just 8},Box {left = 444.0, right = 631.0, up = 187.0, down = 305.0, kind = Just "aeroplane", label = Just 9},Box {left = 459.0, right = 631.0, up = 329.0, down = 435.0, kind = Just "aeroplane", label = Just 10},Box {left = 489.0, right = 631.0, up = 501.0, down = 622.0, kind = Just "aeroplane", label = Just 11},Box {left = 554.0, right = 702.0, up = 598.0, down = 718.0, kind = Just "aeroplane", label = Just 12},Box {left = 913.0, right = 1070.0, up = 351.0, down = 458.0, kind = Just "aeroplane", label = Just 13},Box {left = 1050.0, right = 1207.0, up = 351.0, down = 438.0, kind = Just "aeroplane", label = Just 14},Box {left = 917.0, right = 1060.0, up = 507.0, down = 611.0, kind = Just "aeroplane", label = Just 15},Box {left = 671.0, right = 880.0, up = 351.0, down = 491.0, kind = Just "aeroplane", label = Just 16},Box {left = 1127.0, right = 1273.0, up = 186.0, down = 287.0, kind = Just "aeroplane", label = Just 17}]

cs = [Box {left = 925.0, right = 1073.0, up = 188.0, down = 298.0, kind = Just "aeroplane", label = Nothing},Box {left = 103.0, right = 302.0, up = 489.0, down = 626.0, kind = Just "aeroplane", label = Nothing},Box {left = 566.0, right = 744.0, up = 751.0, down = 872.0, kind = Just "aeroplane", label = Nothing},Box {left = 734.0, right = 874.0, up = 511.0, down = 623.0, kind = Just "aeroplane", label = Nothing},Box {left = 702.0, right = 856.0, up = 159.0, down = 263.0, kind = Just "aeroplane", label = Nothing},Box {left = 723.0, right = 845.0, up = 577.0, down = 687.0, kind = Just "aeroplane", label = Nothing},Box {left = 469.0, right = 635.0, up = 91.0, down = 208.0, kind = Just "aeroplane", label = Nothing},Box {left = 467.0, right = 629.0, up = 215.0, down = 322.0, kind = Just "aeroplane", label = Nothing},Box {left = 471.0, right = 653.0, up = 329.0, down = 443.0, kind = Just "aeroplane", label = Nothing},Box {left = 494.0, right = 637.0, up = 501.0, down = 599.0, kind = Just "aeroplane", label = Nothing},Box {left = 695.0, right = 858.0, up = 96.0, down = 207.0, kind = Just "aeroplane", label = Nothing},Box {left = 913.0, right = 1063.0, up = 351.0, down = 452.0, kind = Just "aeroplane", label = Nothing},Box {left = 1135.0, right = 1281.0, up = 337.0, down = 424.0, kind = Just "aeroplane", label = Nothing},Box {left = 914.0, right = 1053.0, up = 505.0, down = 605.0, kind = Just "aeroplane", label = Nothing},Box {left = 699.0, right = 883.0, up = 344.0, down = 466.0, kind = Just "aeroplane", label = Nothing},Box {left = 1117.0, right = 1301.0, up = 160.0, down = 294.0, kind = Just "aeroplane", label = Nothing}]
