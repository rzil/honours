module Main where

import Data.List.Split (splitWhen, splitOn, chunksOf)
import Data.List (isPrefixOf, isSuffixOf, sortBy)
import Data.Function (on)
import System.Directory (listDirectory)
import Types
import BoxTrack

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Codec.Picture
import Codec.Picture.Canvas
import qualified Codec.Picture.Types as M
import Debug.Trace

testFile = "/Users/ruben/Documents/thirdparty/darknet/airplane-formation-boxes.txt"
imagesDirectory = "/Users/ruben/Documents/thirdparty/darknet/airplane-formation/"

drawRectangleLineWidth :: Pixel t => Int -> Int -> Int -> Int -> Int -> t -> Canvas t -> Canvas t
drawRectangleLineWidth 0 _ _ _ _ _ c = c
drawRectangleLineWidth lw l u w h p c = drawRectangle l u w h p (drawRectangleLineWidth (lw-1) (l+1) (u+1) (w-2) (h-2) p c)

drawBoxes n imageFilename boxes = do
  putStrLn ("Image " ++ show n)
  eimg <- readImage (imagesDirectory ++ imageFilename)
  case eimg of
    Left err -> error ("Could not read image: " ++ err)
    Right dynamicImage -> do
      let img = convertRGB8 dynamicImage
      case imageToCanvas img of
         Left err -> error ("Could not create canvas: " ++ err)
         Right canvas -> do
            let can = foldr ($) canvas [let (RGB r g b) = fmap (floor . (* 255)) $ hsl (fromIntegral (label*5)) 0.54 0.5 in traceShow (left,right,up,down) $ drawRectangleLineWidth 3 (round left) (round up) (round (right-left)) (round (down-up)) (PixelRGB8 r g b) | Box left right up down kind (Just label) <- boxes]
            let image = canvasToImage can
            savePngImage ("output/" ++ show n ++ ".png") (ImageRGB8 image)

filenameParse name = read (takeWhile (/= '.') (drop 6 name)) :: Int

main = do
  images <- return . sortBy (on compare filenameParse) . filter (isSuffixOf "jpg") =<< (listDirectory imagesDirectory)
  f <- readFile testFile
  let boxes = accumulate1 groupAndLabelBoxes (parseBoxes f)
  sequence_ (drop 150 (zipWith3 drawBoxes [0..] images boxes))

parseBox :: [String] -> Box
parseBox [name,coords] = defaultBox {left = l, right = r, up = u, down = d, kind = Just (takeWhile (/= ':') name)}
 where [l,r,u,d] = map read (splitOn "," coords)
parseBox huh = error $ show huh

parseBoxes :: String -> [[Box]]
parseBoxes string = map (map parseBox . chunksOf 2) (splitWhen (isPrefixOf "Enter Image Path:") (lines string))

accumulate :: (u -> t -> u) -> u -> [t] -> [u]
accumulate _ x [] = [x]
accumulate f x (y:ys) = x : accumulate f (f x y) ys

accumulate1 :: (t -> t -> t) -> [t] -> [t]
accumulate1 f (x:xs) = accumulate f x xs
