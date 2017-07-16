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
import Graphics.Text.TrueType( loadFontFile )
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import Debug.Trace

testFile = "/Users/ruben/Documents/thirdparty/darknet/airplane-formation-boxes.txt"
imagesDirectory = "/Users/ruben/Documents/thirdparty/darknet/airplane-formation/"
lineWidth = 10

drawBoxes n imageFilename boxes = do
  putStrLn ("Image " ++ show n)
  (Right dynamicImage) <- readImage (imagesDirectory ++ imageFilename)
  (Right font) <- loadFontFile "DejaVuSans.ttf"
  let currentImage = convertRGBA8 dynamicImage
  let white = PixelRGBA8 255 255 255 255
      img = renderDrawing (imageWidth currentImage) (imageHeight currentImage) white $ do
        drawImageAtSize currentImage 0 (V2 0 0) (fromIntegral (imageWidth currentImage)) (fromIntegral (imageHeight currentImage))
        sequence_ [traceShow (label,(left,right,up,down),kind) $ let (RGB r g b) = fmap (floor . (* 255)) $ hsl (fromIntegral (label*5)) 0.54 0.5
                   in withTexture (uniformTexture (PixelRGBA8 r g b 255)) $ do
                       stroke lineWidth JoinRound (CapRound, CapRound) $ rectangle (V2 left up) (right-left) (down-up)
                       printTextAt font (PointSize 12) (V2 (left+12) (up+24)) (show label ++ " " ++ kind) | Box left right up down (Just kind) (Just label) <- boxes]
  
  writePng ("output/" ++ show n ++ ".png") img

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
