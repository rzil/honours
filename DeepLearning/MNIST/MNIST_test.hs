module Main where

import MNIST
import Data.Array
import System.Process

-- JuicyPixel package
import Codec.Picture

imageFromData :: Pixel px => Int -> Int -> [px] -> Image px
imageFromData rows columns imageData = generateImage (\x y -> imageDataUnpacked ! (y*columns + x)) columns rows
 where
  imageByteCount = rows * columns
  imageDataUnpacked = listArray (0,imageByteCount-1) imageData

test :: Int -> IO ()
test idx = do
  downloadData
  (rows,columns,labelledData) <- readTrainingData
  let images = map (imageFromData rows columns . fst) labelledData
  let labels = map snd labelledData
  writePng "test.png" (images !! idx)
  readProcess "open" ["test.png"] ""
  print (labels !! idx)

main = do
  test 1235
