module Main where

import Data.MNIST
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
  downloadData "MNIST_Data"
  (rows,columns,labelledData) <- readTrainingData
  let images = map (imageFromData rows columns . snd) labelledData
  let labels = map fst labelledData
  writePng "test.png" (images !! idx)
  readProcess "open" ["test.png"] ""
  print (labels !! idx)

main = do
  test 1235
