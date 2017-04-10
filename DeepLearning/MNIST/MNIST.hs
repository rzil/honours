{-
This module provides some helper functions for downloading, reading, and parsing
the data from the MNIST handwritten digits database.

Author: Ruben Zilibowitz
Date: 10/4/17
-}

module MNIST(downloadData,readTrainingData,readTestData) where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Array
import Data.List.Split (splitOn)
import System.Process
import System.Directory
import Control.Exception
import Codec.Picture

--
-- Deserialising image data
--

imagesFileMagicNumber = 2051

deserialiseImagesHeader :: Get (Word32, Word32, Word32)
deserialiseImagesHeader = do
  magicNumber <- getWord32be
  numberOfImages <- getWord32be
  numberOfRows <- getWord32be
  numberOfColumns <- getWord32be
  return (assert (magicNumber == imagesFileMagicNumber) (numberOfImages,numberOfRows,numberOfColumns))

deserialiseImageData :: Int -> Int -> Get [Word8]
deserialiseImageData rows columns = do
  let imageByteCount = rows * columns
  imageData <- getLazyByteString (fromIntegral imageByteCount)
  return (BL.unpack imageData)

deserialiseAllImageData :: Get (Int, Int, [[Word8]])
deserialiseAllImageData = do
  (numberOfImages,numberOfRows,numberOfColumns) <- deserialiseImagesHeader
  let getImageData = deserialiseImageData (fromIntegral numberOfRows) (fromIntegral numberOfColumns)
  imageData <- sequence (take (fromIntegral numberOfImages) (repeat getImageData))
  return (fromIntegral numberOfRows,fromIntegral numberOfColumns,imageData)

imageFromData :: Pixel px => Int -> Int -> [px] -> Image px
imageFromData rows columns imageData = generateImage (\x y -> imageDataUnpacked ! (y*columns + x)) columns rows
 where
  imageByteCount = rows * columns
  imageDataUnpacked = listArray (0,imageByteCount-1) imageData

deserialiseAllImages :: Get [Image Word8]
deserialiseAllImages = do
  (rows,columns,imageData) <- deserialiseAllImageData
  return (map (imageFromData rows columns) imageData)

--
-- Deserialising label data
--

labelsFileMagicNumber = 2049

deserialiseLabelsHeader :: Get Word32
deserialiseLabelsHeader = do
  magicNumber <- getWord32be
  numberOfLabels <- getWord32be
  return (assert (magicNumber == labelsFileMagicNumber) numberOfLabels)

deserialiseAllLabels :: Get [Word8]
deserialiseAllLabels = do
  numberOfLabels <- deserialiseLabelsHeader
  labelsData <- getLazyByteString (fromIntegral numberOfLabels)
  let labels = BL.unpack labelsData
  return labels

--
-- THE MNIST DATABASE
-- http://yann.lecun.com/exdb/mnist/
--

urlFileName :: String -> String
urlFileName url = last (splitOn "/" url)

trainingSetImagesURL = "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz"
trainingSetLabelsURL = "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz"
testSetImagesURL = "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz"
testSetLabelsURL = "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz"

downloadIfNeeded :: String -> IO ()
downloadIfNeeded url = do
  let fileName = urlFileName url
  fileExists <- doesFileExist fileName
  if fileExists
  then return ()
  else do
    putStrLn (">>> Downloading " ++ fileName)
    readProcess "wget" [url] ""
    return ()

unzipIfNeeded :: String -> IO ()
unzipIfNeeded gzipFileName = do
  fileExists <- doesFileExist (takeWhile ('.' /=) gzipFileName)
  if fileExists
  then return ()
  else do
    putStrLn ">>> Unzipping"
    readProcess "gunzip" ["-k", gzipFileName] ""
    return ()

downloadAndUnzipIfNeeded :: String -> IO ()
downloadAndUnzipIfNeeded url = do
  downloadIfNeeded url
  unzipIfNeeded (urlFileName url)

--
-- MNIST files
--

urlFileNameUnzipped :: String -> String
urlFileNameUnzipped url = takeWhile ('.' /=) (urlFileName url)

trainingImagesFileName = urlFileNameUnzipped trainingSetImagesURL
trainingLabelsFileName = urlFileNameUnzipped trainingSetLabelsURL
testImagesFileName = urlFileNameUnzipped testSetImagesURL
testLabelsFileName = urlFileNameUnzipped testSetLabelsURL

--
-- Functions exported by this module
--

-- downloads the MNIST files and unzips them if they do not exist within current directory
downloadData :: IO ()
downloadData = sequence_ (map downloadAndUnzipIfNeeded [trainingSetImagesURL,trainingSetLabelsURL,testSetImagesURL,testSetLabelsURL])

-- reads the MNIST training data and returns it
readTrainingData :: IO (Int, Int, [([Word8], Int)])
readTrainingData = do
  trainingImagesFile <- BL.readFile trainingImagesFileName
  let (rows,columns,imageData) = runGet deserialiseAllImageData trainingImagesFile
  trainingLabelsFile <- BL.readFile trainingLabelsFileName
  let labels = map fromIntegral (runGet deserialiseAllLabels trainingLabelsFile)
  let imageLabelPairs = zip imageData labels
  return (rows,columns,imageLabelPairs)

-- reads the MNIST test data and returns it
readTestData :: IO (Int, Int, [([Word8], Int)])
readTestData = do
  testImagesFile <- BL.readFile testImagesFileName
  let (rows,columns,imageData) = runGet deserialiseAllImageData testImagesFile
  testLabelsFile <- BL.readFile testLabelsFileName
  let labels = map fromIntegral (runGet deserialiseAllLabels testLabelsFile)
  let imageLabelPairs = zip imageData labels
  return (rows,columns,imageLabelPairs)

--
-- A test function
--

test :: Int -> IO ()
test idx = do
  downloadData
  trainingImagesFile <- BL.readFile trainingImagesFileName
  let images = runGet deserialiseAllImages trainingImagesFile
  trainingLabelsFile <- BL.readFile trainingLabelsFileName
  let labels = map fromIntegral (runGet deserialiseAllLabels trainingLabelsFile)
  
  let imageLabelPairs = zip labels images
  
  writePng "test.png" (images !! idx)
  readProcess "open" ["test.png"] ""
  print (labels !! idx)
