{-
Requirements: youtube-dl must be installed
Run this program from the directory where you want to download all the videos.
Pass in the csv file containing all the video ids.
This will download all the videos sequentially, skipping over videos that
have already been downloaded.
-}
module Main where

import Text.CSV
import Data.List (group)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import System.FilePath.Glob (namesMatching)
import System.Exit
import System.IO (hFlush, stdout)

person_class_id :: String
person_class_id = "0"

fastNub :: [String] -> [String]
fastNub = map head . group

csv_class_id :: Show t => [t] -> t
csv_class_id [youtube_id,timestamp_ms,class_id,class_name,object_id,object_presence,xmin,xmax,ymin,ymax] = class_id
csv_class_id err = error (show err)

extractVideoNames :: FilePath -> IO [String]
extractVideoNames fname = do {Right csv <- parseCSVFromFile fname; return (fastNub $ map head (filter ((person_class_id ==) . csv_class_id) csv))}

main :: IO ()
main = do
   (fname:_) <- getArgs
   videoNames <- extractVideoNames fname
   sequence_ [
    do
     ns <- namesMatching (vidName++".*")
     if null ns
     then do
      putStr vidName
      hFlush stdout
      (exitCode,_,_) <- readProcessWithExitCode "youtube-dl" ("--prefer-free-formats":"-k":"--id":vidName:[]) []
      if exitCode == ExitSuccess then putStrLn "." else putStrLn " failed"
     else putStrLn ("skipping " ++ vidName) | vidName <- videoNames]
