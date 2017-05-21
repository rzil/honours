{-
Requirements: ffmpeg must be installed
Run this program from inside the directory with all the videos you want to extract the frames out of.
Pass in the csv file with all the video and timestamp data.
It will create a new directory inside the current one called frames. Inside there it will create
directories for each video and output the frame images to there.
-}

module Main where

import Text.CSV
import Data.List (group,intercalate)
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import System.FilePath.Glob (namesMatching)
import System.Exit
import System.IO (hFlush, stdout)
import System.Directory
import Text.Printf (printf)

{-
youtube_id - same as above.
timestamp_ms - same as above.
class_id - same as above.
class_name - same as above.
object_id - (integer) an identifier of the object in the video. (see note below)
object_presence - same as above.
xmin - (float) a [0.0, 1.0] number indicating the left-most location of the bounding box in coordinates relative to the frame size.
xmax - (float) a [0.0, 1.0] number indicating the right-most location of the bounding box in coordinates relative to the frame size.
ymin - (float) a [0.0, 1.0] number indicating the top-most location of the bounding box in coordinates relative to the frame size.
ymax - (float) a [0.0, 1.0] number indicating the bottom-most location of the bounding box in coordinates relative to the frame size.
-}

extractVideos :: FilePath -> IO ()
extractVideos fname = do
  Right csv <- parseCSVFromFile fname
  mapM_ extractFrames csv

timestampToHHMMSS :: Integer -> String
timestampToHHMMSS ms = intercalate ":" ["00",printf "%02d" m,printf "%02d" s]
 where
  m = ms `div` 60000
  s = (ms - m*60000) `div` 1000

extractFrames :: [String] -> IO ExitCode
extractFrames [youtube_id,timestamp_ms,class_id,class_name,object_id,object_presence,xmin,xmax,ymin,ymax] = do
  fnames <- namesMatching (youtube_id ++ ".f*.mp4")
  if null fnames
  then putStrLn ("Skipping " ++ youtube_id) >> hFlush stdout >> return ExitSuccess
  else do
   let fname = head fnames
   putStrLn ("Extracting " ++ fname ++ " frame " ++ timestamp_ms) >> hFlush stdout
   let dirName = "frames/" ++ fname ++ ".frames"
   createDirectoryIfMissing True dirName
   (exitCode,_,_) <- readProcessWithExitCode "ffmpeg" ["-ss",timestampToHHMMSS (read timestamp_ms),"-i",fname,"-frames:v","1",dirName ++ "/frame_" ++ timestamp_ms ++ ".bmp"] []
   return exitCode

main :: IO ()
main = do
   (fname:_) <- getArgs
   extractVideos fname
