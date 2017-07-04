module Main where
import System.IO
import qualified Control.Monad.State as S
import qualified XML as X
import Data.Time

main :: IO ()
main = do
  let fileName = "./xmark10m.xml"
  putStrLn fileName
  input <- readFile fileName
  startTime <- getCurrentTime
  print $ take 200 (show (X.parseText input))
  endTime <- getCurrentTime
  print $ diffUTCTime endTime startTime
