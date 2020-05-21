module Main where

import qualified Course4.Week1

main :: IO ()
main = do
  g1File <- readFile "./src/Course4/g3.txt"
  let g1Data@(g1Vc, _, _) = Course4.Week1.parseData g1File
      g1Result = Course4.Week1.bellmanFord (Course4.Week1.buildAdjacencyListByHead g1Data) g1Vc 1
  print g1Result
