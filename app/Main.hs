module Main where

import qualified Course4.Week2

main :: IO ()
main = do
  (nodeCount, coordinates) <- Course4.Week2.parseData "./src/Course4/tsp.txt"
  let result = Course4.Week2.tsp nodeCount coordinates
  print result
