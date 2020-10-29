module Main where

import qualified Course4.Week3

main :: IO ()
main = do
  (nodeCount, coordinates) <- Course4.Week3.parseData "./src/Course4/nn.txt"
  let result = Course4.Week3.tsp nodeCount coordinates
  print result
