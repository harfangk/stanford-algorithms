module Main where

import qualified Course4.Week1
import qualified Data.Array.IArray as IArray

main :: IO ()
main = do
  (g1Vc, _, g1Edges) <- Course4.Week1.parseData "./src/Course4/g3.txt"
  let g1 = Course4.Week1.buildAdjacencyListByTail g1Vc g1Edges
  let g1Result = Course4.Week1.floydWarshall2 g1 g1Vc
  let mina = fmap (minimum . IArray.elems) g1Result
  print mina
