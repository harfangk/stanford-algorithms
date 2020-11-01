module Main where

import qualified Course4.Week4

main :: IO ()
main = do
  (n1, c1) <- Course4.Week4.parseData "./src/Course4/2sat1.txt"
  (n2, c2) <- Course4.Week4.parseData "./src/Course4/2sat2.txt"
  (n3, c3) <- Course4.Week4.parseData "./src/Course4/2sat3.txt"
  (n4, c4) <- Course4.Week4.parseData "./src/Course4/2sat4.txt"
  (n5, c5) <- Course4.Week4.parseData "./src/Course4/2sat5.txt"
  (n6, c6) <- Course4.Week4.parseData "./src/Course4/2sat6.txt"
  let result1 = Course4.Week4.twoSat n1 c1
      result2 = Course4.Week4.twoSat n2 c2
      result3 = Course4.Week4.twoSat n3 c3
      result4 = Course4.Week4.twoSat n4 c4
      result5 = Course4.Week4.twoSat n5 c5
      result6 = Course4.Week4.twoSat n6 c6
  print (result1, result2, result3, result4, result5, result6)
