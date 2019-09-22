{-# LANGUAGE ViewPatterns, FlexibleContexts #-}

module Course3.Week4 where

import qualified Data.Array.ST as STArray
import qualified Data.Array.Unboxed as UnboxedArray
import qualified Data.List as List
import qualified Control.Monad.ST as ST
import Debug.Trace

main :: IO ()
main = do
  smallKnapsackData <- readFile "src/Course3/knapsack1.txt"
  bigKnapsackData <- readFile "src/Course3/knapsack_big.txt"
  let (smallKnapsackSize, smallItemCount, smallItemData) = parseKnapsackData smallKnapsackData
      smallKnapsackTable = knapsackOuterLoop (smallKnapsackSize, smallItemCount, smallItemData)
      smallKnapsackResult = (UnboxedArray.!) smallKnapsackTable (smallItemCount, smallKnapsackSize)
      (bigKnapsackSize, bigItemCount, bigItemData) = parseKnapsackData bigKnapsackData
      bigKnapsackTable = knapsackOuterLoop (bigKnapsackSize, bigItemCount, bigItemData)
      bigKnapsackResult = (UnboxedArray.!) bigKnapsackTable (bigItemCount, bigKnapsackSize)
  print $ "smallKnapsackResult: " ++ show smallKnapsackResult
  print $ "bigKnapsackResult: " ++ show bigKnapsackResult

-- Q1, Q2

parseKnapsackData :: String -> (Int, Int, [(Int, Int)])
parseKnapsackData s =
  (knapsackSize, itemCount, itemData)
  where
    fileLines = lines s
    (knapsackSize, itemCount) = parseLine . head $ fileLines
    itemData = map parseLine . tail $ fileLines
    parseLine :: String -> (Int, Int)
    parseLine line =
      case List.map read . words $ line of
        (x:y:[]) -> (x, y)
        _ -> error "Invalid data format"

knapsackOuterLoop :: (Int, Int, [(Int, Int)]) -> UnboxedArray.UArray (Int, Int) Int
knapsackOuterLoop (knapsackSize, itemCount, itemData) = STArray.runSTUArray resultArray
  where
    initialArray = STArray.newArray ((0,0), (itemCount,knapsackSize)) 0 :: ST.ST s (STArray.STUArray s (Int, Int) Int)
    (resultArray, _) = List.foldl' (\(outerAcc, i) (v, w) -> knapsackInnerLoop knapsackSize (v, w) (outerAcc, i)) (initialArray, 1) itemData :: (ST.ST s (STArray.STUArray s (Int, Int) Int), Int)


knapsackInnerLoop :: Int -> (Int, Int) -> (ST.ST s (STArray.STUArray s (Int, Int) Int), Int) -> (ST.ST s (STArray.STUArray s (Int, Int) Int), Int)
knapsackInnerLoop knapsackSize (v, w) (acc, i) = (List.foldl' (\innerAcc x -> do
                                         innerAcc' <- innerAcc
                                         withoutItem <- STArray.readArray innerAcc' (i-1,x)
                                         withItem <- if (x-w) >= 0 then
                                                       fmap ((+) v) (STArray.readArray innerAcc' (i-1,x-w))
                                                     else
                                                       pure 0
                                         let val = max withoutItem withItem
                                         STArray.writeArray innerAcc' (trace ("index: " ++ show (i,x) ++ ", val: " ++ show val) (i,x)) val
                                         pure innerAcc'
                                         ) acc [0..knapsackSize], i+1)
