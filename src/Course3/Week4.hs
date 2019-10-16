{-# LANGUAGE ViewPatterns, FlexibleContexts #-}

module Course3.Week4 where

import qualified Data.Array.ST as STArray
import qualified Data.Array.IArray as IArray
import qualified Data.Array.Unboxed as UnboxedArray
import qualified Data.List as List
import qualified Control.Monad.ST as ST
import qualified Data.STRef as STRef
import qualified Data.HashTable.Class as HT
import qualified Data.HashTable.ST.Cuckoo as Cuckoo
import Debug.Trace

type HashTable s k v = Cuckoo.HashTable s k v

main :: IO ()
main = do
  smallKnapsackData <- readFile "src/Course3/knapsack_test.txt"
  bigKnapsackData <- readFile "src/Course3/knapsack_big.txt"
  let (smallKnapsackSize, smallItemCount, smallItemData) = parseKnapsackData smallKnapsackData
      smallItemArray = IArray.listArray (1,smallItemCount) smallItemData
      smallKnapsackResult = knapsack (smallItemArray) (smallItemCount, smallKnapsackSize)
  {-
      smallKnapsackTable = knapsackOuterLoop (smallKnapsackSize, smallItemCount, smallItemData)
      smallKnapsackResult = (UnboxedArray.!) smallKnapsackTable (smallItemCount,smallKnapsackSize)
      (bigKnapsackSize, bigItemCount, bigItemData) = parseKnapsackData bigKnapsackData
      bigKnapsackTable = knapsackOuterLoop'' (bigKnapsackSize, bigItemCount, bigItemData)
      bigKnapsackResult = (UnboxedArray.!) bigKnapsackTable (bigKnapsackSize)
  -}
  print smallItemArray
  print $ "smallKnapsackResult: " ++ show smallKnapsackResult
  -- print $ "bigKnapsackResult: " ++ show bigKnapsackResult

-- Q1, Q2

knapsack :: Monad m => (Int -> m Int) -> IArray.Array Int (Int,Int) -> (Int,Int) -> m Int
knapsack _ _ (0,_) = return 0
knapsack _ _ (_,0) = return 0
knapsack recf items (i,x) = do
  max val1 val2
  where
    (v,w) = (IArray.!) items i
    val1 = knapsack items (i-1,x)
    val2 = if x < w then
             0
           else
             (knapsack items (i-1,x-w)) + v

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
                                         STArray.writeArray innerAcc' (i,x) val
                                         pure innerAcc'
                                         ) acc [0..knapsackSize], i+1)


  {-
knapsackOuterLoop' :: (Int, Int, [(Int, Int)]) -> UnboxedArray.UArray Int Int
knapsackOuterLoop' (knapsackSize, _, itemData) = STArray.runSTUArray resultArray
  where
    initialArray = STArray.newArray (0, knapsackSize) 0 :: ST.ST s (STArray.STUArray s Int Int)
    (resultArray, _) = List.foldl' (\(prevColumn, i) (v, w) -> knapsackInnerLoop' knapsackSize (v, w) (prevColumn, i)) (initialArray, 1) itemData :: (ST.ST s (STArray.STUArray s Int Int), Int)



knapsackInnerLoop' :: Int -> (Int, Int) -> (ST.ST s (STArray.STUArray s Int Int), Int) -> (ST.ST s (STArray.STUArray s Int Int), Int)
knapsackInnerLoop' knapsackSize (v, w) (prevColumn, i) = (List.foldl' (\acc x -> do
                                                                          newColumn <- STArray.newArray (0, knapsackSize) 0 :: ST.ST s (STArray.STUArray s Int Int)
                                                                          acc' <- acc
                                                                          withoutItem <- STArray.readArray acc' x
                                                                          withItem <-
                                                                            if (x-w) >= 0 then
                                                                              fmap ((+) v) (STArray.readArray acc' (x - w))
                                                                            else
                                                                              pure 0
                                                                          let val = max withoutItem withItem
                                                                          STArray.writeArray newColumn x val
                                                                          pure newColumn
                                                                      ) prevColumn [0..knapsackSize], i+1)
{-
1. initial array for i = 0
2. build a new array from previous column of i - 1
-}

knapsackOuterLoop'' :: (Int, Int, [(Int, Int)]) -> IArray.Array Int Int
knapsackOuterLoop'' (knapsackSize, _, items) =
    resultArray
    where
      initialArray = IArray.array (0, knapsackSize) [(i,0) | i <- [0..knapsackSize]] :: IArray.Array Int Int
      (resultArray, _) = List.foldl' (\(prevColumn, i) (v, w) ->
                                        let (nextStep, i') = knapsackInnerLoop'' knapsackSize (v, w) (prevColumn, i)
                                        in
                                          traceShow ((IArray.!) nextStep knapsackSize, i') (nextStep, i')
                                     ) (initialArray, 1) items :: (IArray.Array Int Int, Int)

knapsackInnerLoop'' :: Int -> (Int, Int) -> (IArray.Array Int Int, Int) -> (IArray.Array Int Int, Int)
knapsackInnerLoop'' knapsackSize (v, w) (prevColumn, i) = (IArray.array (0,knapsackSize) $ foldr f [] [0..knapsackSize], i+1)
  where
    f :: Int -> [(Int, Int)] -> [(Int, Int)]
    f x acc =
      let
        withItem = (IArray.!) prevColumn x
        withoutItem =
          if (x-w) >= 0 then
            (IArray.!) prevColumn (x-w) + v
          else
            0
        val = max withItem withoutItem
      in
        (x,val):acc

{-
1. val = lookup (i,x) from ht
2. if val exists, return ht
3. if val doesn't exist
3-1. v,w = lookup i from array
3-2. val' = max (lookup (i-1,x)) (lookup (i-1,x-w) + v)
3-3. ht' = insert (i,x) val'
-}

knapsack :: IArray.Array Int (Int,Int) -> (Int,Int) -> Int
knapsack _ (0,_) = 0
knapsack _ (_,0) = 0
knapsack items (i,x) = 0
  max val1 val2
  where
    (v,w) = (IArray.!) items i
    val1 = knapsack items (i-1,x)
    val2 = if x < w then
             0
           else
             (knapsack items (i-1,x-w)) + v

buildKnapsackHashtable :: (Int, Int) -> IArray.Array Int (Int,Int) -> HashTable s (Int,Int) Int
buildKnapsackHashtable (itemCount, knapsackSize) items = ST.runST $ buildKnapsackHashtable' (HT.new, itemCount, knapsackSize, items)

buildKnapsackHashtable' :: (ST.ST s (HashTable s (Int,Int) Int), Int, Int, IArray.Array Int (Int,Int)) -> (ST.ST s (HashTable s (Int,Int) Int), Int, Int, IArray.Array Int (Int,Int))
buildKnapsackHashtable' (ht, i, x, items) = do
  ht' <- ht
  (_, _, ) <- getVal ht' (i,x)
  case val of
    Just val' -> ht
    Nothing -> max (getVal ht' (i-1,x) (getVal ht' (i-1,x - w)))
  where
    (v,w) = (IArray.!) items i
    getVal :: HashTable s (Int,Int) Int -> (Int,Int) -> ST.ST s (Maybe Int)
    getVal ht' (i,x) =
      if i == 0 then
        pure (Just 0)
      else
        case HT.lookup ht' (i,x) of
          Just val'' -> pure (Just val'')
          Nothing -> getVal ht' (i-1,x)
    f :: HashTable s (Int,Int) Int -> (Int,Int) -> ST.ST s (HashTable s (Int,Int) Int)

  mbVal <-
    case (i,x) of
      (0,_) -> pure (Just 0)
      _ -> HT.lookup ht (i,x)
  case mbVal of
    Nothing -> HT.insert
-}
