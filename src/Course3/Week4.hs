module Course3.Week4 where

import qualified Data.Array.ST as STArray
import qualified Data.Array.IArray as IArray
import qualified Data.Array.Unboxed as UnboxedArray
import qualified Data.List as List
import qualified Control.Monad.ST as ST
import qualified Data.HashTable.Class as HT
import qualified Data.HashTable.ST.Cuckoo as Cuckoo

type HashTable s k v = Cuckoo.HashTable s k v

main :: IO ()
main = do
  smallKnapsackData <- readFile "src/Course3/knapsack1.txt"
  bigKnapsackData <- readFile "src/Course3/knapsack_big.txt"
  let (smallKnapsackSize, smallItemCount, smallItemData) = parseKnapsackData smallKnapsackData
      smallItemArray = IArray.listArray (1,smallItemCount) smallItemData
      smallKnapsackResult = knapsack (smallItemArray) (smallItemCount, smallKnapsackSize)
      (bigKnapsackSize, bigItemCount, bigItemData) = parseKnapsackData bigKnapsackData
      bigItemArray = IArray.listArray (1,bigItemCount) bigItemData
      bigKnapsackResult = knapsack (bigItemArray) (bigItemCount, bigKnapsackSize)
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

-- Dynamic programming implementation with memoization based on hashtable

knapsack :: IArray.Array Int (Int,Int) -> (Int, Int) -> Int
knapsack items (i,x) = ST.runST $
   do ht <- HT.new :: ST.ST s (HashTable s (Int,Int) Int)
      memoF ht (i,x)
   where
      memoF :: HashTable s (Int,Int) Int -> (Int, Int) -> ST.ST s Int
      memoF ht (i',x') = do
         k <- HT.lookup ht (i',x')
         case k of
            Just k' -> return k'
            Nothing -> do
               k' <- solve (memoF ht) items (i',x')
               HT.insert ht (i',x') k'
               return k'

solve :: Monad m => ((Int, Int) -> m Int) -> IArray.Array Int (Int,Int) -> (Int,Int) -> m Int
solve _ _ (0,_) = return 0
solve _ _ (_,0) = return 0
solve memoF items (i,x) = do
  val1 <- memoF (i-1,x)
  val2 <- if x < w then
            return 0
          else
            fmap ((+) v) $ memoF (i-1,x-w)
  return $ max val1 val2
  where
    (v,w) = (IArray.!) items i

-- Brute force implementation with memoization based on mutable array

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
