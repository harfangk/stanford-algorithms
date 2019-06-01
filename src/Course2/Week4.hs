module Course2.Week4 where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.HashTable.IO as HT
import Control.Monad

type HashTable k v = HT.CuckooHashTable k v

-- HashTable implementation, was correct but too slow
main :: IO ()
main = do
  file <- readFile "src/Course2/2sum.txt"
  let ints = List.map read (lines file) :: [Int]
  let tupleInts = List.map (\i -> (i, i)) ints
  ht <- HT.fromListWithSizeHint 1000000 tupleInts :: IO (HashTable Int Int)
  let range = [-10000 .. -9000]
  result <- filterM (filterF ht) range
  print (List.length result)
  where
    filterF :: HashTable Int Int -> Int -> IO Bool
    filterF ht i = do
      (_,i',bool) <- (HT.foldM folder (ht, i, False) ht)
      return bool
    folder :: (HashTable Int Int, Int, Bool) -> (Int, Int) -> IO (HashTable Int Int, Int, Bool)
    folder (ht, i, bool) (k,v) = do
        let k' = i - k
        t <- HT.lookup ht k'
        case t of
            Nothing -> return (ht, i, bool || False)
            Just _ ->
              if k == k' then
                return (ht, i, bool || False)
              else
                return (ht, i, bool || True)

-- Set implementation
main2 :: IO ()
main2 = do
  file <- readFile "src/Course2/2sum.txt"
  let set = Set.fromList . List.map read . lines $ file :: Set.Set Int
      rMax = 10000
      rMin = -10000
      tupleSet = Set.foldl (f rMin rMax set) Set.empty set
      resultSet = Set.map (\(x,y) -> x + y) tupleSet
      result = Set.size resultSet
  print result
  where
    f :: Int -> Int -> Set.Set Int -> Set.Set (Int, Int) -> Int -> Set.Set (Int, Int)
    f rMin rMax set acc i =
        let rMin' = rMin - i
            rMax' = rMax - i
            set' =  Set.map (\x -> (min i x, max i x)) . Set.takeWhileAntitone (\x -> x <= rMax') . Set.dropWhileAntitone (\x -> x < rMin') $ set
        in
          Set.union acc set'
