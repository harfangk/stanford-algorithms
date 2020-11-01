{-# LANGUAGE FlexibleContexts #-}

module Course4.Week4 where

import System.IO
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Array.ST.Safe as STArray
import qualified Data.HashTable.Class as HT
import qualified Data.HashTable.ST.Basic as HTBasic

main :: IO ()
main = do
  (n1, c1) <- parseData "./src/Course4/2sat1.txt"
  (n2, c2) <- parseData "./src/Course4/2sat2.txt"
  (n3, c3) <- parseData "./src/Course4/2sat3.txt"
  (n4, c4) <- parseData "./src/Course4/2sat4.txt"
  (n5, c5) <- parseData "./src/Course4/2sat5.txt"
  (n6, c6) <- parseData "./src/Course4/2sat6.txt"
  let result1 = twoSat n1 c1
      result2 = twoSat n2 c2
      result3 = twoSat n3 c3
      result4 = twoSat n4 c4
      result5 = twoSat n5 c5
      result6 = twoSat n6 c6
  print (result1, result2, result3, result4, result5, result6)

parseData :: FilePath -> IO (Int, [(Int, Int)])
parseData fp = withFile fp ReadMode $ \h -> do
  n <- getMetaData h
  clauses <- replicateM n (getClauses h)
  pure (n, clauses)
  where
    getMetaData h = parseMetaData <$> BS.hGetLine h
    parseMetaData line = Maybe.fromJust $ do
      (x, line') <- BS.readInt line
      if BS.null (dropSpace line')
        then pure x
        else error "Invalid metadata format"

    getClauses h = parseClauses <$> BS.hGetLine h
    parseClauses line = Maybe.fromJust $ do
      (u, line') <- BS.readInt line
      (v, line'') <- BS.readInt (dropSpace line')
      if BS.null (dropSpace line'')
        then pure (u, v)
        else error "Invalid data format"

    dropSpace = BS.dropWhile (== ' ')

inf :: Double
inf = 1/0

twoSat :: Int -> [(Int, Int)] -> Bool
twoSat n coordinates =
  checkSatisfiability n scc
  where
    g = buildGraph n coordinates
    invG = buildInvGraph n coordinates
    list = firstDfs g
    scc = secondDfs n invG list

buildGraph :: Int -> [(Int, Int)] -> IArray.Array Int [Int]
buildGraph n clauses = STArray.runSTArray $ do
  g <- STArray.newArray (-n, n) []
  forM_ clauses (\(t1,t2) -> do
                      outboundEdgesFromT1' <- STArray.readArray g (-t1)
                      STArray.writeArray g (-t1) (t2:outboundEdgesFromT1')
                      outboundEdgesFromT2' <- STArray.readArray g (-t2)
                      STArray.writeArray g (-t2) (t1:outboundEdgesFromT2')
                    )
  return g

buildInvGraph :: Int -> [(Int, Int)] -> IArray.Array Int [Int]
buildInvGraph n clauses = STArray.runSTArray $ do
  invG <- STArray.newArray (-n, n) []
  forM_ clauses (\(t1,t2) -> do
                      outboundEdgesFromT1 <- STArray.readArray invG t1
                      STArray.writeArray invG t1 (-t2:outboundEdgesFromT1)
                      outboundEdgesFromT2 <- STArray.readArray invG t2
                      STArray.writeArray invG t2 (-t1:outboundEdgesFromT2)
                    )
  return invG

firstDfs :: IArray.Array Int [Int] -> [Int]
firstDfs g =
  result
  where
    vertices = Set.toList . List.foldr (\(u, vs) acc -> List.foldr Set.insert (Set.insert u acc) vs) Set.empty $ (List.filter (\(_, vs) -> not (List.null vs)) . IArray.assocs $ g)
    result = runST $ do
      ht <- HT.fromListWithSizeHint (length vertices) (zip vertices $ repeat False) :: ST s (HTBasic.HashTable s Int Bool)
      foldM (firstDfs' ht) [] vertices
    firstDfs' ht list u = do
      visited' <- HT.lookup ht u
      if Maybe.fromJust visited' then
        pure list
      else do
        HT.insert ht u True
        list' <- foldM (firstDfs' ht) list (g IArray.! u)
        pure (u:list')

secondDfs :: Int -> IArray.Array Int [Int] -> [Int] -> IArray.Array Int (Bool, Int)
secondDfs n invG list = STArray.runSTArray $ do
  memo <- STArray.newArray (-n, n) (False, -1)
  foldM_ (step memo) 1 list
  pure memo
  where
    step memo counter u = do
      (visited, _) <- STArray.readArray memo u
      if visited then
        pure counter
      else do
        secondDfs' memo counter u
        pure (counter + 1)

    secondDfs' memo counter u = do
      (visited, component) <- STArray.readArray memo u
      if visited then
        pure ()
      else do
        STArray.writeArray memo u (True, component)
        forM_ (invG IArray.! u) (secondDfs' memo counter)
        STArray.writeArray memo u (True, counter)

checkSatisfiability :: Int -> IArray.Array Int (Bool, Int) -> Bool
checkSatisfiability n scc =
  List.foldl' (\acc t -> acc && isSatisfiable t) True [1..n]
  where
    isSatisfiable t = componentT == (-1) || componentT' == (-1) || (componentT /= componentT')
      where
        componentT = snd (scc IArray.! t)
        componentT' = snd (scc IArray.! (-t))

