{-# LANGUAGE FlexibleContexts #-}

module Course4.Week2 where

import System.IO
import Control.Monad
import qualified Data.Bits as Bits
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lex.Fractional as BSLexing
import qualified Data.Array.ST.Safe as STArray
import qualified Data.Set as Set

import qualified Data.Maybe as Maybe

main :: IO ()
main = do
  (nodeCount, coordinates) <- parseData "./src/Course4/tsp.txt"
  let result = tsp nodeCount coordinates
  print result

parseData :: FilePath -> IO (Int, [(Double, Double)])
parseData fp = withFile fp ReadMode $ \h -> do
  nodeCount <- getMetaData h
  coordinates <- replicateM nodeCount (getCoordinates h)
  pure (nodeCount, coordinates)
  where
    getMetaData h = parseMetaData <$> BS.hGetLine h
    parseMetaData line = Maybe.fromJust $ do
      (x, line') <- BS.readInt line
      if BS.null (dropSpace line')
        then pure x
        else error "Invalid data format"

    getCoordinates h = parseCoordinates <$> BS.hGetLine h
    parseCoordinates line = Maybe.fromJust $ do
      (x, line') <- BSLexing.readDecimal line
      (y, line'') <- BSLexing.readDecimal (dropSpace line')
      if BS.null (dropSpace line'')
        then pure (x, y)
        else error "Invalid data format"

    dropSpace = BS.dropWhile (== ' ')

inf :: Double
inf = 1/0

calculateDists :: Int -> [(Double, Double)] -> IArray.Array (Int, Int) Double
calculateDists nodeCount coordinates =
  IArray.array ((1,1), (nodeCount, nodeCount)) [((i,j), calculateDist c1 c2) | (i, c1) <- indexedCoordinates, (j, c2) <- indexedCoordinates]
  where
    indexedCoordinates = zip [1..] coordinates :: [(Int, (Double, Double))]


tsp :: Int -> [(Double, Double)] -> Double
tsp nodeCount coordinates =
  minimum [dist + (dists IArray.! (1,destination)) | ((visited, destination), dist) <- IArray.assocs calculated, visited == maxI]
  where
    powerSet = Set.delete Set.empty . Set.powerSet . Set.fromList $ [2..nodeCount]
    sets = (STArray.runSTArray $ do
      array <- STArray.newArray (2, 25) []
      forM_ (Set.toList powerSet) (\set -> do
                                      let size = Set.size set + 1
                                      setsInArray <- STArray.readArray array size
                                      STArray.writeArray array size (Set.insert 1 set:setsInArray)
                                     )
      return array) :: IArray.Array Int [Set.Set Int]
    maxI = Bits.shift 1 nodeCount - 1:: Int
    dists = calculateDists nodeCount coordinates
    calculated = STArray.runSTUArray $ do
      memo <- STArray.newArray ((1, 1), (maxI, nodeCount)) inf
      STArray.writeArray memo (1,1) 0
      forM_ sets (iterateSubsetsOfSizeM memo dists)
      return memo
        where
          iterateSubsetsOfSizeM memo dists' subsetsOfSizeM =
            forM_ subsetsOfSizeM (\subsetOfSizeM ->
                                     forM_ subsetOfSizeM (\j ->
                                                           if j == 1 then
                                                             pure ()
                                                           else do
                                                             let subset' = Set.delete j subsetOfSizeM
                                                             val <- minimum <$> forM (Set.toList subset') (\k -> do
                                                                                                            previousStepValue <- STArray.readArray memo (setToBits subset', k)
                                                                                                            return (previousStepValue + dists' IArray.! (j, k))
                                                                                                          )
                                                             STArray.writeArray memo (setToBits subsetOfSizeM, j) val
                                                         )
                                 )
    setToBits :: Set.Set Int -> Int
    setToBits = Set.foldl (\acc i -> (Bits..|.) acc (Bits.shift 1 (i - 1))) 1

calculateDist :: (Double, Double) -> (Double, Double) -> Double
calculateDist (x1,y1) (x2,y2) =
  sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
