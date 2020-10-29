{-# LANGUAGE FlexibleContexts #-}

module Course4.Week3 where

import System.IO
import Control.Monad
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lex.Fractional as BSLexing
import qualified Data.List as List
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
  (nodeCount, coordinates) <- parseData "./src/Course4/nn.txt"
  let result = tsp nodeCount coordinates
  print result

parseData :: FilePath -> IO (Int, [(Int, (Double, Double))])
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
        else error "Invalid metadata format"

    getCoordinates h = parseCoordinates <$> BS.hGetLine h
    parseCoordinates line = Maybe.fromJust $ do
      (i, line') <- BS.readInt line
      (x, line'') <- BSLexing.readDecimal (dropSpace line')
      (y, line''') <- BSLexing.readDecimal (dropSpace line'')
      if BS.null (dropSpace line''')
        then pure (i, (x, y))
        else error "Invalid data format"

    dropSpace = BS.dropWhile (== ' ')

inf :: Double
inf = 1/0

tsp :: Int -> [(Int, (Double, Double))] -> Double
tsp nodeCount coordinates =
  dist + finalTripDist
  where
    cities = reverse . tail . map fst $ coordinates
    dists = IArray.array (1, nodeCount) coordinates :: IArray.Array Int (Double, Double)
    tsp' _ [] accDist cur = (cur, accDist)
    tsp' dists' cities' accDist cur =
      tsp' dists' cities'' accDist' next
      where
        (next, distToNext) = List.foldl' (compareCity cur) (0, inf) cities'
        accDist' = accDist + sqrt distToNext
        cities'' = List.delete next cities'
    (lastCity, dist) = tsp' dists cities 0 1
    finalTripDist = sqrt (calculateSquaredDist (dists IArray.! 1) (dists IArray.! lastCity))
    compareCity currentCity (minCity, minDist) candidateCity
      | candidateDist < minDist = (candidateCity, candidateDist)
      | candidateDist == minDist && candidateCity < minCity = (candidateCity, candidateDist)
      | candidateDist == minDist && candidateCity >= minCity = (minCity, minDist)
      | otherwise = (minCity, minDist)
      where
        candidateDist = calculateSquaredDist (dists IArray.! currentCity) (dists IArray.! candidateCity)

calculateSquaredDist :: (Double, Double) -> (Double, Double) -> Double
calculateSquaredDist (x1, y1) (x2, y2)= ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)
