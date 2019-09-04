{-# LANGUAGE ViewPatterns #-}

module Course3.Week3 where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Sequence

main :: IO ()
main = do
  huffmanData <- readFile "src/Course3/huffman.txt"
  mwisData <- readFile "src/Course3/mwis.txt"
  let mbHuffmanCodes = fmap (getCodes) . buildHuffmanTree (parseHuffman huffmanData) $ Sequence.Empty
      weights = parseMwis mwisData
      vMap = getVertexMap weights
      mwisMap = getMaxWeightTable weights
      mwisIndices = getMwisSolutionIndices (length weights) vMap mwisMap []
  print ("minCodeLength: " ++ (show . fmap (length . fst . head) $ mbHuffmanCodes))
  print ("maxCodeLength: " ++ (show . fmap (length . fst . head . List.reverse) $ mbHuffmanCodes))
  print ("Indices of vertices included in mwis: " ++ (show mwisIndices))

-- Q1, Q2

data Tree = LeafNode Int | TrunkNode Int Tree Tree
  deriving (Show)

instance Eq Tree where
  (==) (LeafNode _) (TrunkNode _ _ _) = False
  (==) (TrunkNode _ _ _) (LeafNode _) = False
  (==) (LeafNode i) (LeafNode j) = i == j
  (==) (TrunkNode _ t1l t1r) (TrunkNode _ t2l t2r) = (t1l == t2l) && (t1r == t2r)

instance Ord Tree where
  compare (LeafNode i) (TrunkNode j _ _) = compare i j
  compare (TrunkNode i _ _) (LeafNode j) = compare i j
  compare (LeafNode i) (LeafNode j) = compare i j
  compare (TrunkNode i _ _) (TrunkNode j _ _) = compare i j

getWeight :: Tree -> Int
getWeight tree =
  case tree of
    LeafNode i -> i
    TrunkNode i _ _ -> i

merge :: Tree -> Tree -> Tree
merge tree1 tree2 =
    TrunkNode (getWeight tree1 + getWeight tree2) (min tree1 tree2) (max tree1 tree2)

getCodes :: Tree -> [(String, Int)]
getCodes tree =
  List.sortOn (\(s, _) -> length s) . getCodes' "" $ tree

getCodes' :: String -> Tree -> [(String, Int)]
getCodes' prefix tree =
  case tree of
    LeafNode i -> [(prefix, i)]
    TrunkNode _ left right -> getCodes' (prefix ++ "0") left ++ getCodes' (prefix ++ "1") right

parseHuffman :: String -> Sequence.Seq Tree
parseHuffman =
  Sequence.fromList . List.sort . List.map (LeafNode . read) . tail . lines

buildHuffmanTree :: Sequence.Seq Tree -> Sequence.Seq Tree -> Maybe Tree
buildHuffmanTree seq1 seq2 =
  if Sequence.length seq1 == 1 && Sequence.null seq2 then
    Sequence.lookup 0 seq1
  else if Sequence.null seq1 && Sequence.length seq2 == 1 then
    Sequence.lookup 0 seq2
  else
    buildHuffmanTree seq1'' ((Sequence.|>) seq2'' (merge tree1 tree2))
    where
      (seq1', seq2', tree1) = dequeueSequences seq1 seq2
      (seq1'', seq2'', tree2) = dequeueSequences seq1' seq2'

dequeueSequences :: Sequence.Seq Tree -> Sequence.Seq Tree -> (Sequence.Seq Tree, Sequence.Seq Tree, Tree)
dequeueSequences seq1 seq2 =
    case (seq1, seq2) of
      ((Sequence.:<|) tree seq1', Sequence.Empty) -> (seq1', seq2, tree)
      (Sequence.Empty, (Sequence.:<|) tree seq2') -> (seq1, seq2', tree)
      ((Sequence.:<|) tree1 seq1', (Sequence.:<|) tree2 seq2') ->
        case compare tree1 tree2 of
          GT -> (seq1, seq2', tree2)
          LT -> (seq1', seq2, tree1)
          EQ -> (seq1', seq2, tree1)
      (Sequence.Empty, Sequence.Empty) -> (seq1, seq2, (LeafNode (-1)))

-- Q3

parseMwis :: String -> [Int]
parseMwis = List.map read . tail . lines

getMaxWeightTable :: [Int] -> Map.Map Int Int
getMaxWeightTable =
  snd . List.foldl f (1, Map.empty)
  where
    f :: (Int, Map.Map Int Int) -> Int -> (Int, Map.Map Int Int)
    f (index, mwisMap) w =
      case index of
        1 -> (2, Map.insert 1 w mwisMap)
        2 -> (3, Map.insert 2 w mwisMap)
        i -> (i + 1, Map.insert i maxWeight mwisMap)
          where
            prevIndexMaxWeight = Maybe.fromJust . Map.lookup (i - 1) $ mwisMap
            prevPrevIndexMaxWeight = Maybe.fromJust . Map.lookup (i - 2) $ mwisMap
            maxWeight = max (prevPrevIndexMaxWeight + w) prevIndexMaxWeight

getVertexMap :: [Int] -> Map.Map Int Int
getVertexMap =
  snd . List.foldl (\(i, vMap) w -> (i+1, Map.insert i w vMap)) (1, Map.empty)

getMwisSolutionIndices :: Int -> Map.Map Int Int -> Map.Map Int Int -> [Int] -> [Int]
getMwisSolutionIndices currentIndex vertexMap mwMap solutionIndices =
  if currentIndex == 2 then
    solutionIndices
  else if currentIndex == 1 then
    (currentIndex:solutionIndices)
  else
    if (prevPrevIndexMaxWeight + vWeight) <= prevIndexMaxWeight then
      getMwisSolutionIndices (currentIndex - 1) vertexMap mwMap solutionIndices
    else
      getMwisSolutionIndices (currentIndex - 2) vertexMap mwMap (currentIndex:solutionIndices)
  where
    vWeight = Maybe.fromJust . Map.lookup (currentIndex) $ vertexMap
    prevIndexMaxWeight = Maybe.fromJust . Map.lookup (currentIndex - 1) $ mwMap
    prevPrevIndexMaxWeight = Maybe.fromJust . Map.lookup (currentIndex - 2) $ mwMap
