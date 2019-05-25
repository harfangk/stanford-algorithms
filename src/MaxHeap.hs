module Course2.MaxHeap (MaxHeap(Empty,Node), empty, insert, merge, mergeAll, isEmpty, extract, find, size, update) where

data MaxHeap k v = Empty | Node k v [MaxHeap k v]

instance (Show k, Show v) => Show (MaxHeap k v) where
  show h =
    case h of
      Empty -> "MaxHeap.Empty"
      Node k v hs -> "Heap.Node(" ++ show k ++ ", " ++ show v ++ ", " ++ (show . length $ hs) ++ ")"

empty :: MaxHeap k v
empty = Empty

update :: Int -> [(Int, Int)] -> MinHeap Int Int -> MinHeap Int Int
update d es h =
  merge h (mergeAll (List.map (\(edgeHead, edgeWeight) -> Node (edgeWeight + d) edgeHead []) es) )

insert :: (Ord k) => (k, v) -> MaxHeap k v -> MaxHeap k v
insert (key, val) = merge (Node key val [])

merge :: (Ord k) => MaxHeap k v -> MaxHeap k v -> MaxHeap k v
merge h Empty = h
merge Empty h = h
merge h1@(Node key1 value1 hs1) h2@(Node key2 value2 hs2)
  | key1 > key2 = Node key1 value1 (h2:hs1)
  | otherwise = Node key2 value2 (h1:hs2)

mergeAll :: (Ord k) => [MaxHeap k v] -> MaxHeap k v
mergeAll [] = Empty
mergeAll [h] = h
mergeAll (h:h':hs) = merge (merge h h') (mergeAll hs)

isEmpty :: MaxHeap k v -> Bool
isEmpty Empty = True
isEmpty _ = False

extract :: (Ord k) => MaxHeap k v -> (k, v, MaxHeap k v)
extract Empty = error "MaxHeap extract: empty MaxHeap"
extract (Node key value hs) = (key, value, mergeAll hs)

find :: (Ord k) => MaxHeap k v -> (k, v)
find Empty = error "MaxHeap.find: empty heap"
find (Node k v _) = (k, v)

size :: (Ord k) => MaxHeap k v -> Int
size Empty = 0
size (Node _ _ hs) = (1 + (foldl (+) 0 (List.map size hs)))
