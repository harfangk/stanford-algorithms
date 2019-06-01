module MinHeap (MinHeap(Empty,Node), empty, insert, merge, update, mergeAll, isEmpty, extract, find, size) where

data MinHeap k v = Empty | Node k v [MinHeap k v]

instance (Show k, Show v) => Show (MinHeap k v) where
  show h =
    case h of
      Empty -> "MinHeap.Empty"
      Node k v hs -> "MinHeap.Node(" ++ show k ++ ", " ++ show v ++ ", " ++ (show . length $ hs) ++ ")"

empty :: MaxHeap k v
empty = Empty

update :: Int -> [(Int, Int)] -> MinHeap Int Int -> MinHeap Int Int
update d es h =
  merge h (mergeAll (List.map (\(edgeHead, edgeWeight) -> MinNode (edgeWeight + d) edgeHead []) es) )

insert :: (Ord k) => (k, v) -> MinHeap k v -> MinHeap k v
insert (key, val) = merge (Node key val [])

merge :: (Ord k) => MinHeap k v -> MinHeap k v -> MinHeap k v
merge h Empty = h
merge Empty h = h
merge h1@(Node key1 value1 hs1) h2@(Node key2 value2 hs2)
  | key1 < key2 = Node key1 value1 (h2:hs1)
  | otherwise = Node key2 value2 (h1:hs2)

mergeAll :: (Ord k) => [MinHeap k v] -> MinHeap k v
mergeAll [] = Empty
mergeAll [h] = h
mergeAll (h:h':hs) = merge (merge h h') (mergeAll hs)

isEmpty :: MinHeap k v -> Bool
isEmpty Empty = True
isEmpty _ = False

extract :: (Ord k) => MinHeap k v -> (k, v, MinHeap k v)
extract Empty = error "MinHeap extract: empty MinHeap"
extract (Node key value hs) = (key, value, mergeAll hs)

size :: (Ord k) => MinHeap k v -> Int
size Empty = 0
size (Node _ _ hs) = (1 + (foldl (+) 0 (List.map size hs)))

find :: (Ord k) => MinHeap k v -> (k, v)
find Empty = error "MinHeap.find: empty heap"
find (Node k v _) = (k, v)
