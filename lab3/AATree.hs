{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where


-- AA search trees
data AATree a = Empty | TwoNode a (AATree a) (AATree a) Level
  deriving (Eq, Show, Read)
type Level = Int

-- O(1)
emptyTree :: AATree a
emptyTree = Empty

-- O(log n)
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get a (TwoNode b left right _)
  | a == b = Just a
  | a < b = get a left
  | a > b = get a right

-- O(1), ej rekursiv, alla operationer har konstant komplexitet
skew :: AATree a -> AATree a
skew Empty = Empty
skew (TwoNode node leftChild rightChild level)
  | level == getLevel leftChild = TwoNode (getNodeValue leftChild) (getLeftChild leftChild) originalNode level
  | otherwise = TwoNode node leftChild rightChild level
  where
    originalNode = TwoNode node (getRightChild leftChild) rightChild level

-- O(1), ej rekursiv, alla operationer har konstant komplexitet
split :: AATree a -> AATree a
split Empty = Empty
split (TwoNode node leftChild rightChild level)
  | level == getLevel rightChild && level == getLevel (getRightChild rightChild) = TwoNode newNode newLeftChild newRightChild (level+1)
  | otherwise = TwoNode node leftChild rightChild level
  where
    newNode = getNodeValue rightChild
    newLeftChild = TwoNode node leftChild (getLeftChild rightChild) level
    newRightChild = getRightChild rightChild

-- O(1)
getRightChild :: AATree a -> AATree a
getRightChild (TwoNode _ _ right _) = right

-- O(1)
getLeftChild :: AATree a -> AATree a
getLeftChild (TwoNode _ left _ _) = left

-- O(1)
getNodeValue :: AATree a -> a
getNodeValue (TwoNode value _ _ _) = value

-- O(1)
getLevel :: AATree a -> Int
getLevel Empty = 0
getLevel (TwoNode _ _ _ level) = level

-- How does this work?
-- We pattern match the AATree: 
  -- if it's Empty we will make it a leaf with the value we want to add.
  -- if not we have to check in which subtree we want the value to go (which recursively inserts the value to the left or right subtree)
    --  smaller goes to left subtree, so we have to skew the subtree (which will also call split)
    -- larger goes to right subtree, so we call the split function (which may or may not actually do a split)
    -- if equal, we don't want to add the new value since it is already in the tree
-- O(log n), because of skew and heap
insert :: Ord a => a -> AATree a -> AATree a
insert a Empty = TwoNode a Empty Empty 1
insert a (TwoNode b left right level)
  | a < b = split (skew (TwoNode b (insert a left) right level))
  | a > b = split (TwoNode b left (insert a right) level)
  | a == b = TwoNode b left right level

-- O(n), has to go through every node in the tree
inorder :: AATree a -> [a]
inorder Empty = []
inorder (TwoNode value Empty Empty _) = [value]
inorder (TwoNode nodeValue left right _) = inorder left ++ [nodeValue] ++ inorder right

-- O(n), has to go through every node in the tree
size :: AATree a -> Int
size Empty = 0
size (TwoNode _ Empty Empty _) = 1
size (TwoNode _ left Empty _) = 1 + size left
size (TwoNode _ Empty right _) = 1 + size right
size (TwoNode _ left right _) = 1 + size left + size right

-- O(log n)
height :: AATree a -> Int
height Empty = 0
height (TwoNode _ Empty Empty _) = 1
height (TwoNode _ left right _) = 1 + max(height left) (height right)

-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (getLeftChild x) ++ nodes (getRightChild x)

-- True if the given list is ordered
-- O(n)
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = (x <= y) && isSorted (y:xs)

-- Check if the invariant is true for a single AA node
-- (O (size tree)) so O(n)
checkLevels :: AATree a -> Bool
checkLevels Empty = True
checkLevels (TwoNode _ Empty Empty level) = level == 1
checkLevels (TwoNode _ Empty right level) = level >= getLevel right
checkLevels (TwoNode _ left right level) = level > getLevel left && level >= getLevel right && level > getLevel(getRightChild right)

-- O(1), constant factor of 1 try
isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty (TwoNode _ _ _ _) = False

--------------------------------------------------------------------------------