{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

--------------------------------------------------------------------------------

import Data.Maybe (isJust)
import Control.Monad (guard)

-- AA search trees
data AATree a = TODO
  deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = error "emptyTree not implemented"

get :: Ord a => a -> AATree a -> Maybe a
get = error "get not implemented"

-- You may find it helpful to define
--   split :: AATree a -> AATree a
--   skew  :: AATree a -> AATree a
-- and call these from insert.
insert :: Ord a => a -> AATree a -> AATree a
insert = error "insert not implemented"

inorder :: AATree a -> [a]
inorder = error "inorder not implemented"

size :: AATree a -> Int
size = error "size not implemented"

height :: AATree a -> Int
height = error "height not implemented"

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  checkRedNodes root &&
  isJust (checkBlackHeight root) &&
  isBlack root

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted = error "isSorted not implemented"

-- True if red nodes only occur as the right child of a black node
checkRedNodes :: AATree a -> Bool
checkRedNodes = error "checkRedNodes not implemented"

-- If the number of black nodes from each leaf to the root
-- are the same, then that number is returned,
-- otherwise returns Nothing
checkBlackHeight :: AATree a -> Maybe Int
checkBlackHeight tree
  | isEmpty tree = do
      guard (isBlack tree)
      return 1
  | otherwise = do
      let left = leftSub tree
      let right = rightSub tree
      lh <- checkBlackHeight left
      rh <- checkBlackHeight right
      guard (lh == rh)
      if isBlack tree
      then
        return (lh + 1)
      else do
        guard (isBlack left)
        guard (isBlack right)
        return lh

isEmpty :: AATree a -> Bool
isEmpty = error "isEmpty not implemented"

leftSub :: AATree a -> AATree a
leftSub = error "leftSub not implemented"

rightSub :: AATree a -> AATree a
rightSub = error "rightSub not implemented"

isBlack :: AATree a -> Bool
isBlack = error "isBlack not implemented"


--------------------------------------------------------------------------------
