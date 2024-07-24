module Data.PriorityQueue
  ( Queue
  , newMaxQueue
  , newMinQueue
  -- , peekBack
  , peekFront
  , pop
  , popN
  , push
  , pushMany
  , size
  , toArray
  , toList
  ) where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global, toEffect)
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord (greaterThan, lessThan)
import Data.Traversable (for, for_)
import Effect (Effect)

--------------------------------------------------------------------------------
-- API

-- | A priority queue that allows for efficient insertion and removal of elements.
-- The queue can be created with a custom ordering function that determines the
-- priority of elements.
-- Note: it's not possible to have a meaninful Eq instance, as two queues with
-- the same elements might have them in different order due to the heap structure.
-- It's recommended to convert the queue to an array to compare it.
newtype Queue a = Queue
  { contents :: STArray Global a
  , ordering :: a -> a -> Boolean
  }

-- | Create a new priority queue where the element with the smallest value
-- according to the provided function will be at the front of the queue.
newMinQueue :: forall a. (a -> Number) -> Effect (Queue a)
newMinQueue fn = toEffect do
  contents <- STA.new
  pure $ Queue
    { contents
    , ordering: \a b -> greaterThan (fn a) (fn b)
    }

-- | Create a new priority queue where the element with the largest value
-- according to the provided function will be at the front of the queue.
newMaxQueue :: forall a. (a -> Number) -> Effect (Queue a)
newMaxQueue fn = toEffect do
  contents <- STA.new
  pure $ Queue
    { contents
    , ordering: \a b -> lessThan (fn a) (fn b)
    }

-- | Add an element to the queue.
push :: forall a. Queue a -> a -> Effect Unit
push q els = pushMany q [ els ]

-- | Add multiple elements to the queue.
pushMany :: forall a. Queue a -> Array a -> Effect Unit
pushMany (Queue { contents, ordering }) newVals = toEffect do
  for_ newVals (insert ordering contents)

-- | Remove and return the element at the front of the queue.
pop :: forall a. Queue a -> Effect (Maybe a)
pop (Queue { contents, ordering }) = toEffect do
  removeMax contents ordering

-- | Remove and return the first n elements from the queue.
-- Note: we guarantee that the elements are sorted in the order of the queue,
-- first element is the one with the highest priority.
popN :: forall a. Int -> Queue a -> Effect (Array a)
popN n (Queue { contents, ordering }) = toEffect do
  result <- STA.new
  go 0 result contents ordering
  where
  go :: forall h. Int -> STArray h a -> STArray h a -> (a -> a -> Boolean) -> ST h (Array a)
  go i result arr lt = do
    case i == n of
      true -> STA.freeze result
      false -> do
        maybeEl <- removeMax arr lt
        case maybeEl of
          -- If the queue is empty, there are no more items to pop
          Nothing -> STA.freeze result
          Just el -> do
            void $ STA.push el result
            go (i + 1) result arr lt

-- | Return the element at the front of the queue without removing it.
peekFront :: forall a. Queue a -> Effect (Maybe a)
peekFront (Queue { contents }) = toEffect do
  STA.peek 0 contents

-- TODO: we are not guaranteeing that the last element is actually sorted,
-- need to look into this.
-- peekBack :: forall a. Queue a -> Effect (Maybe a)
-- peekBack (Queue { contents }) = toEffect do
--   len <- STA.length contents
--   STA.peek (len - 1) contents

-- | Return the number of elements in the queue.
size :: forall a. Queue a -> Effect Int
size (Queue { contents }) = toEffect (STA.length contents)

-- | Convert the queue to an array.
toArray :: forall a. Queue a -> Effect (Array a)
toArray (Queue { contents }) = toEffect (STA.freeze contents)

-- | Convert the queue to a list.
toList :: forall a. Queue a -> Effect (List a)
toList = toArray >>> map List.fromFoldable

--------------------------------------------------------------------------------
-- Implementation

-- Implementation of the binary heap lifted from:
-- https://github.com/shawnw/racket-priority-queue (MIT License)

insert :: forall h a. (a -> a -> Boolean) -> STArray h a -> a -> ST h Unit
insert ordering arr el = do
  len <- STA.push el arr
  bubbleUp arr (len - 1) ordering

bubbleUp :: forall h a. STArray h a -> Int -> (a -> a -> Boolean) -> ST h Unit
bubbleUp arr i lt = do
  when (i > 0) do
    let j = Int.quot (i - 1) 2
    maybeIv <- STA.peek i arr
    maybeJv <- STA.peek j arr
    case maybeIv, maybeJv of
      Just iv, Just jv | not (lt iv jv) -> do
        swap arr i j
        bubbleUp arr j lt
      _, _ -> pure unit

removeMax :: forall h a. STArray h a -> (a -> a -> Boolean) -> ST h (Maybe a)
removeMax contents ordering = do
  maybeMax <- STA.peek 0 contents
  for maybeMax \maxEl -> do
    len <- STA.length contents
    case len == 1 of
      true -> void $ STA.pop contents
      false -> heapRemoveMax contents ordering
    pure maxEl

heapRemoveMax :: forall h a. STArray h a -> (a -> a -> Boolean) -> ST h Unit
heapRemoveMax contents lt = do
  len <- STA.length contents
  swap contents 0 (len - 1)
  _ <- STA.pop contents
  bubbleDown contents 0 lt

bubbleDown :: forall h a. STArray h a -> Int -> (a -> a -> Boolean) -> ST h Unit
bubbleDown arr idx lt = do
  let leftIdx = 2 * idx + 1
  let rightIdx = 2 * idx + 2
  maybeIv <- STA.peek idx arr
  maybeLeftV <- STA.peek leftIdx arr
  len <- STA.length arr
  let
    largestIdx' = case maybeIv, maybeLeftV of
      Just iv, Just lv | leftIdx < len, lt iv lv -> leftIdx
      _, _ -> idx
  maybeLargestV' <- STA.peek largestIdx' arr
  maybeRightV <- STA.peek rightIdx arr
  let
    largestIdx = case maybeLargestV', maybeRightV of
      Just lv, Just rv | rightIdx < len, lt lv rv -> rightIdx
      _, _ -> largestIdx'
  unless (largestIdx == idx) do
    swap arr idx largestIdx
    bubbleDown arr largestIdx lt

swap :: forall h a. STArray h a -> Int -> Int -> ST h Unit
swap arr i j = do
  maybeIv <- STA.peek i arr
  maybeJv <- STA.peek j arr
  case maybeIv, maybeJv of
    Just iv, Just jv -> do
      void $ STA.modify i (const jv) arr
      void $ STA.modify j (const iv) arr
    _, _ -> pure unit
