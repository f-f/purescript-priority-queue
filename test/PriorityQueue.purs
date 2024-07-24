module Test.PriorityQueue where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.PriorityQueue as PriorityQueue
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert as Assert

main :: Effect Unit
main = do
  log "PriorityQueue tests"
  log "  pushAndPop"
  pushAndPop
  log "  customSelector"
  customSelector
  log "  conversions"
  conversions
  log "  pushPopManyMax"
  pushPopManyMax
  log "  pushPopManyMin"
  pushPopManyMin
  log "  peek"
  peek

pushAndPop :: Effect Unit
pushAndPop = do
  queue <- PriorityQueue.newMinQueue Int.toNumber
  PriorityQueue.push queue 1
  PriorityQueue.push queue 2
  size' <- PriorityQueue.size queue
  Assert.assertEqual { actual: size', expected: 2 }
  el1 <- PriorityQueue.pop queue
  el2 <- PriorityQueue.pop queue
  size'' <- PriorityQueue.size queue
  Assert.assertEqual { actual: size'', expected: 0 }
  Assert.assertEqual { actual: { el1, el2 }, expected: { el1: Just 1, el2: Just 2 } }

customSelector :: Effect Unit
customSelector = do
  queue <- PriorityQueue.newMinQueue (_.foo >>> Int.toNumber)
  PriorityQueue.push queue { foo: 1 }
  PriorityQueue.push queue { foo: 2 }
  size' <- PriorityQueue.size queue
  Assert.assertEqual { actual: size', expected: 2 }
  el1 <- PriorityQueue.pop queue
  el2 <- PriorityQueue.pop queue
  size'' <- PriorityQueue.size queue
  Assert.assertEqual { actual: size'', expected: 0 }
  Assert.assertEqual { actual: el1, expected: Just { foo: 1 } }
  Assert.assertEqual { actual: el2, expected: Just { foo: 2 } }

conversions :: Effect Unit
conversions = do
  queue <- PriorityQueue.newMaxQueue Int.toNumber
  PriorityQueue.push queue 1
  PriorityQueue.push queue 2
  PriorityQueue.push queue 3
  list <- PriorityQueue.toList queue
  array <- PriorityQueue.toArray queue
  -- Note: we sort here because we can't guarantee the order of the elements,
  -- that's an implementation detail.
  Assert.assertEqual { actual: List.sort list, expected: List.fromFoldable [ 1, 2, 3 ] }
  Assert.assertEqual { actual: Array.sort array, expected: [ 1, 2, 3 ] }

pushPopManyMax :: Effect Unit
pushPopManyMax = do
  queue <- PriorityQueue.newMaxQueue Int.toNumber
  PriorityQueue.pushMany queue [ 1, 2, 3 ]
  els <- PriorityQueue.popN 4 queue
  Assert.assertEqual
    { actual: els
    , expected: [ 3, 2, 1 ]
    }
  size <- PriorityQueue.size queue
  Assert.assertEqual { actual: size, expected: 0 }

pushPopManyMin :: Effect Unit
pushPopManyMin = do
  queue <- PriorityQueue.newMinQueue Int.toNumber
  PriorityQueue.pushMany queue [ 1, 2, 3 ]
  els <- PriorityQueue.popN 2 queue
  Assert.assertEqual
    { actual: els
    , expected: [ 1, 2 ]
    }
  size <- PriorityQueue.size queue
  Assert.assertEqual { actual: size, expected: 1 }

peek :: Effect Unit
peek = do
  queue <- PriorityQueue.newMaxQueue Int.toNumber
  PriorityQueue.pushMany queue [ 1, 2, 3 ]
  front <- PriorityQueue.peekFront queue
  -- back <- PriorityQueue.peekBack queue
  Assert.assertEqual { actual: { front }, expected: { front: Just 3 } }
  -- Assert.assertEqual { actual: { back }, expected: { back: Just 1 } }
  size <- PriorityQueue.size queue
  Assert.assertEqual { actual: size, expected: 3 }
