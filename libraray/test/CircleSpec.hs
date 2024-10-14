module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Circle

-- Test case: Create a circular linked list from a Haskell list
testFromList :: Test
testFromList = TestCase $ do
  maybeCircle <- fromList [1, 2, 3, 4, 5]
  case maybeCircle of
    Just (headNode, tailNode) -> do
      firstNext <- moveForward headNode
      case firstNext of
        Just node2 -> assertEqual "Head moves to second node" (value node2) 2
        Nothing -> assertFailure "Expected node2 after head"

      firstPrev <- moveBackward tailNode
      case firstPrev of
        Just node4 -> assertEqual "Tail moves to fourth node" (value node4) 4
        Nothing -> assertFailure "Expected node4 before tail"

      tailNext <- moveForward tailNode
      case tailNext of
        Just node1 -> assertEqual "Tail next is head node" (value node1) 1
        Nothing -> assertFailure "Expected head node after tail"
    Nothing -> assertFailure "Could not create circular list"

-- Test case: Moving forward and backward in the circle
testMove :: Test
testMove = TestCase $ do
  maybeCircle <- fromList [1, 2, 3, 4, 5]
  case maybeCircle of
    Just (headNode, _) -> do
      -- Move forward through the circle
      firstNext <- moveForward headNode
      case firstNext of
        Just node2 -> assertEqual "Move forward from head" (value node2) 2
        Nothing -> assertFailure "Expected node2 after head"

      -- Move backward after moving forward
      case firstNext of
        Just node2 -> do
          maybeTailNode <- moveBackward node2
          case maybeTailNode of
            Just tailNode -> assertEqual "Move backward to head" 1 (value tailNode)
            Nothing -> assertFailure "Expected head node"
        Nothing -> assertFailure "Expected node2 to move backward from"
    Nothing -> assertFailure "Could not create circular list"

-- Test case: Insert a node into the circle
testInsertAfter :: Test
testInsertAfter = TestCase $ do
  fromList [1, 3] >>= maybe
    (assertFailure "Failed to create initial circle")
    (\(node1, node3) -> do
      fromList [2] >>= maybe
        (assertFailure "Failed to create node2")
        (\(node2, _) -> do
          insertAfter node1 node2

          -- Check the circular connections
          maybeNext <- moveForward node1
          case maybeNext of
            Just node2' -> assertEqual "Insert node after node1" 2 (value node2')
            Nothing -> assertFailure "Expected node2 after node1"

          maybeNext2 <- moveForward node2
          case maybeNext2 of
            Just node3' -> assertEqual "Node2 should link to node3" 3 (value node3')
            Nothing -> assertFailure "Expected node3 after node2"
        )
    )

-- Test case: Remove nodes from the circle
testRemoveAfter :: Test
testRemoveAfter = TestCase $ do
  maybeCircle <- fromList [1, 2, 3, 4, 5]
  case maybeCircle of
    Just (headNode, _) -> do
      -- Remove 3 nodes after the head node
      removedNodes <- removeAfter headNode 3
      assertEqual "Removed nodes" [2, 3, 4] removedNodes

      -- Check remaining circle (head should now point to 5)
      maybeNext <- moveForward headNode
      case maybeNext of
        Just node5 -> assertEqual "Remaining node should be 5" 5 (value node5)
        Nothing -> assertFailure "Expected node5 in circle"
    Nothing -> assertFailure "Could not create circular list"

-- Test suite
tests = hUnitTestToTests $ TestList
  [ TestLabel "testFromList" testFromList
  , TestLabel "testMove" testMove
  , TestLabel "testInsertAfter" testInsertAfter
  , TestLabel "testRemoveAfter" testRemoveAfter
  ]

-- Main function to run the tests
main :: IO ()
main = defaultMain tests
