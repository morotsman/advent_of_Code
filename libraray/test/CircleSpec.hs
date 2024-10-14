module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.IORef (readIORef)
import Circle

-- Test case: Create a circular linked list from a Haskell list
testFromList :: Test
testFromList = TestCase $ do
  maybeHeadNode <- fromList [1, 2, 3, 4, 5]
  case maybeHeadNode of
    Just headNode -> do
      firstNext <- moveForward headNode
      case firstNext of
        Just node2 -> assertEqual "Head moves to second node" (value node2) 2
        Nothing -> assertFailure "Expected node2 after head"

      -- Move to the last node (tailNode)
      lastNode <- moveBackward headNode >>= maybe (assertFailure "No last node") return
      assertEqual "Tail node should be 5" 5 (value lastNode)

      -- Check that the list is circular
      tailNext <- moveForward lastNode
      case tailNext of
        Just node1 -> assertEqual "Tail next is head node" (value node1) 1
        Nothing -> assertFailure "Expected head node after tail"
    Nothing -> assertFailure "Could not create circular list"

-- Test case: Moving forward and backward in the circle
testMove :: Test
testMove = TestCase $ do
  maybeHeadNode <- fromList [1, 2, 3, 4, 5]
  case maybeHeadNode of
    Just headNode -> do
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
  maybeHeadNode <- fromList [1, 3]
  case maybeHeadNode of
    Just node1 -> do
      maybeNode2 <- fromList [2]
      case maybeNode2 of
        Just node2 -> do
          insertAfter node1 node2

          maybeNext <- moveForward node1
          case maybeNext of
            Just node2' -> assertEqual "Insert node after node1" 2 (value node2')
            Nothing -> assertFailure "Expected node2 after node1"

          maybeNext2 <- moveForward node2
          case maybeNext2 of
            Just node3' -> assertEqual "Node2 should link to node3" 3 (value node3')
            Nothing -> assertFailure "Expected node3 after node2"
        Nothing -> assertFailure "Failed to create node2"
    Nothing -> assertFailure "Failed to create initial circle"

-- Test case: Remove a single node from the circle
testRemoveOneNode :: Test
testRemoveOneNode = TestCase $ do
  maybeHeadNode <- fromList [1, 2, 3, 4, 5]
  case maybeHeadNode of
    Just headNode -> do
      removedNodes <- removeAfter headNode 1
      assertEqual "Removed nodes" [2] removedNodes

      maybeNext <- moveForward headNode
      case maybeNext of
        Just node3 -> assertEqual "Remaining node should be 3" 3 (value node3)
        Nothing -> assertFailure "Expected node3 in circle"
    Nothing -> assertFailure "Could not create circular list"

-- Test case: Remove multiple nodes from the circle
testRemoveAfter :: Test
testRemoveAfter = TestCase $ do
  maybeHeadNode <- fromList [1, 2, 3, 4, 5]
  case maybeHeadNode of
    Just headNode -> do
      -- Remove 3 nodes after the head node
      removedNodes <- removeAfter headNode 3
      assertEqual "Removed nodes" [2, 3, 4] removedNodes

      maybeNext <- moveForward headNode
      case maybeNext of
        Just node5 -> assertEqual "Remaining node should be 5" 5 (value node5)
        Nothing -> assertFailure "Expected node5 in circle"
    Nothing -> assertFailure "Could not create circular list"

-- Test case: Remove all nodes from the circle
testRemoveAll :: Test
testRemoveAll = TestCase $ do
  maybeHeadNode <- fromList [1, 2, 3, 4, 5]
  case maybeHeadNode of
    Just headNode -> do
      removedNodes <- removeAfter headNode 5
      assertEqual "Removed nodes" [2, 3, 4, 5, 1] removedNodes

      -- Check that the circle is now empty
      maybeNext <- readIORef (next headNode)
      case maybeNext of
        Just _ -> assertFailure "Expected an empty circle"
        Nothing -> return ()  -- Success

      maybePrev <- readIORef (prev headNode)
      case maybePrev of
        Just _ -> assertFailure "Expected an empty circle"
        Nothing -> return ()  -- Success
    Nothing -> assertFailure "Could not create circular list"

-- Test suite
tests = hUnitTestToTests $ TestList
  [ TestLabel "testFromList" testFromList
  , TestLabel "testMove" testMove
  , TestLabel "testInsertAfter" testInsertAfter
  , TestLabel "testRemoveAfter" testRemoveAfter
  , TestLabel "testRemoveOneNode" testRemoveOneNode
  , TestLabel "testRemoveAll" testRemoveAll
  ]

-- Main function to run the tests
main :: IO ()
main = defaultMain tests
