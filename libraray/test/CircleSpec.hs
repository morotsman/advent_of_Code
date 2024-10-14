module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as HashMap
import Circle

-- Test case: Create a circular linked list from a Haskell list and check HashMap
testFromList :: Test
testFromList = TestCase $ do
  (maybeHeadNode, index) <- fromList ([1, 2, 3, 4, 5] :: [Int])
  case maybeHeadNode of
    Just headNode -> do
      firstNext <- moveForward headNode
      case firstNext of
        Just node2 -> assertEqual "Head moves to second node" (value node2) (2 :: Int)
        Nothing -> assertFailure "Expected node2 after head"

      -- Check that the HashMap contains all nodes
      assertBool "HashMap contains node 1" (HashMap.member 1 index)
      assertBool "HashMap contains node 2" (HashMap.member 2 index)
      assertBool "HashMap contains node 3" (HashMap.member 3 index)
      assertBool "HashMap contains node 4" (HashMap.member 4 index)
      assertBool "HashMap contains node 5" (HashMap.member 5 index)

    Nothing -> assertFailure "Could not create circular list"

-- Test case: Insert a node into the circle and check HashMap
testInsertAfter :: Test
testInsertAfter = TestCase $ do
  (maybeHeadNode, index) <- fromList ([1, 3] :: [Int])
  assertBool "HashMap contains node 1" (HashMap.member 1 index)
  assertBool "HashMap contains node 3" (HashMap.member 3 index)
  case maybeHeadNode of
    Just node1 -> do
      (maybeNode2, _) <- fromList ([2] :: [Int])
      case maybeNode2 of
        Just node2 -> do
          finalIndex <- insertAfter node1 node2 index

          -- Check that the node was inserted correctly
          maybeNext <- moveForward node1
          case maybeNext of
            Just node2' -> assertEqual "Insert node after node1" (value node2') (2 :: Int)
            Nothing -> assertFailure "Expected node2 after node1"

          maybeNext2 <- moveForward node2
          case maybeNext2 of
            Just node3' -> assertEqual "Node2 should link to node3" (value node3') (3 :: Int)
            Nothing -> assertFailure "Expected node3 after node2"

          -- Check that the HashMap is updated correctly
          assertBool "finalIndex contains node 1" (HashMap.member 1 finalIndex)
          assertBool "finalIndex contains node 2" (HashMap.member 2 finalIndex)
          assertBool "finalIndex contains node 3" (HashMap.member 3 finalIndex)

        Nothing -> assertFailure "Failed to create node2"
    Nothing -> assertFailure "Failed to create initial circle"

-- Test case: Remove a single node from the circle and check HashMap
testRemoveOneNode :: Test
testRemoveOneNode = TestCase $ do
  (maybeHeadNode, index) <- fromList ([1, 2, 3, 4, 5] :: [Int])
  case maybeHeadNode of
    Just headNode -> do
      (removedNodes, updatedIndex) <- removeAfter headNode 1 index
      assertEqual "Removed nodes" [2] removedNodes

      maybeNext <- moveForward headNode
      case maybeNext of
        Just node3 -> assertEqual "Remaining node should be 3" (value node3) (3 :: Int)
        Nothing -> assertFailure "Expected node3 in circle"

      -- Check that the HashMap is updated correctly
      assertBool "HashMap does not contain removed node 2" (not $ HashMap.member 2 updatedIndex)
      assertBool "HashMap contains node 1" (HashMap.member 1 updatedIndex)
      assertBool "HashMap contains node 3" (HashMap.member 3 updatedIndex)
    Nothing -> assertFailure "Could not create circular list"

-- Test case: Remove multiple nodes from the circle and check HashMap
testRemoveAfter :: Test
testRemoveAfter = TestCase $ do
  (maybeHeadNode, index) <- fromList ([1, 2, 3, 4, 5] :: [Int])
  case maybeHeadNode of
    Just headNode -> do
      -- Remove 3 nodes after the head node
      (removedNodes, updatedIndex) <- removeAfter headNode 3 index
      assertEqual "Removed nodes" [2, 3, 4] removedNodes

      maybeNext <- moveForward headNode
      case maybeNext of
        Just node5 -> assertEqual "Remaining node should be 5" (value node5) (5 :: Int)
        Nothing -> assertFailure "Expected node5 in circle"

      -- Check that the HashMap is updated correctly
      assertBool "HashMap does not contain removed node 2" (not $ HashMap.member 2 updatedIndex)
      assertBool "HashMap does not contain removed node 3" (not $ HashMap.member 3 updatedIndex)
      assertBool "HashMap does not contain removed node 4" (not $ HashMap.member 4 updatedIndex)
      assertBool "HashMap contains node 1" (HashMap.member 1 updatedIndex)
      assertBool "HashMap contains node 5" (HashMap.member 5 updatedIndex)
    Nothing -> assertFailure "Could not create circular list"

-- Test case: Remove all nodes from the circle and check HashMap
testRemoveAll :: Test
testRemoveAll = TestCase $ do
  (maybeHeadNode, index) <- fromList ([1, 2, 3, 4, 5] :: [Int])
  case maybeHeadNode of
    Just headNode -> do
      (removedNodes, updatedIndex) <- removeAfter headNode 5 index
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

      -- Check that the HashMap is empty
      assertBool "HashMap is empty after removing all nodes" (HashMap.null updatedIndex)
    Nothing -> assertFailure "Could not create circular list"

-- Test suite
tests = hUnitTestToTests $ TestList
  [
  TestLabel "testFromList" testFromList
  , TestLabel "testInsertAfter" testInsertAfter
  , TestLabel "testRemoveOneNode" testRemoveOneNode
  , TestLabel "testRemoveAfter" testRemoveAfter
  , TestLabel "testRemoveAll" testRemoveAll
  ]

-- Main function to run the tests
main :: IO ()
main = defaultMain tests
