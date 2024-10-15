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

-- Test case: Insert a list of nodes into the circle and check both list and HashMap
testInsertListAfter :: Test
testInsertListAfter = TestCase $ do
  (maybeHeadNode, index) <- fromList ([1, 3] :: [Int])
  case maybeHeadNode of
    Just node1 -> do
      -- Insert [2, 4] after node 1
      updatedIndex <- insertListAfter node1 [2, 4] index

      -- Verify the linked structure:
      -- node1 -> node2 -> node4 -> node3 (circular)
      maybeNext1 <- moveForward node1
      case maybeNext1 of
        Just node2 -> do
          assertEqual "Node after node1 should be 2" (value node2) (2 :: Int)

          maybeNext2 <- moveForward node2
          case maybeNext2 of
            Just node4 -> do
              assertEqual "Node after node2 should be 4" (value node4) (4 :: Int)

              maybeNext3 <- moveForward node4
              case maybeNext3 of
                Just node3 -> do
                  assertEqual "Node after node4 should be 3" (value node3) (3 :: Int)

                  -- Check that the circular structure is maintained
                  maybeNext4 <- moveForward node3
                  case maybeNext4 of
                    Just node1' -> assertEqual "Node after node3 should be node1" (value node1') (1 :: Int)
                    Nothing -> assertFailure "Expected node1 after node3 (circular structure)"
                Nothing -> assertFailure "Expected node3 after node4"
            Nothing -> assertFailure "Expected node4 after node2"
        Nothing -> assertFailure "Expected node2 after node1"

      -- Verify the updated HashMap contains all inserted nodes
      assertBool "HashMap contains node 1" (HashMap.member 1 updatedIndex)
      assertBool "HashMap contains node 2" (HashMap.member 2 updatedIndex)
      assertBool "HashMap contains node 4" (HashMap.member 4 updatedIndex)
      assertBool "HashMap contains node 3" (HashMap.member 3 updatedIndex)

    Nothing -> assertFailure "Failed to create initial circle"

-- Test case: Insert a list of nodes into the circle and check both list and HashMap
testInsertListAfterAtEnd :: Test
testInsertListAfterAtEnd = TestCase $ do
  (maybeHeadNode, index) <- fromList ([1] :: [Int])
  case maybeHeadNode of
    Just node1 -> do
      updatedIndex <- insertListAfter node1 [2, 3] index
      -- node1 -> node2 -> node3 -> node1 (circular)
      maybeNext1 <- moveForward node1
      case maybeNext1 of
        Just node2 -> do
          assertEqual "Node after node1 should be 2" (value node2) (2 :: Int)
          maybeNext2 <- moveForward node2
          case maybeNext2 of
            Just node3 -> do
              assertEqual "Node after node2 should be 3" (value node3) (3 :: Int)
              maybeNext1 <- moveForward node3
              case maybeNext1 of
                Just node1 -> do
                  assertEqual "Node after node3 should be 1" (value node1) (1 :: Int)
                Nothing -> assertFailure "Expected node1 after node3"
            Nothing -> assertFailure "Expected node3 after node1"
        Nothing -> assertFailure "Expected node2 after node1"

      -- Verify the updated HashMap contains all inserted nodes
      assertBool "HashMap contains node 1" (HashMap.member 1 updatedIndex)
      assertBool "HashMap contains node 2" (HashMap.member 2 updatedIndex)
      assertBool "HashMap contains node 3" (HashMap.member 3 updatedIndex)

    Nothing -> assertFailure "Failed to create initial circle"

-- Test case: Insert multiple nodes into a non-trivial list and check list and HashMap
testInsertListAfterLargerList :: Test
testInsertListAfterLargerList = TestCase $ do
  (maybeHeadNode, index) <- fromList ([1, 3, 5, 6] :: [Int])
  case maybeHeadNode of
    Just node1 -> do
      -- Move to node3 (the node after node1)
      maybeNode2 <- moveForward node1
      case maybeNode2 of
        Just node3 -> do
          -- Insert [4] after node 3
          updatedIndex <- insertListAfter node3 [4] index

          -- Verify the structure: 1 -> 3 -> 4 -> 5 -> 6 -> 1 (circular)
          maybeNext3 <- moveForward node3
          case maybeNext3 of
            Just node4 -> do
              assertEqual "Node after node3 should be 4" (value node4) (4 :: Int)

              maybeNext4 <- moveForward node4
              case maybeNext4 of
                Just node5 -> do
                  assertEqual "Node after node4 should be 5" (value node5) (5 :: Int)

                  -- Check the circular structure: node5 -> node6 -> node1
                  maybeNext5 <- moveForward node5
                  case maybeNext5 of
                    Just node6 -> do
                      assertEqual "Node after node5 should be 6" (value node6) (6 :: Int)
                      -- Now assign node6 and check the circular structure
                      maybeNext6 <- moveForward node6
                      case maybeNext6 of
                        Just node1' -> assertEqual "Node after node6 should be node1" (value node1') (1 :: Int)
                        Nothing -> assertFailure "Expected node1 after node6 (circular structure)"
                    Nothing -> assertFailure "Expected node6 after node5"
                Nothing -> assertFailure "Expected node5 after node4"
            Nothing -> assertFailure "Expected node4 after node3"
        Nothing -> assertFailure "Failed to move to node3"
    Nothing -> assertFailure "Failed to create initial circle"

-- Test case: Find an existing node in the HashMap
testFindNodeExisting :: Test
testFindNodeExisting = TestCase $ do
  -- Create a circular linked list from the list [1, 2, 3, 4, 5]
  (maybeHeadNode, index) <- fromList ([1, 2, 3, 4, 5] :: [Int])
  case maybeHeadNode of
    Just _ -> do
      -- Try to find node 3 in the map
      let maybeNode3 = findNode 3 index
      case maybeNode3 of
        Just node3 -> assertEqual "Found node with value 3" (value node3) (3 :: Int)
        Nothing -> assertFailure "Expected to find node with value 3"

      -- Try to find node 5 in the map
      let maybeNode5 = findNode 5 index
      case maybeNode5 of
        Just node5 -> assertEqual "Found node with value 5" (value node5) (5 :: Int)
        Nothing -> assertFailure "Expected to find node with value 5"

    Nothing -> assertFailure "Could not create circular list"

-- Test case: Try to find a node that does not exist in the HashMap
testFindNodeNonExisting :: Test
testFindNodeNonExisting = TestCase $ do
  -- Create a circular linked list from the list [1, 2, 3, 4, 5]
  (_, index) <- fromList ([1, 2, 3, 4, 5] :: [Int])

  -- Try to find a node with a value that doesn't exist
  let maybeNode10 = findNode 10 index
  case maybeNode10 of
    Just _ -> assertFailure "Expected not to find node with value 10"
    Nothing -> return ()  -- Success, the node does not exist

-- Test case: Find a node after removing other nodes
testFindNodeAfterRemoval :: Test
testFindNodeAfterRemoval = TestCase $ do
  -- Create a circular linked list from the list [1, 2, 3, 4, 5]
  (maybeHeadNode, index) <- fromList ([1, 2, 3, 4, 5] :: [Int])
  case maybeHeadNode of
    Just headNode -> do
      -- Remove nodes 2 and 3
      (_, updatedIndex) <- removeAfter headNode 2 index

      -- Try to find node 1 (should still exist)
      let maybeNode1 = findNode 1 updatedIndex
      case maybeNode1 of
        Just node1 -> assertEqual "Found node with value 1" (value node1) (1 :: Int)
        Nothing -> assertFailure "Expected to find node with value 1"

      -- Try to find node 2 (should not exist after removal)
      let maybeNode2 = findNode 2 updatedIndex
      case maybeNode2 of
        Just _ -> assertFailure "Expected not to find node with value 2"
        Nothing -> return ()  -- Success, the node has been removed

      -- Try to find node 4 (should still exist)
      let maybeNode4 = findNode 4 updatedIndex
      case maybeNode4 of
        Just node4 -> assertEqual "Found node with value 4" (value node4) (4 :: Int)
        Nothing -> assertFailure "Expected to find node with value 4"

    Nothing -> assertFailure "Could not create circular list"

-- Test case: Ensure the map is empty after all nodes are removed
testFindNodeAfterAllRemoved :: Test
testFindNodeAfterAllRemoved = TestCase $ do
  -- Create a circular linked list from the list [1, 2, 3, 4, 5]
  (maybeHeadNode, index) <- fromList ([1, 2, 3, 4, 5] :: [Int])
  case maybeHeadNode of
    Just headNode -> do
      -- Remove all nodes
      (_, updatedIndex) <- removeAfter headNode 5 index

      -- Try to find any node (none should exist)
      assertBool "HashMap should be empty after removing all nodes" (HashMap.null updatedIndex)

    Nothing -> assertFailure "Could not create circular list"

-- Test suite
tests = hUnitTestToTests $ TestList
  [
  TestLabel "testFromList" testFromList
  , TestLabel "testInsertAfter" testInsertAfter
  , TestLabel "testRemoveOneNode" testRemoveOneNode
  , TestLabel "testRemoveAfter" testRemoveAfter
  , TestLabel "testRemoveAll" testRemoveAll
  , TestLabel "testInsertListAfter" testInsertListAfter
  , TestLabel "testInsertListAfterLargerList" testInsertListAfterLargerList
  , TestLabel "testInsertListAfterAtEnd" testInsertListAfterAtEnd
  , TestLabel "testFindNodeExisting" testFindNodeExisting
  , TestLabel "testFindNodeNonExisting" testFindNodeNonExisting
  , TestLabel "testFindNodeAfterRemoval" testFindNodeAfterRemoval
  , TestLabel "testFindNodeAfterAllRemoved" testFindNodeAfterAllRemoved
  ]

-- Main function to run the tests
main :: IO ()
main = defaultMain tests
