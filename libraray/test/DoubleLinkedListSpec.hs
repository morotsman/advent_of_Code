module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit
import DoubleLinkedList

-- Test case: Create a doubly linked list from a Haskell list
testFromList :: Test
testFromList = TestCase $ do
  fromList [1, 2, 3, 4, 5] >>= maybe
    (assertFailure "Failed to create doubly linked list")
    (\dList -> do
        list <- toList dList
        assertEqual "Convert DoubleLinkedList to list" [1, 2, 3, 4, 5] list
    )

-- Test case: Convert a DoubleLinkedList to a Haskell list
testToList :: Test
testToList = TestCase $ do
  maybeDList <- fromList [1, 2, 3, 4, 5]

  case maybeDList of
    Just dList -> do
      list <- toList dList
      assertEqual "Convert DoubleLinkedList to list" [1, 2, 3, 4, 5] list
    Nothing -> assertFailure "Failed to create DoubleLinkedList"

-- Test case: Moving forward and backward
testMove :: Test
testMove = TestCase $ do
  fromList [1, 2, 3] >>= maybe
    (assertFailure "Failed to create doubly linked list")
    (\(node1, node3) -> do
        moveForward node1 >>= maybe
          (assertFailure "Expected node2 as next")
          (\node2 -> assertEqual "Move forward from node1" 2 (value node2))

        moveBackward node3 >>= maybe
          (assertFailure "Expected node2 as previous")
          (\node2 -> assertEqual "Move backward from node3" 2 (value node2))
    )

-- Test case: Insert a node after another node
testInsertAfter :: Test
testInsertAfter = TestCase $ do
  fromList [1, 3] >>= maybe
    (assertFailure "Failed to create initial list")
    (\dList -> do
        fromList [2] >>= maybe
          (assertFailure "Failed to create node2 list")
          (\(node2, _) -> do
              let (node1, node3) = dList
              insertAfter node1 node2
              list <- toList dList
              assertEqual "Insert after node1" [1, 2, 3] list
          )
    )

-- Test case: Insert an entire list after a given node
testInsertListAfter :: Test
testInsertListAfter = TestCase $ do
  fromList [1, 2, 3] >>= maybe
    (assertFailure "Failed to create initial list")
    (\dList -> do
        moveForward (fst dList) >>= maybe
          (assertFailure "Expected node2 as next")
          (\node2 -> do
              fromList [4, 5, 6] >>= maybe
                (assertFailure "Failed to create new list")
                (\listToInsert -> do
                    insertListAfter node2 listToInsert
                    list <- toList dList
                    assertEqual "Insert list after node2" [1, 2, 4, 5, 6, 3] list
                )
          )
    )

tests = hUnitTestToTests $ TestList
  [ TestLabel "testFromList" testFromList
  , TestLabel "testToList" testToList
  , TestLabel "testMove" testMove  -- Unchanged
  , TestLabel "testInsertAfter" testInsertAfter
  , TestLabel "testInsertListAfter" testInsertListAfter
  ]

main :: IO ()
main = defaultMain tests
