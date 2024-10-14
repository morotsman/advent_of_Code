{-# LANGUAGE ScopedTypeVariables #-}

module DoubleLinkedList
  ( Node(..)
  , DoubleLinkedList
  , moveForward
  , moveBackward
  , fromList
  , toList
  , insertAfter
  , insertListAfter
  ) where

import Data.IORef

data Node a = Node
  { value :: a
  , prev  :: IORef (Maybe (Node a))
  , next  :: IORef (Maybe (Node a))
  }

-- Define DoubleLinkedList as a pair of (head, last) nodes
type DoubleLinkedList a = (Node a, Node a)

-- Function to create a new node
newNode :: a -> IO (Node a)
newNode val = do
  prevRef <- newIORef Nothing
  nextRef <- newIORef Nothing
  return (Node val prevRef nextRef)

-- Function to link two nodes together
linkNodes :: Node a -> Node a -> IO ()
linkNodes node1 node2 = do
  writeIORef (next node1) (Just node2)
  writeIORef (prev node2) (Just node1)

-- Function to move forward a single step
moveForward :: Node a -> IO (Maybe (Node a))
moveForward node = readIORef (next node)

-- Function to move backward a single step
moveBackward :: Node a -> IO (Maybe (Node a))
moveBackward node = readIORef (prev node)

-- Function to create a doubly linked list from a Haskell list
fromList :: [a] -> IO (Maybe (DoubleLinkedList a))
fromList [] = return Nothing
fromList (x:xs) = do
  headNode <- newNode x
  buildList headNode headNode xs
  where
    buildList :: Node a -> Node a -> [a] -> IO (Maybe (DoubleLinkedList a))
    buildList headNode lastNode [] = return (Just (headNode, lastNode))
    buildList headNode lastNode (y:ys) = do
      newNode <- newNode y
      linkNodes lastNode newNode
      buildList headNode newNode ys

toList :: DoubleLinkedList a -> IO [a]
toList (head, _) = do
  let a = value head
  loop [a] (next head)
  where
    loop :: [a] -> IORef (Maybe (Node a)) -> IO [a]
    loop acc nodeRef = do
      maybeNode <- readIORef nodeRef
      case maybeNode of
        Nothing -> return (reverse acc)
        Just node -> loop (value node : acc) (next node)


-- Insert a node after a given node
insertAfter :: Node a -> Node a -> IO ()
insertAfter node newNode = do
  nextNode <- readIORef (next node)
  writeIORef (next newNode) nextNode
  writeIORef (prev newNode) (Just node)
  writeIORef (next node) (Just newNode)
  case nextNode of
    Just nextNode' -> writeIORef (prev nextNode') (Just newNode)
    Nothing -> return ()

-- Insert an entire DoubleLinkedList after a given node
insertListAfter :: Node a -> DoubleLinkedList a -> IO ()
insertListAfter node (firstNewNode, lastNewNode) = do
  nextNode <- readIORef (next node)
  case nextNode of
    Just nextNode' -> writeIORef (prev nextNode') (Just lastNewNode)
    Nothing -> return ()

  writeIORef (next lastNewNode) nextNode
  writeIORef (prev firstNewNode) (Just node)
  writeIORef (next node) (Just firstNewNode)



