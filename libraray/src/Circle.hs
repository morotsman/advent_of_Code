module Circle
  ( Node(..)
  , Circle
  , moveForward
  , moveBackward
  , fromList
  , insertAfter
  , removeAfter
  ) where

import Data.IORef

data Node a = Node
  { value :: a
  , prev  :: IORef (Maybe (Node a))
  , next  :: IORef (Maybe (Node a))
  }

type Circle a = (Node a, Node a)

newNode :: a -> IO (Node a)
newNode val = do
  prevRef <- newIORef Nothing
  nextRef <- newIORef Nothing
  return (Node val prevRef nextRef)

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

-- Function to create a circular linked list from a Haskell list
fromList :: [a] -> IO (Maybe (Circle a))
fromList [] = return Nothing
fromList (x:xs) = do
  headNode <- newNode x
  buildCircle headNode headNode xs
  where
    buildCircle :: Node a -> Node a -> [a] -> IO (Maybe (Circle a))
    buildCircle headNode lastNode [] = do
      -- Link the last node to the head node to form a circle
      linkNodes lastNode headNode
      return (Just (headNode, lastNode))
    buildCircle headNode lastNode (y:ys) = do
      newNode <- newNode y
      linkNodes lastNode newNode
      buildCircle headNode newNode ys

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

-- Remove a specified number of nodes after a given node and return them as a list
removeAfter :: Node a -> Int -> IO [a]
removeAfter _ 0 = return []
removeAfter node n = do
  nextNode <- readIORef (next node)
  case nextNode of
    Just firstNode -> removeNodes node firstNode n
    Nothing -> return []

-- Helper function to remove nodes
removeNodes :: Node a -> Node a -> Int -> IO [a]
removeNodes startNode currentNode 0 = do
  -- Fix the circular links after removal
  nextNode <- readIORef (next currentNode)
  writeIORef (next startNode) nextNode
  case nextNode of
    Just next -> writeIORef (prev next) (Just startNode)
    Nothing -> return ()
  return []
removeNodes startNode currentNode n = do
  -- Recursively remove nodes
  nextNode <- readIORef (next currentNode)
  case nextNode of
    Just next -> do
      rest <- removeNodes startNode next (n - 1)
      return ((value currentNode) : rest)
    Nothing -> return [value currentNode]

