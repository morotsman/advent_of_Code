module Circle
  ( Node(..)
  , moveForward
  , moveBackward
  , fromList
  , insertAfter
  , removeAfter
  , findNode
  ) where

import Data.IORef
import Debug.Trace (trace)
import System.Mem.StableName
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

data Node a = Node
  { value :: a
  , prev  :: IORef (Maybe (Node a))
  , next  :: IORef (Maybe (Node a))
  }

type NodeMap a = HashMap a (Node a)  -- HashMap for O(1) lookup of nodes by value

-- Create a new node and add it to the index
newNode :: (Eq a, Hashable a) => a -> IO (Node a, NodeMap a)
newNode val = do
  prevRef <- newIORef Nothing
  nextRef <- newIORef Nothing
  let node = Node val prevRef nextRef
  return (node, HashMap.singleton val node)

-- Function to link two nodes
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
fromList :: (Eq a, Hashable a) => [a] -> IO (Maybe (Node a), NodeMap a)
fromList [] = return (Nothing, HashMap.empty)
fromList (x:xs) = do
  (headNode, index) <- newNode x
  buildCircle headNode headNode xs index
  where
    buildCircle :: (Eq a, Hashable a) => Node a -> Node a -> [a] -> NodeMap a -> IO (Maybe (Node a), NodeMap a)
    buildCircle headNode lastNode [] index = do
      -- Link the last node to the head node to form a circle
      linkNodes lastNode headNode
      return (Just headNode, index)
    buildCircle headNode lastNode (y:ys) index = do
      (newNode, newIndex) <- newNode y
      linkNodes lastNode newNode
      buildCircle headNode newNode ys (HashMap.union index newIndex)

-- Function to insert a node after a given node
insertAfter :: (Eq a, Hashable a) => Node a -> Node a -> NodeMap a -> IO (NodeMap a)
insertAfter node newNode index = do
  nextNode <- readIORef (next node)
  writeIORef (next newNode) nextNode
  writeIORef (prev newNode) (Just node)
  writeIORef (next node) (Just newNode)
  case nextNode of
    Just nextNode' -> writeIORef (prev nextNode') (Just newNode)
    Nothing -> return ()
  return (HashMap.insert (value newNode) newNode index)

-- Function to find a node by value in O(1)
findNode :: (Eq a, Hashable a) => a -> NodeMap a -> Maybe (Node a)
findNode val index = HashMap.lookup val index

-- Function to remove a specified number of nodes after a given node and update the map
removeAfter :: (Show a, Eq a, Hashable a) => Node a -> Int -> NodeMap a -> IO ([a], NodeMap a)
removeAfter _ 0 index = return ([], index)
removeAfter node n index = do
  nextNode <- readIORef (next node)
  case nextNode of
    Just firstNode -> removeNodes node firstNode n index
    Nothing -> return ([], index)

-- Helper function to check if two nodes are the same by comparing their stable names
nodesAreSame :: Node a -> Node a -> IO Bool
nodesAreSame node1 node2 = do
  sn1 <- makeStableName node1
  sn2 <- makeStableName node2
  return (sn1 == sn2)

-- Helper function to remove nodes and update the map
removeNodes :: (Show a, Eq a, Hashable a) => Node a -> Node a -> Int -> NodeMap a -> IO ([a], NodeMap a)
removeNodes startNode currentNode 0 index = do
  -- Re-link the nodes to maintain circular structure
  writeIORef (next startNode) (Just currentNode)
  writeIORef (prev currentNode) (Just startNode)
  return ([], index)
removeNodes startNode currentNode n index = do
  sameNode <- nodesAreSame startNode currentNode
  let updatedIndex = HashMap.delete (value currentNode) index
  if sameNode
    then do
      writeIORef (next currentNode) Nothing
      writeIORef (prev currentNode) Nothing
      return ([value currentNode], updatedIndex)
    else do
      nextNode <- readIORef (next currentNode)
      case nextNode of
        Just next' -> do
          writeIORef (next currentNode) Nothing
          writeIORef (prev currentNode) Nothing
          (rest, finalIndex) <- removeNodes startNode next' (n - 1) updatedIndex
          return (value currentNode : rest, finalIndex)
        Nothing ->
          return ([value currentNode], updatedIndex)
