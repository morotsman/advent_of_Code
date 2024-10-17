module Challenge1( Coordinate, Path, getPath, traversePath ) where

-- column, row
type Column = Int
type Row = Int
-- offset coordinates as suggested here: https://math.stackexchange.com/questions/2254655/hexagon-grid-coordinate-system
type Coordinate = (Column, Row)

type Path = [String]


-- e, se, sw, w, nw, and ne.
getPath :: String -> Path
getPath pathAsString =
  go pathAsString [] where
    go :: String -> [String] -> [String]
    go "" acc = reverse acc
    go path acc = do
      let maybeDirection = take 2 path
      case maybeDirection of
        "se" -> go (drop 2 path) ("se" : acc)
        "sw" -> go (drop 2 path) ("sw" : acc)
        "nw" -> go (drop 2 path) ("nw" : acc)
        "ne" -> go (drop 2 path) ("ne" : acc)
        (first : _) | first == 'e' ->  go (drop 1 path) ("e" : acc)
        (first : _) | first == 'w' ->  go (drop 1 path) ("w" : acc)

traversePath :: Coordinate -> Path -> [Coordinate]
traversePath originalPosition path =
   go originalPosition path [originalPosition] where
     go :: Coordinate -> Path -> [Coordinate] -> [Coordinate]
     go current [] acc = acc
     go (column, row) (step : rest) acc = do
       case step of
         "e"  -> go (column + 1, row) rest ((column + 1, row) : acc)
         "w"  -> go (column - 1, row) rest ((column - 1, row) : acc)
         "ne" -> if even row
                 then go (column, row - 1) rest ((column, row - 1) : acc)
                 else go (column + 1, row - 1) rest ((column + 1, row - 1) : acc)
         "nw" -> if even row
                 then go (column - 1, row - 1) rest ((column - 1, row - 1) : acc)
                 else go (column, row - 1) rest ((column, row - 1) : acc)
         "se" -> if even row
                 then go (column, row + 1) rest ((column, row + 1) : acc)
                 else go (column + 1, row + 1) rest ((column + 1, row + 1) : acc)
         "sw" -> if even row
                 then go (column - 1, row + 1) rest ((column - 1, row + 1) : acc)
                 else go (column, row + 1) rest ((column, row + 1) : acc)
         _    -> acc  -- handle unexpected steps safely
