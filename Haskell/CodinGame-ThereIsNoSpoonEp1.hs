import System.IO
import Control.Monad
import Data.Functor



type Node = (Int, Int)
type Grid = [Node]

emptyNode :: Node
emptyNode = (-1, -1)

-- |Takes in a serialized grid as a list of strings, each string being a row,
-- and returns a grid containing the nodes present.
deserializeGrid :: [String] -> Grid
deserializeGrid grid = _deserializeGrid grid [] 0
  where
    _deserializeGrid :: [String] -> Grid -> Int -> Grid
    _deserializeGrid []          accumulator _ = accumulator
    _deserializeGrid (line:rest) accumulator y = _deserializeGrid rest (deserializeLine line accumulator 0 y) (y + 1)

    deserializeLine :: String -> Grid -> Int -> Int -> Grid
    deserializeLine []          accumulator _ _ = accumulator
    deserializeLine (node:rest) accumulator x y
      | node == '0' = deserializeLine rest ((x, y) : accumulator) (x + 1) y
      | node == '.' = deserializeLine rest accumulator (x + 1) y
      | otherwise   = error "(CodinGame-ThereIsNoSpoonEp1:deserializeGrid) Unknown node character encountered on deserialization of the grid!"



data NodeWithNeighbors = NodeWithNeighbors { node  :: Node
                                           , below :: Node -- ^The node below.
                                           , right :: Node -- ^The node to the right.
                                           }

instance Show NodeWithNeighbors where
  -- |Converts the node and it's neighbors to the output format required by the
  -- challenge.
  show (NodeWithNeighbors {node = (x1, y1), right = (x2, y2), below = (x3, y3)})
    = foldr1 (\a b -> a ++ " " ++ b) $ map show [x1, y1, x2, y2, x3, y3]

computeNeighbors :: Grid -> Int -> Int -> Node -> NodeWithNeighbors
computeNeighbors grid width height (x, y)
  = NodeWithNeighbors {node = (x, y), below = scan (x, y + 1) 0 1, right = scan (x + 1, y) 1 0}
  where
    -- |Attempts to locate a node. Starts at the given node, and will advance it
    -- by (dx,dy) until a node is found or the search hits the edges of the
    -- grid. Returns the first node found, else an empty node.
    scan :: Node -> Int -> Int -> Node
    scan (x, y) dx dy
      | (x, y) `elem` grid                          = (x, y)
      | x >= 0 && x < width && y >= 0 && y < height = scan (x + dx, y + dy) dx dy
      | otherwise                                   = emptyNode



main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  -- Don't let the machines win. You are humanity's last hope...

  width  <- (getLine <&> read) :: IO Int
  height <- (getLine <&> read) :: IO Int
  grid   <- replicateM height getLine <&> deserializeGrid

  mapM_ (print . computeNeighbors grid width height) grid
