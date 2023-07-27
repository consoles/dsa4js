-- 使用一个列表monad解决迷宫问题

import Data.List (find)
import Data.Maybe (maybeToList)

data Maze a = Maze { nodes :: [(a, [a])] }

getNode :: Eq a => Maze a -> a -> Maybe (a, [a])
getNode maze key = find (\(node, _) -> node == key) (nodes maze)

findPath :: (Eq a) => Maze a -> a -> a -> [[a]]
findPath maze start end = do
  path <- findPathHelper maze start end []
  return (reverse path)

findPathHelper :: (Eq a) => Maze a -> a -> a -> [a] -> [[a]]
findPathHelper maze current end path
  | current == end = [current:path]
  | otherwise = do
      (next, exits) <- maybeToList (getNode maze current)
      exit <- exits
      if exit `elem` path
        then []
        else findPathHelper maze exit end (current:path)

-- Test case
main :: IO ()
main = do
  let maze = Maze { nodes = [("A", ["B", "C"]),
                             ("B", ["D"]),
                             ("C", ["D", "E"]),
                             ("D", ["E"]),
                             ("E", ["F"]),
                             ("F", [])] }

  putStrLn "Finding paths from A to F..."
  let paths = findPath maze "A" "F"

  putStrLn "Paths found:"
  mapM_ print paths

-- Finding paths from A to F...
-- Paths found:
-- ["A","B","D","E","F"]
-- ["A","C","D","E","F"]
-- ["A","C","E","F"]
