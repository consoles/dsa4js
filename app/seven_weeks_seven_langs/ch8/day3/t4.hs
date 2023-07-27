-- 用Haskell表示一个迷宫（Maze）。你将需要一个Maze类型、一个Node类型以及一个返回给定坐标节点的函数。这个节点应该具有一个到其他节点的出口列表

data Maze a = Maze { nodes :: [(a, [a])] }

getNode :: Eq a => Maze a -> a -> Maybe (a, [a])
getNode maze key = lookup key (nodes maze)
