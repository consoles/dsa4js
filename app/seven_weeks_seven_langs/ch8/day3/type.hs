-- 类和类型
data Suit = Spades | Hearts deriving (Show)
data Rank = Ten | Jack | Queen | King | Ace deriving (Show)
type Card = (Rank, Suit)
type Hand = [Card]
value :: Rank -> Integer
value Ten = 1
value Jack = 2
value Queen = 3
value King = 4
value Ace = 5

-- 函数和多态
data Triplet a = Trio a a a deriving (Show)

-- 递归类型
data Tree a = Children [Tree a] | Leaf a deriving (Show)
depth (Leaf _) = 1
depth (Children c) = 1 + maximum (map depth c)

cardValue :: Card -> Integer
cardValue (rank, suit) = value rank

-- monad
-- monad 主要有 3 个组成元素，包含类型容器、return 和 bind
data Position t = Position t deriving (Show) -- 类型容器
stagger (Position d) = Position (d + 2)
crawl (Position d) = Position (d + 1)
rtn x = x -- return
x >>== f = f x -- bind
treasureMap pos = pos >>== 
                    stagger >>== 
                    stagger >>== 
                    crawl >>== 
                    rtn

-- 从控制台读取一行文字并其翻转输出
tryIo = do putStr "Enter a name: ";
            line <- getLine;
            let { backwards = reverse line };
            return ("Hello. Your name backwards is " ++ backwards)       

main = do
    let r = cardValue(Ten, Hearts)
    -- 1
    print r

    -- Trio 'A' 'B' 'C'
    print(Trio 'A' 'B' 'C')

-- 构造一颗只有一个叶子节点的树
    let leaf = Leaf 1
    -- Leaf 1
    print leaf
    let (Leaf value) = leaf
    -- 1
    print value
-- 构建一些更复杂的树
    -- Children [Leaf 1,Children [Leaf 2,Leaf 3]]
    let tree = Children[Leaf 1, Children [Leaf 2, Leaf 3]]
    print tree
    let (Children ch) = tree
    let (fst:tail) = ch
    -- Leaf 1
    print fst
    -- [Children [Leaf 2,Leaf 3]]
    print tail
    -- 3
    print (depth tree)
    -- 1
    print (depth fst)

    -- Position 5
    print (treasureMap (Position 0))

-- monad 和 do 记号
    -- 从列表 xs 中得到 x，从列表 ys 中得到 y，然后得到 x y 的各种组合
    let cartesian (xs, ys) = do x <- xs; y <- ys; return (x, y)
    -- [(1,2),(1,3),(1,4),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4)]
    print (cartesian([1..3],[2..4]))
