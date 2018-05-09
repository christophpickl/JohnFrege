data Graph a = Leaf | Node a (Graph a) (Graph a)

--          6
--        /   \
--       4     2
--      / \   /  \
--     3   9 4    1
--    /            \
--   8              5

graph :: Graph Int
graph = Node 6 nl nr
  where
    nl = Node 4 nll nlr
    nll = Node 3 nlll Leaf
    nlr = Node 9 Leaf Leaf
    nlll = Node 8 Leaf Leaf
    nr = Node 2 nrl nrr
    nrl = Node 4 Leaf Leaf
    nrr = Node 1 Leaf nrrr
    nrrr = Node 5 Leaf Leaf

sumGraph :: Num a => Graph a -> a
sumGraph Leaf = 0
sumGraph (Node x left right) = x + (sumGraph left) + (sumGraph right)

main :: IO ()
main = do
  putStrLn (show (sumGraph graph))
