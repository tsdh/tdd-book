data Tree a = Empty
          | Node (Tree a) a (Tree a)
          
%name Tree tree1, tree1, tree2

insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x orig@(Node tree1 y tree2) = case compare x y of
                                       LT => Node (insert x tree1) y tree2
                                       EQ => orig
                                       GT => Node tree1 y (insert x tree2)

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x $ listToTree xs

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right

