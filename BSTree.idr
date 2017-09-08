data BSTree : Type -> Type where
     BSEmpty : Ord a => BSTree a 
     BSNode : Ord a => BSTree a -> a -> BSTree a -> BSTree a
     
%name BSTree tree, tree1, tree2

insert : a -> BSTree a -> BSTree a
insert x BSEmpty = BSNode BSEmpty x BSEmpty
insert x orig@(BSNode tree1 y tree2) = case compare x y of
                                       LT => BSNode (insert x tree1) y tree2
                                       EQ => orig
                                       GT => BSNode tree1 y (insert x tree2)
