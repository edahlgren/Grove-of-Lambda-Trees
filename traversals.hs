-- | A module for 4 Haskell binary tree traversals: preorder, postorder, inorder, levelorder
-- -- Three operations are supported for each: 
-- -- 1. generate list of node values in traversal order
-- -- 2. generate list of only the node values that fit a value predicate
-- -- 3. generate list of only the node values that fit a structural predicate (only leafs, only internals, only right branches, etc)
-- 
-- Authored by Erin Dahlgren
-- 
-- December 28 2011
-- 
-- |



data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

sample_tree = Node 'c' 
              (Node 'b' (Node 'a' Empty Empty) Empty) 
              (Node 'd' (Node 'e' Empty Empty) (Node 'f' Empty Empty))
-- |	       c
-- |	      / \
-- |	     b	 d
-- |        /   / \
-- |	   a   e   f


-- | preorder
-- | visit the root, traverse the left subtree, traverse the right
-- | c -> b -> a -> d -> e -> f
preorder Empty = []
preorder (Node x left right) = [x] ++ (preorder left) ++ (preorder right)

-- | list of all values that pass the value predicate pred
preorder_val pred Empty = []
preorder_val pred (Node x left right)
  | (pred) x = [x] ++ (preorder_val pred left) ++ (preorder_val pred right)
  | otherwise = (preorder_val pred left) ++ (preorder_val pred right)

-- | list of all values that pass the structural left predicate predl 
-- | and the structural right predicate predr
-- | (use this to find, for instance, all leaf node values
-- |  where predl = (==Empty) and predr = (==Empty), etc.)
preorder_struc predl predr Empty = []
preorder_struc predl predr (Node x left right)
  | (predl) left && (predr) right = [x] ++ (preorder_struc predl predr left) 
                                    ++ (preorder_struc predl predr right)
  | otherwise = (preorder_struc predl predr left) 
                ++ (preorder_struc predl predr right)


-- | postorder
-- | traverse the left subtree, traverse the right, visit the root
-- | a -> b -> e -> f -> d -> c 
postorder Empty = []
postorder (Node x left right) = (postorder left) ++ (postorder right) ++ [x]

-- | postorder value filter, using value predicate pred
postorder_val pred Empty = []
postorder_val pred (Node x left right)
  | (pred) x = (postorder_val pred left) ++ (postorder_val pred right) ++ [x]
  | otherwise = (postorder_val pred left) ++ (postorder_val pred right)
                
-- | postorder structure filter, using structural predicates predl and predr
postorder_struc predl predr Empty = []
postorder_struc predl predr (Node x left right)
  | (predl) left && (predr) right = (postorder_struc predl predr left) ++ 
                                    (postorder_struc predl predr right) ++ [x]
  | otherwise = (postorder_struc predl predr left) 
                ++ (postorder_struc predl predr right)


-- | inorder  
-- | traverse the left subtree, visit the root, traverse the right subtree
-- | a -> b -> c -> e -> d -> f
inorder Empty = []
inorder (Node x left right) = (inorder left) ++ [x] ++ (inorder right)

-- | inorder value filter, using value predicate pred
inorder_val pred Empty = []
inorder_val pred (Node x left right)
  | (pred) x = (inorder_val pred left) ++ [x] ++ (inorder_val pred right)
  | otherwise = (inorder_val pred left) ++ (inorder_val pred right)
                
-- | inorder structural filter, using structural predicates predl and predr
inorder_struc predl predr Empty = []
inorder_struc predl predr (Node x left right)
  | (predl) left && (predr) right = (inorder_struc predl predr left) ++
                                    [x] ++ (inorder_struc predl predr right)
  | otherwise = (inorder_struc predl predr left)  
                ++ (inorder_struc predl predr right)
                

-- | levelorder  
-- | traverse each level of the tree, moving down levels
-- | c -> b -> d -> a -> e -> f

-- | list of values
valuelist Empty = []
valuelist (Node x left right) = [x]

-- | list of subtrees
subtlist Empty = []
subtlist (Node x left right) = [left, right]

-- | concat tree values by each level's valuelist, then subtlist, and so on
levelorder t = level [t]
               where
                 level [] = []
                 level ts = concatMap valuelist ts 
                            ++ level (concatMap subtlist ts)

-- | levelorder value filter, by value predicate pred 
valuelist_val pred Empty = []
valuelist_val pred (Node x left right)
  | (pred) x = [x]
  | otherwise = []

levelorder_val pred t = level pred [t]                 
               where
                level pred [] = []
                level pred ts = concatMap (valuelist_val pred) ts
                                ++ level pred (concatMap subtlist ts)

-- | levelorder structural filter, by subtree predicates predl and predr
valuelist_struc predl predr Empty = []
valuelist_struc predl predr (Node x left right)
  | (predl) left && (predr) right = [x]
  | otherwise = []

levelorder_struc predl predr t = level predl predr [t]
                 where
                  level predl predr [] = []
                  level predl predr ts = concatMap (valuelist_struc predl predr) ts
                                         ++ level predl predr (concatMap subtlist ts)





