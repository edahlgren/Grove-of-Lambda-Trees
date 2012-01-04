-- | A module for basic tree building and changing techniques.  Balanced trees are explored.
-- --
-- -- Authored by Erin Dahlgren
-- --
-- -- December 23 2011
-- --
-- -- |



-- | define datatype Tree
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)


-- | maps a function to all values in the tree, not just leaves.
treemap f Empty = Empty
treemap f (Node x left right) = Node (f x) (treemap f left) (treemap f right)


-- | Is value x somewhere in the tree?
tree_member :: (Ord a) => a -> Tree a -> Bool
tree_member x Empty = False
tree_member x (Node a left right)
	     | x == a = True
	     | x < a = tree_member x left
	     | x > a = tree_member x right


-- | check if a value x is a member of tree
tree_check_member val tree = val `tree_member` tree


-- | define a single node tree, where x is the value at that node
-- | This also defines a leaf
tree_single :: a -> Tree a
tree_single x = Node x Empty Empty


-- | insert a new node with value x, maintaining a binary tree
-- | This does NOT ensure a balanced tree
tree_insert :: (Ord a) => a -> Tree a -> Tree a
tree_insert x Empty = tree_single x
tree_insert x (Node a left right)
       | x == a = Node x left right
       | x < a = Node a (tree_insert x left) right
       | x > a = Node a left (tree_insert x right)


-- | tranform a list l into an unbalanced binary tree
list_to_tree xs = foldr tree_insert Empty xs



-- | improve upon this by generating a BALANCED binary tree from a list

-- | split a list into two parts, stored in a tuple
split xs 0 = ([], xs)
split (x:xs) n = let (f,l) = split xs (n-1) in (x : f, l)


-- | transform a list (x:xs), of any type signature, into an BALANCED binary tree
-- | more than one BALANCED binary tree may result
tree_bal 0 [] = [Empty]
tree_bal n (x:xs) = let (q,r) = (n-1) `quotRem` 2
	  in [Node x left right | i     <- [q .. q+r],
	     	     	  	  left  <- tree_bal i (fst (split xs i)),
				  right <- tree_bal (n-i-1) (snd (split xs i))]


-- | TODO
-- | I'm working on extending tree_bal to an insert and delete function that maintains balance
-- | I'm getting inspiration from AA trees








