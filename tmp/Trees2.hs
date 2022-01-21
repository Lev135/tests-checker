module Trees2 where

data Par t = Par [Int] t

instance Functor Par where
    f `fmap` (Par p a) = Par p (f a)

instance Applicative Par where
    pure a = Par mempty a 
    (Par fp ft) <*> (Par ap at) = Par (fp <> ap) (ft at)

instance Monad Par where
    (Par p t) >>= f = Par (p <> p') t'
        where Par p' t' = f t

getPos :: Par t -> Par [Int]
getPos (Par p _) = Par p p

putPos :: [Int] -> Par ()
putPos p = Par p ()

data Tree
    = Leaf 
    | Branch (Par Tree) (Par Tree)

treeEx :: Par Tree
treeEx = Par [0] $ Branch (Par [1, 1] Leaf) (Par [2, 2, 2] Leaf)

data Tree'
    = Leaf'
    | Branch' (Par Tree') (Par Tree')

toTree' :: Tree -> Tree'
toTree' Leaf         = Leaf'
toTree' (Branch a b) = Branch' (toTree' <$> a) (toTree' <$> b)

toTree'2 :: Par Tree -> Par Tree'
toTree'2 pt = do
    p <- getPos pt
    putPos p
    t <- pt
    undefined 

instance Show Tree where 
    show Leaf           = "Leaf"
    show (Branch t1 t2) = "(" ++ show t1 ++ " | " ++ show t2 ++ ")"
instance Show Tree' where 
    show Leaf'           = "Leaf'"
    show (Branch' t1 t2) = "(" ++ show t1 ++ " || " ++ show t2 ++ ")"


instance (Show t) => Show (Par t) where
    show (Par p t') = show t' ++ "<" ++ show p ++ ">"
