module Trees where

data Par t p = Par (t p) p

withP :: p -> t p -> Par t p
withP = flip Par

instance Functor t => Functor (Par t) where 
    fmap f (Par t p) = Par (fmap f t) (f p)

instance Applicative t => Applicative (Par t) where
    pure p = Par (pure p) p
    (Par tf pf) <*> (Par ta pa) = Par (tf <*> ta) (pf pa)

data Tree' p
    = Leaf 
    | Branch (Par Tree' p) (Par Tree' p)

instance Functor Tree' where
    fmap f Leaf             = Leaf
    fmap f (Branch t1 t2)   = Branch (fmap f t1) (fmap f t2)


-- Incorrect instance!!!
instance Applicative Tree' where
    pure p = Leaf
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Branch ft1 ft2) <*> (Branch at1 at2) = Branch (ft1 <*> at1) (ft2 <*> at2) 

type Tree p = Par Tree' p

type BoolTree = Tree Bool

boolTreeEx :: Tree Bool
boolTreeEx = withP False $ Branch (withP True Leaf) (withP False Leaf)



instance Show p => Show (Tree' p) where 
    show Leaf           = "Leaf"
    show (Branch t1 t2) = "(" ++ show t1 ++ " | " ++ show t2 ++ ")"

instance (Show p, Show (t p)) => Show (Par t p) where
    show (Par t' p) = show t' ++ "<" ++ show p ++ ">"

