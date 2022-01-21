module Trees3 () where

class Box p where
    unBox :: p a -> a

(<$.>) :: (Functor f, Functor p, Box p) => (a -> f b) -> p a -> f (p b)
f <$.> a = (<$ a) <$> unBox (f <$> a)

($$) :: Box p => (a -> b) -> p a -> b
f $$ pa = f $ unBox pa


type Wrap t p = p (t p)

data BinTree' p
    = Leaf
    | Branch (Wrap BinTree' p) (Wrap BinTree' p)

instance Box p => Show (BinTree' p) where
    show Leaf           = "()"
    show (Branch t1 t2) = "[" ++ show $$ t1 ++ show $$ t2 ++  "]" 

height :: Box p => BinTree' p -> Int
height Leaf             = 0
height (Branch t1 t2)   = 1 + max (height $$ t1) (height $$ t2)

data Pos t = Pos [Int] t

instance Box Pos where
    unBox (Pos _ a) = a

type BinTree = Pos (BinTree' Pos)

tree :: Wrap BinTree' Pos
tree = Pos [1, 2] $ Branch (Pos [] Leaf) (Pos [] $ Branch (Pos [] Leaf) (Pos [] Leaf))

heightEx :: Int
heightEx = height $$ tree
