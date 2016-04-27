
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

import Control.Monadt Control.Comonad
import Control.Comonad
import Control.Comonad.Cofree

data TreeF a =
     Node Char
   | Branch Char a deriving (Show, Functor)

type Tree a = Cofree TreeF a

genTree 0 a n = a :< Node n
genTree d a n = a :< (Branch n (genTree (d - 1) (succ a) (succ n)))

ffmap f (a :< w) = a :< app w
    where
        app (Branch v w1) = Branch (f a v) (ffmap f w1)
        app (Node v) = Node (f a v)


foome :: (Functor f1) => (f1 a -> f1 a) -> Cofree f1 c -> Cofree f1 c
foome f (a :< b) = a :< fmap (foome f) b

bar (Identifier _) = Identifier "foo"
bar x = x
