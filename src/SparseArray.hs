{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module SparseArray (
    lkup,
    deflkup,
    create,
    insert,
    toList,
    SparseArray
    ) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Text.Show.Deriving

data SparseArray a = SparseArray {
    l :: Int,
    m :: Int,
    d :: a,
    a :: M.Map Int a
} deriving (Functor)

$(deriveShow ''SparseArray)
$(deriveShow1 ''SparseArray)
$(deriveShow1 ''M.Map)

lkup :: Int -> SparseArray a -> Maybe a
lkup i sa = do
    guard (i >= l sa)
    guard (i <= m sa)
    M.lookup i (a sa)

deflkup :: Int -> SparseArray a -> a
deflkup k sa = fromMaybe (d sa) (lkup k sa)

create :: Int -> Int -> a -> SparseArray a
create l m d = SparseArray {l = l, m = m, d = d, a = M.empty}

insert :: Int -> a -> SparseArray a -> SparseArray a
insert k v sa = sa { a = M.insert k v (a sa) }

toList :: SparseArray a -> [a]
toList sa = map (flip deflkup $ sa) [(l sa)..(m sa)]
