
module SymbolTable (
      empty,
      pop,
      push,
      lkup,
      add,
      SymTab
   ) where

import qualified Data.Map.Strict as M

type SymTab a b = [M.Map a b]

empty      = [M.empty]
pop (x:[]) = empty
pop (x:xs) = xs
push       = (:) M.empty
add n d (x:xs) = (M.insert n d x) : xs

lkup n (x:[]) = M.lookup n x
lkup n (x:xs) = case M.lookup n x of
                   Nothing -> lkup n xs
                   Just a -> Just a

