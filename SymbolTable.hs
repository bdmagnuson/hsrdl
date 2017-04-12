
module SymbolTable (
      empty,
      pop,
      push,
      lkup,
      add,
      SymTab
   ) where

import qualified Data.Map.Strict as M
import Control.Monad

type SymTab a b = [M.Map a b]

empty           = [M.empty]
pop (x:xs)      = xs
push            = (:) M.empty
add n d (x:xs)  = (M.insert n d x) : xs
lkup n x        = msum $ map (M.lookup n)  x

