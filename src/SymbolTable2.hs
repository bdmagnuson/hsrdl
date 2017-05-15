{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module SymbolTable2 (
      lkup,
      add,
      SymTab2,
      empty
   ) where

import qualified Data.Map.Strict as M
import Control.Lens

type Scope = [String]
type Identifier = String
type SymTab2 a = M.Map Scope (M.Map Identifier a)

empty         = M.empty
add  t s n d  = t &  at s . non M.empty . at n ?~ d
lkup t s n    =
    case t ^. at s . non M.empty . at n of
        Just a -> Just (s, a)
        Nothing -> if null s then Nothing else lkup t (init s) n



