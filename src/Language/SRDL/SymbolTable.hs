{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.SRDL.SymbolTable (
      lkup,
      add,
      SymTab
   ) where

import qualified Data.Map.Strict as M
import Control.Lens
import Control.Lens.At

import qualified Data.Text as T
import Data.Text (Text)

type Scope = [Text]
type SymTab a = M.Map Text (M.Map Text a)

add s n d t = t & at sc . non M.empty . at n ?~ d
    where sc = T.intercalate "," s

lkup :: Eq a => SymTab a -> Scope -> Text -> Maybe (Scope, a)
lkup t s n    =
    case t ^. at sc . non M.empty . at n of
        Just a -> Just (s, a)
        Nothing -> if T.null sc then Nothing else lkup t (init s) n
    where sc = T.intercalate "," s



