{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Backends.UVM
  ( generateUVM
  ) where

import Control.Lens
import Data.Functor.Foldable
import Data.Text.Lazy (toStrict)
import Data.Text.Prettyprint.Doc ((<>), (<+>), pretty)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text (Text)
import qualified Data.Map.Strict as M

import qualified SymbolTable as ST
import Types (ElabF(..), CompType(..), PropRHS(..), IntrType(..))
import qualified Data.Text as T

$(makeLenses ''ElabF)
$(makePrisms ''Fix)

generateUVM :: ST.SymTab (Fix ElabF) -> [[Text]]
generateUVM e = map f (M.toList e)
   where f (k, i) = map (f' k) (M.toList (M.filter (\x -> x ^. _Fix . etype /= Field) i))
         f' k1 (k2, i) = uvmClass k1 k2 i

baseClass :: Fix ElabF -> Text
baseClass inst =
   case inst ^. _Fix . etype of
      Addrmap -> "uvm_reg_block"
      Regfile -> "uvm_reg_block"
      Reg     -> "uvm_reg"
      Field   -> "uvm_field"
      Array   -> "array"
      _       -> error $ show  (inst ^. _Fix . etype)

delim = foldl1 (\x y -> x <> ("_" :: Text) <> y)

uvmClass :: Text -> Text -> Fix ElabF -> Text
uvmClass s n i = (renderStrict . P.layoutPretty P.defaultLayoutOptions) $ P.vcat [open, decl, new, build, close]
   where
     open = pretty ("class" :: Text) <+> pretty (delim $ i ^. _Fix . escope) <+> pretty ("extends" :: Text) <+> pretty (baseClass i) <> P.semi
     decl = P.vcat $ map printDecl $ i ^. _Fix . inst
     new = ""
     build = ""
     close = pretty ("endclass" :: Text)

printDecl i =
   case i ^. _Fix . etype of
      Array -> c <+> n <> P.brackets (pretty $ length (i ^. _Fix . inst)) <> P.semi <+> pretty (i ^. _Fix . offset)
      _ -> c <+> n <> P.semi <+> pretty (i ^. _Fix . offset)
   where c = pretty $ delim (i ^. _Fix . escope)
         n = pretty $ (i ^. _Fix . name)






