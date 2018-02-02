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
import Data.List (partition)

import qualified SymbolTable as ST
import Types
import qualified Data.Text as T
import Props
import Control.Monad (msum)
import Data.Maybe (fromMaybe)

generateUVM :: ST.SymTab (Fix ElabF) -> P.Doc Text
generateUVM e = P.vcat $ map f (M.toList e)
   where f (k, i) = P.vcat $ map (f' k) (M.toList (M.filter (\x -> x ^. _Fix . etype /= Field) i))
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

delim = foldl1 (\x y -> x <> ("_" :: Text) <> y) . tail
sname i = pretty $ delim (i ^. _Fix . escope)

pt :: Text -> P.Doc ann
pt = pretty

uvmClass :: Text -> Text -> Fix ElabF -> P.Doc Text
uvmClass s n i = P.vcat [P.hang 3 (P.vcat [open, decl, new, build]), close]
   where
     open  = pt "class" <+> pretty (delim $ i ^. _Fix . escope) <+> pt "extends" <+> pretty (baseClass i) <> P.semi
     decl  = P.vcat $ map printDecl $ i ^. _Fix . einst
     new   = printNew  (i ^. _Fix . etype) i
     build = printBuild (i ^. _Fix . etype) i
     close = pt "endclass"

printDecl i =
   case i ^. _Fix . etype of
      Array -> c <+> n <> P.brackets (pretty $ length (i ^. _Fix . einst)) <> P.semi
      _ -> c <+> n <> P.semi
   where c = sname i
         n = pretty $ (i ^. _Fix . ename)

printNew Reg r = P.vcat [P.hang 3 (P.vcat [proto, body]), end]
   where proto = pt "function new(input string name = " <> P.dquotes (sname r) <> pt ");"
         body  = pt "super.new(name," <+> (pretty $ getNumProp "regwidth" r) <> pt ", UVM_NO_COVERAGE);"
         end   = pt "endfunction"

printNew _ r = P.vcat [P.hang 3 (P.vcat [proto, body]), end]
   where proto = pt "function new(input string name = " <> P.dquotes (sname r) <> pt ");"
         body  = pt "super.new(name" <> pt ", UVM_NO_COVERAGE);"
         end   = pt "endfunction"


printBuild Reg r = P.vcat [P.hang 3 (P.vcat [proto, create, config]), end]
   where proto = pt "virtual function void build();"
         end = pt "endfunction"
         create = P.vcat $ map (\f -> pretty (f ^. _Fix . ename) <+> pt "= new();") (r ^. _Fix . einst)
         config = P.vcat $ map configline (r ^. _Fix . einst)
         configline f = pretty (f ^. _Fix . ename) <> pt ".configure" <> (P.parens . P.hcat) (P.punctuate P.comma [size, lsb, access, volitile, reset, has_reset, is_rand, ind]) <> P.semi
           where
             size = pretty $ getNumProp "fieldwidth" f
             lsb  = pretty $ getNumProp "lsb" f
             access :: P.Doc Text

             access = P.dquotes $ (pt . T.pack . show . calcAccess) f
             volitile = pretty $ (if hwWritable || counter then 0 else 1 :: Integer)
             hwWritable = any id [getBoolProp "counter" f, getEnumProp "hw" f == "w", getEnumProp "hw" f == "rw"]
             counter = getBoolProp "counter" f
             reset = pretty $ getNumProp "reset" f
             has_reset = pt "1"
             is_rand = pt "1"
             ind = pt "0"

printBuild ctype i = P.vcat [P.hang 3 (P.vcat [proto, sNew, aNew, sAdd, aAdd]), end]
   where proto = pretty ("virtual function void build();" :: Text)
         end = pretty ("endfunction" :: Text)
         (scalar, array)    = partition (\x -> x ^. _Fix . etype /= Array) (i ^. _Fix . einst)
         (scalarR, scalarB) = partition (\x -> x ^. _Fix . etype /= Reg) scalar
         (arrayR, arrayB)   = partition (\x -> x ^? _Fix . einst . ix 0 . _Fix . etype /= Just Reg) array

         n x = pt (x ^. _Fix . ename)
         sNew = P.vcat $ map (\x -> pt (x ^. _Fix . ename) <+> P.equals <+> pt "new();") scalar
         aNew = let n x = pt (x ^. _Fix . ename)
                in P.vcat $ map (\x -> P.hang 3 $ P.vcat ["foreach" <> P.parens (n x <> pt "[i]"), (n x) <> pt "[i]" <+> P.equals <+> pt "new();"]) array


         regAdd' n o = pt "default_map.add_reg" <> (P.parens . P.hcat) (P.punctuate P.comma [n, o, pt "RW"]) <> P.semi
         blkAdd' n o = pt "default_map.add_submap" <> (P.parens . P.hcat) (P.punctuate P.comma [n, o]) <> P.semi

         sAdd = P.vcat $ (map sregAdd scalarR) ++ (map aregAdd arrayR)
         aAdd = P.vcat $ (map sblkAdd scalarB) ++ (map ablkAdd arrayB)

         sgenAdd f x = f (pt (x ^. _Fix . ename)) (pretty (x ^. _Fix . eoffset))
         agenAdd f x = let v = n x <> pt "[i]"
                           b = x ^. _Fix . eoffset
                           s = x ^. _Fix . estride
                           o = pretty b <+> pt "+" <+> pretty s <+> pt "*" <+> pt "i"
                       in P.hang 3 $ P.vcat ["foreach" <> P.parens v, f v o]

         sregAdd x = sgenAdd regAdd' x
         aregAdd x = agenAdd regAdd' x
         sblkAdd x = sgenAdd blkAdd' x
         ablkAdd x = agenAdd blkAdd' x


