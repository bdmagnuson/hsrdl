{-# LANGUAGE OverloadedStrings #-}

module Language.SRDL.Backends.Html
  ( genHtml
  ) where

import Control.Lens

import           Data.Functor.Foldable
import           Data.Monoid ((<>))

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!), table, tr, td, toValue, toHtml, link)
import           Text.Blaze.Html5.Attributes (rowspan, href, type_, rel)
import Text.Blaze.Html.Renderer.String

import Language.SRDL.Elab2
import Language.SRDL.Props
import Language.SRDL.Types

genHtml t = renderHtml $ do
              H.docTypeHtml $ do
                H.head $ do
                  link ! rel "stylesheet" ! type_ "text/css" ! href "regs.css"
                table (mconcat $ map renderReg (getRegs t))

renderReg :: Fix ElabF -> H.Html
renderReg r = tr $ (td ! rowspan nFields) regName <> rows
  where regName = toHtml (r ^. _Fix . ename)
        rows = let (f:fs) = fields
               in renderField f <> mconcat (map (tr . renderField) fs)
        fields = getFields r
        nFields = toValue (length fields)


renderField :: Fix ElabF -> H.Html
renderField f = mconcat $ map td [bits, name, desc, reset]
   where bits = toHtml (getNum f "fieldwidth")
         name = toHtml (f ^. _Fix . ename)
         desc = toHtml (getLit f "desc")
         reset = toHtml (getNum f "reset")

