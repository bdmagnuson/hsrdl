{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Backends.UVM
  ( generateUVM
  ) where

import Control.Lens
import Control.Monad.State
import Data.Functor.Foldable
import Data.Text.Lazy (toStrict)
import Data.Text.Prettyprint.Doc ((<>), (<+>), pretty)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Text.Blaze.Renderer.Text
import Text.Heterocephalus
import Text.Show.Deriving
import Debug.Trace

import Elab
import Props (assignProp)
import Types (ElabF(..), CompType(..), PropRHS(..), IntrType(..))


generateUVM :: ST.Symtab (Fix ElabF)


