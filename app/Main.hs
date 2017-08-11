{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Debug.Trace

import Parser
import Elab
import Backends.Verilog
import Data.Maybe (fromJust)

import Data.Text (unpack, pack)

import System.IO
import GHC.IO
import Control.Lens

import qualified Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc.Render.Text

import Control.Monad.State

main :: IO ()
main = do
  res <- parseFile "test/srdl/user_prop.srdl"
  if res == Nothing
    then return ()
    else do
      case head $ elab (fromJust res) of
        (Nothing, st) -> mapM_  (putStrLn . unpack) (getMsgs st)
        (Just t, st) -> do
          putDoc $ verilog t
          withFile "out.sv" WriteMode (flip hPutDoc (verilog t))
          mapM_ (putStrLn . unpack)  (getMsgs st)
      return ()

a = unsafePerformIO $ parseFile "test/srdl/user_prop.srdl"
b = head $ elab (fromJust a)
c = fromJust (b ^. _1)

