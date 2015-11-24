module Hsrdl () where

import Parser
import Elab
import TestStrings

import Text.Show.Pretty

pp t = do
   let r = hsrdlParseString t in
      case r of (Left e) -> error (show e)
                (Right []) -> error "no expr"
                (Right p) -> head p

ss n x = elab (pp x) n


test file = do
   r <- hsrdlParseFile file
   case r of (Left e) -> error $ (show e)
             (Right []) -> error "no expr"
             (Right p) -> (putStrLn . show) $ ppDoc $ elab (head p) "mytop"
