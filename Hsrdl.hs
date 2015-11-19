module Hsrdl () where

import Parser
import Elab
import TestStrings

pp t = do
   let r = hsrdlParseString t in
      case r of (Left e) -> error (show e)
                (Right p) -> p

ss x = elab (pp x) "mytop"
