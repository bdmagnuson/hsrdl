module Main where

import Control.Monad
import Debug.Trace

import Parser
import Elab
import Backends.Verilog
import Data.Maybe (fromJust)

import qualified Text.PrettyPrint.Leijen as P

main :: IO ()
main = do
  res <- parseFile "test/srdl/user_prop.srdl"
  if res == Nothing
    then return ()
    else do
      case head $ elab (fromJust res) of
        (Nothing, st) -> mapM_  putStrLn (getMsgs st)
        (Just t, st) -> do
          putStrLn (show res) ;P.putDoc $ verilog' t
          mapM_  putStrLn (getMsgs st)
      return ()
