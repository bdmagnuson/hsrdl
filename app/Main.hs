{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Debug.Trace

import Parser
import Elab2
import Backends.UVM
import Backends.Verilog
import Data.Maybe (fromJust)

import Data.Text (unpack, pack)

import System.IO
import Control.Lens

import qualified Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Map.Strict as M
import Control.Monad.State
import Debug.Trace
import Options.Applicative
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Maybe (isJust)
import Types (ename, _Fix)

data OutputArg =
     DefaultName
   | SpecifiedName String

data Args = Args
  { input     :: String
  , outputDir :: String
  , svOutput  :: Maybe OutputArg
  , uvmOutput :: Maybe OutputArg
  }

doit :: Args -> IO ()
doit args = do
  res <- parseFile (input args)
  case res of
     Nothing -> return ()
     Just r -> mapM_ f (elab r)
  where f x = do
                 mapM_ (putStrLn . unpack) (getMsgs (snd x))
                 case x of
                  (Nothing, st) -> putStrLn "Errors found.  Exiting..."  >> return ()
                  (Just t, st) -> do
                    --putDoc $ verilog t
                    when (isJust (svOutput  args))
                         (withFile (svName  (svOutput  args) t) WriteMode (flip hPutDoc (verilog t)))
                    when (isJust (uvmOutput args))
                         (withFile (uvmName (uvmOutput args) t) WriteMode (flip hPutDoc (generateUVM . getInstCache $ st)))
        svName (Just args) t =
          case args of
            DefaultName     -> T.unpack $ (t ^. _Fix . ename) <> "_regs.sv"
            SpecifiedName n -> n
        uvmName (Just args) t =
          case args of
            DefaultName     -> T.unpack $ (t ^. _Fix . ename) <> "_uvm_regs.sv"
            SpecifiedName n -> n

main :: IO ()
main = doit =<< execParser (info opts fullDesc)
  where
    opts = Args <$> strOption (long "input" <> metavar "INPUT" <> help "Input SRDL file")
                <*> strOption (long "output" <> metavar "DIR" <> help "Output directory" <> value ".")
                <*> (optional $ (     (SpecifiedName <$> strOption (long "svfile"  <> metavar "FILE" <> help "SV"))
                                  <|> (flag' DefaultName  (long "sv"  <> help "SV"))))
                <*> (optional $ (     (SpecifiedName <$> strOption (long "uvmfile" <> metavar "FILE" <> help "UVM"))
                                  <|> (flag' DefaultName  (long "uvm" <> help "UVM"))))

