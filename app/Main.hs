module Main where

import Control.Monad.Identity
import Options.Applicative
import Data.Text (unpack)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Void
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as L
import Data.Void
import Debug.Trace

import Language.SRDL

data Args = Args
  { input     :: String
  , outputDir :: String
  , svOutput  :: Bool
  , uvmOutput :: Bool
  , htmlOutput :: Bool
  }

doit :: Args -> IO ()
doit args = do
  srdl <- readSRDL (input args)
  mapM_ f (M.toList srdl)
  where f (n, s) = do
             when (svOutput  args) $ writeVerilog s
             when (uvmOutput args) $ writeUVM     n s
             when (htmlOutput args) $ writeHtml   s

main :: IO ()
main = doit =<< execParser (info opts fullDesc)
  where
    opts = Args <$> strOption (long "input" <> metavar "INPUT" <> help "Input SRDL file")
                <*> strOption (long "output" <> metavar "DIR" <> help "Output directory" <> value ".")
                <*> switch (long "sv")
                <*> switch (long "uvm")
                <*> switch (long "html")



