module Language.SRDL
  ( readSRDL
  , writeVerilog
  , writeUVM
  , SRDL
  ) where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import System.IO
import Data.Functor.Foldable

import Language.SRDL.Types
import Language.SRDL.Parser
import Language.SRDL.Elab2
import Language.SRDL.Backends.UVM
import Language.SRDL.Backends.Verilog


newtype SRDL = SRDL ((Fix ElabF),  ElabState)

readSRDL :: FilePath -> IO (M.Map Text SRDL)
readSRDL file = do
  res <- parseSRDL file
  case res of
     Nothing -> return M.empty
     Just r  -> mapM f (elab r) >>= (return . M.fromList . catMaybes)
  where f x = do
               mapM_ (putStrLn . unpack) (getMsgs (snd x))
               case x of
                (Nothing, st) -> putStrLn "Errors found.  Exiting..."  >> return Nothing
                (Just t, st) -> return (Just (t ^. _Fix . ename, SRDL (t, st)))


writeVerilog :: FilePath -> SRDL -> IO ()
writeVerilog f (SRDL (s, _)) = withFile f WriteMode (flip hPutDoc (verilog s))

writeUVM :: FilePath -> SRDL -> IO ()
writeUVM f (SRDL (_, st)) = withFile f WriteMode (flip hPutDoc (generateUVM . getInstCache $ st))

