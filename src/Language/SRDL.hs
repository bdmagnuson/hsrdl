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
import Debug.Trace

import Language.SRDL.Types
import Language.SRDL.Parser
import Language.SRDL.Elab2
import Language.SRDL.Backends.UVM
import Language.SRDL.Backends.Verilog


data SRDL = SRDL ElabState

readSRDL :: FilePath -> IO (M.Map Text SRDL)
readSRDL file = do
  res <- parseSRDL file
  case res of
     Nothing -> return M.empty
     Just r  -> mapM f (elab r) >>= (return . M.fromList . catMaybes)
  where f (Nothing, _) = putStrLn "Errors found.  Exiting..."  >> return Nothing
        f (Just t, st) = do
          mapM_ (putStrLn . unpack) (getMsgs st)
          return (Just (t ^. _Fix . ename, SRDL st))


writeVerilog :: SRDL -> IO ()
writeVerilog (SRDL st) = mapM_ f maps
   where
     maps = filter (\x -> x ^. _Fix . etype == Addrmap) (concatMap M.elems (getInstCache st))
     f x = withFile (unpack (x ^. _Fix . ename) ++ "_regs.sv") WriteMode (flip hPutDoc (verilog x))

writeUVM :: Text -> SRDL -> IO ()
writeUVM n (SRDL st) = withFile (unpack n ++ "_regs_pkg.sv") WriteMode (flip hPutDoc (generateUVM . getInstCache $ st))

