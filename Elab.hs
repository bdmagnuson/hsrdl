{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Elab (
   ) where


import Control.Monad.State
import Control.Monad.Identity
import Control.Lens
import qualified Data.Map.Strict as M
import Props
import Parser
import SparseArray as SA
import SymbolTable as S

import Data.Functor.Foldable


data Msgs = Msgs {
    info  :: [String],
    warn  :: [String],
    error :: [String]
}

data ElabState = ElabState {
    syms :: S.SymTab String (Expr SourcePos)
}

type Props = M.Map String PropRHS

data Elab a
 =  Array   Props (SA.SparseArray a)
 |  Addrmap Props [a]
 |  Regfile Props [a]
 |  Reg     Props [a]
 |  Field   Props deriving (Functor)

type ElabS a = State ElabState a

instantiate :: String -> ElabS (Fix Elab)
instantiate = undefined

--type ElabState = SymTab Msgs gg


--instantiate :: Identifier -> ElabS 
--addSymbol :: Identifier -> SExpr -> ElabS ()
--addSymbol n d = do
--   modify $ S.add n d
--   return ()
--
--lookUp n = do
--   symTab <- get
--   case S.lkup n symTab of
--      Just v  -> return v
--      Nothing -> error $ "Undefined component " ++ n
--
--topInst :: SExpr -> Identifier -> ElabS EExpr
--topInst d n = foldl (>>=) (return newInst) (map inst (sexpr d))
--   where newInst = EExpr {
--                     _ectype = sctype d,
--                     _ename  = n,
--                     _einsts  = [],
--                     _eprops  = M.empty
--                   }
--
--inst a@(SPropAssign _ _ _) e = return $ setProp a e
--   where
--      setProp p@(SPropAssign [] prop rhs) e =
--         case (view ectype) e of
--            Array  -> e & einsts %~ map (setProp p)
--            _      -> e & (eprops . at prop) .~ Just rhs
--
--      setProp (SPropAssign ((x, arr):xs) prop rhs) e =
--            e & einsts %~ (map g)
--            where g x'
--                     | view ename x' == x =
--                        case arr of
--                           Nothing -> setProp subProp x'
--                           Just (ArrWidth idx) -> x' & (einsts . element (fromIntegral idx)) %~ setProp subProp
--                     | otherwise = x'
--                  subProp = (SPropAssign xs prop rhs)
--
--inst (SCompInst def name Nothing align) e = do
--   compdef <- lookUp def
--   new <- topInst compdef name
--   return $ (einsts %~ (new:)) e
--
--inst (SCompInst def name (Just (ArrWidth w)) align) e = do
--   compdef <- lookUp def
--   case sctype compdef of
--      Field -> do
--         foo <- topInst compdef name
--         let s = foo & (eprops . at "width") .~ Just (PropNum w)
--         return $ (einsts %~ (s:)) e
--      _ -> do
--         ne <- mapM (topInst compdef) (zipWith (++) (repeat name) (map show [0..(w-1)]))
--         let new = EExpr Array name ne M.empty
--         return $ (einsts %~ (new:)) e
--
--inst d@(SCompDef _ n _) e = do
--   addSymbol n d
--   return e
--
--elab s n = evalState (topInst (simplify s) n) S.empty

