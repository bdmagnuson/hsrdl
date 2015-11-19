{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Elab (
     EExpr(..)
   , elab
   ) where

import Parser

import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import Control.Lens

import SymbolTable as S

data SExpr =
     SCompDef   {
        sctype :: CompType,
        sname  :: Identifier,
        sexpr  :: [SExpr]
     }
   | SCompInst   Identifier Identifier (Maybe Array) [Alignment]
   | SPropAssign PropRef PropRHS deriving (Show)

data EExpr =
     EExpr {
        _ectype :: CompType,
        _ename  :: Identifier,
        _einsts  :: [EExpr],
        _eprops  :: M.Map String PropRHS
     } deriving (Show)

makeLenses ''EExpr

-- Covert richer tree into something easier to walk for elaboration
-- Turn all property assignments into common format
-- Turn all anonymous component instances into standalone expressions
-- Also make comma sperated instances standalone expressions

simplify :: Expr -> SExpr
simplify =
   let
      fn d (CompInst a b c)              = SCompInst d a b c
      simplify' (DefPropAssign i)        = [SPropAssign (PropRef [] i) (PropBool True)]
      simplify' (ExpPropAssign i r)      = [SPropAssign (PropRef [] i) r]
      simplify' (PostPropAssign l r)     = [SPropAssign l r]
      simplify' (CompDef t (Just d) e i) = [SCompDef t d ((concatMap simplify' e))] ++ (map (fn d) i)
      simplify' (ExpCompInst d i)        = map (fn d) i
   in
      head . simplify'

type ExprS a = StateT a Identity Expr
-- Give names to all anonymous declrations
setName :: Expr -> ExprS Int
setName (CompDef a Nothing b c) = do
   x <- get
   put (x + 1)
   d <- mapM setName b
   return (CompDef a (Just ("__anon" ++ (show x))) d c)

setName (CompDef a (Just e) b c) = do
   d <- mapM setName b
   return (CompDef a (Just e) d c)

setName x = return x

type ElabS a = State ElabState a
type ElabState = S.SymTab Identifier SExpr

addSymbol :: Identifier -> SExpr -> ElabS ()
addSymbol n d = do
   modify $ S.add n d
   return ()

lookUp n = do
   symTab <- get
   case S.lkup n symTab of
      Just v  -> return v
      Nothing -> error $ "Undefined component " ++ n

topInst :: SExpr -> Identifier -> ElabS EExpr
topInst d n = do
   let newInst = EExpr {
      _ectype = sctype d,
      _ename  = n,
      _einsts  = [],
      _eprops  = M.empty
   }
   foldl (>>=) (return newInst) (map inst (sexpr d))

inst a@(SPropAssign _ _) e = return $ setProp a e
   where
      setProp p@(SPropAssign (PropRef [] prop) rhs) e =
         case (view ectype) e of
            Array  -> e & einsts %~ map (setProp p)
            _      -> e & (eprops . at prop) .~ Just rhs

      setProp (SPropAssign (PropRef ((x, arr):xs) prop) rhs) e =
            e & einsts %~ (map g)
            where g x'
                     | view ename x' == x =
                        case arr of
                           Nothing -> setProp subProp x'
                           Just (ArrWidth idx) -> x' & (einsts . element (fromIntegral idx)) %~ setProp subProp
                     | otherwise = x'
                  subProp = (SPropAssign (PropRef xs prop) rhs)

inst (SCompInst def name Nothing align) e = do
   compdef <- lookUp def
   new <- topInst compdef name
   return $ (einsts %~ (new:)) e

inst (SCompInst def name (Just (ArrWidth w)) align) e = do
   compdef <- lookUp def
   case sctype compdef of
      Field -> do
         foo <- topInst compdef name
         let s = foo & (eprops . at "width") .~ Just (PropNum w)
         return $ (einsts %~ (s:)) e
      _ -> do
         ne <- mapM (topInst compdef) (zipWith (++) (repeat name) (map show [0..(w-1)]))
         let new = EExpr Array name ne M.empty
         return $ (einsts %~ (new:)) e

inst d@(SCompDef _ n _) e = do
   addSymbol n d
   return e

elab s n = evalState (topInst simple n) S.empty
   where
      simple = simplify $ evalState (setName s) 0



