{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Elab (
   ) where


import GHC.IO

import Control.Monad.State
import Control.Monad.Identity
import Control.Comonad.Cofree
import qualified Data.Map.Strict as M
import SparseArray as SA
import SymbolTable as S
import Data.Maybe (fromJust)
import Data.Functor.Foldable

import Props
import Parser
import Types


data Msgs = Msgs {
    info  :: [String],
    warn  :: [String],
    err   :: [String]
} deriving (Show)

data ElabState = ElabState {
    msgs :: Msgs,
    syms :: S.SymTab String (Expr SourcePos),
    sprops :: M.Map CompType (M.Map String Property)
} deriving (Show)

type Props = M.Map String PropRHS

data Elab a
 =  EArray   String Props (SA.SparseArray a)
 |  EContain {
        ename :: String,
        eprops :: Props,
        einst :: [a]
    }
 |  EField   String Props deriving (Functor)

type ElabS a = State ElabState a

--use lenses for this - yuck!
logMsg t m = do
    case t of
        "info"  -> modify (\s -> s {msgs = (msgs s) {info = m : (info (msgs s))}})
        "warn"  -> modify (\s -> s {msgs = (msgs s) {warn = m : (warn (msgs s))}})
        "err"   -> modify (\s -> s {msgs = (msgs s) {err  = m : (err  (msgs s))}})
    return ()

instantiate :: String -> String -> ElabS (Maybe (Fix Elab))
instantiate d n = do
    modify (\s -> s { syms = push (syms s) })
    s <- get
    d <- case S.lkup d (syms s) of
        Nothing -> do
            logMsg "err" ("Lookup failure: " ++ (show n))
            return Nothing
        Just d -> foldl (>>=) (return newinst) (map elaborate (expr (unwrap d)))
            where
                newinst = case ctype (unwrap d) of
                   Addrmap -> (Just . Fix) $ EContain n M.empty []
                   Regfile -> (Just . Fix) $ EContain n M.empty []
                   Reg     -> (Just . Fix) $ EContain n M.empty []
    modify (\s -> s { syms = pop (syms s) })
    return d

ereturn = return . Just . Fix
assignProp = M.insert

elaborate (_ :< CompInst cd cn _ _) (Just (Fix (EContain n p i))) = do
    new <- instantiate cd cn
    case new of
        Nothing -> return Nothing
        Just a -> ereturn $ EContain n p (i ++ [a])

elaborate (_ :< PropAssign path prop rhs) (Just e) = return $ propAssign path prop rhs e
    where
        propAssign'  prop rhs (Fix (EField   n p))    = Fix $ EField n (assignProp prop rhs p)
        propAssign'  prop rhs (Fix (EContain n p i))  = Fix $ EContain n (assignProp prop rhs p) i
        propAssign [] prop rhs e = Just $ propAssign' prop rhs e
        propAssign (t:ts) prop rhs (Fix (EContain n p i))  =
            let update = (mod1 ((== (peName t)) . getName) (propAssign ts prop rhs) i) in
                case update of
                    Nothing -> Nothing
                    Just u -> (Just . Fix) $ EContain n p u
            where
                getName ((Fix (EArray n _ _))) = n
                getName ((Fix (EContain n _ _))) = n
                getName ((Fix (EField n _))) = n

elaborate d@(_ :< CompDef _ n _) e = do
    modify (\s -> s { syms = S.add n d (syms s) })
    return e

elaborate (_ :< d@(PropDef _ _ _ _)) e = do
    modify (\s -> s { sprops = addProperty (sprops s) d })
    return e


mod1 p f l = doit [] l p f
  where
        doit ys [] _ _ = Nothing
        doit ys (x:xs) p f
           | p x = case f x of
                     Nothing -> Nothing
                     Just a -> Just ((reverse ys) ++ [a] ++ xs)
           | otherwise = doit (x:ys) xs p f

foo file = do
   let r = unsafePerformIO (hsrdlParseFile file)
   case r of (Left e) -> error "error"
             (Right p) -> p

getTop :: Expr SourcePos -> State (S.SymTab String (Expr SourcePos))

