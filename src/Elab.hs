{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Elab (
   ) where


--import GHC.IO

import Control.Monad.Identity
import Control.Comonad.Cofree
import Control.Comonad
import Control.Lens hiding ((:<))
import qualified Data.Map.Strict as M
import SparseArray as SA
import SymbolTable2 as S
import Data.Functor.Foldable

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Text.Show.Deriving

import Props
import Parser
import Types

type Props = M.Map String (Maybe PropRHS)

data ElabF a = ElabF {
    _etype :: CompType,
    _name  :: String,
    _props :: Props,
    _inst  :: [a]
} deriving (Show, Functor)


$(deriveShow1 ''ElabF)

data Msgs = Msgs {
    _info  :: [String],
    _warn  :: [String],
    _err   :: [String]
} deriving (Show)

$(makeLenses ''Msgs)

data ElabState = ElabState {
    _msgs     :: Msgs,
    _addr     :: Integer,
    _regwidth :: Integer
} deriving (Show)

$(makeLenses ''ElabState)

type ElabS = State ElabState
type Elab = Fix ElabF

emptyMsgs = Msgs [] [] []

data ReaderEnv = ReaderEnv {
    _scope  :: [String],
    _syms   :: S.SymTab2 (Expr SourcePos),
    _sprops :: M.Map CompType (M.Map String Property)
}

makeLenses ''ReaderEnv

rename x = cata f x where
    f (ElabF Field a b c ) = Fix $ ElabF Field "woo" b c
    f x = Fix x


getFields x = cata f x where
    f (ElabF Field n _ _) = [n]
    f (ElabF _ n _ i) = map (\x -> n ++ "." ++ x) (concat i)

getRegs x = cata f x where
    f (ElabF Reg n p _) = [(n, M.lookup "address" p)]
    f (ElabF _ n _ i) = map (\(x, y) -> (n ++ "." ++ x, y)) (concat i)


logMsg t m = lift $ (msgs . t) %= (m:)

--assignFields a@(pos :< CompDef Reg n fs) =
--    case assignBits bits of
--        (_, Left msg) -> do logMsg err msg
--                            return a
--        (_, Right newbits) -> return $ a & _unwrap . inst .~ (zipWith f newbits fs)
--    where bits = map (\x -> (((arr . unwrap) x) ^. _Just) fs
--          f arr field = field & _unwrap . arr .~ (Just arr)


instantiate :: Expr SourcePos -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
instantiate (_ :< CompInst d n Nothing _) = do
    s   <- lift get
    env <- ask
    case S.lkup (env ^. syms) (env ^. scope) d of
        Nothing -> do
            logMsg err ("Lookup failure: " ++ (show d) ++ " in " ++ (show (env ^. scope)))
            return Nothing
        Just (sc, _ :< def) -> withReaderT (scope .~ (sc ++ [d])) $ foldl (>>=) newinst (map elaborate (expr def))
            where
                newdef = ctype def
                newinst = do
                  addr <- lift (use addr)
                  let defprop = M.fromList ((traverse . _2) %~ (^. pdefault) $ M.toList (env ^. sprops . at newdef . _Just)) --LOL
                  let newprop = assignProp "address" (PropNum addr) defprop
                  ereturn $ ElabF newdef n (if (newdef /= Field) then newprop else defprop) []

instantiate (pos :< CompInst d name (Just (ArrWidth n)) a) = do
    x <- mapM (\x -> instantiate (pos :< CompInst d (show x) Nothing a)) [0..(n-1)]
    case (traverse id x) of
        Nothing -> return Nothing
        Just ff -> ereturn $ ElabF Array name M.empty ff

ereturn = return . Just . Fix
assignProp k v m = M.insert k (Just v) m

elaborate _ Nothing = return Nothing


elaborate inst@(pos :< (CompInst cd cn _ _)) (Just (Fix (ElabF t n p i))) = do
    s <- lift get
    new <- instantiate inst
    case new of
        Nothing -> do
            logMsg err ((show pos) ++ ": Failed to instantiate " ++ (show cd))
            return Nothing
        Just a -> do
--                aa <- calcRegLayout a
                gg <- lift (use regwidth)
                when (t == Reg) $ lift (addr += gg)
                ereturn $ ElabF t n p (i ++ [a])

elaborate (_ :< PropAssign path prop rhs) (Just e) = return $ propAssign path prop rhs e
    where
        propAssign'  prop rhs (Fix (ElabF t n p i))  = Fix $ ElabF t n (assignProp prop rhs p) i
        propAssign [] prop rhs e = Just $ propAssign' prop rhs e
        propAssign (t:ts) prop rhs (Fix (ElabF t' n p i))  =
            let update = (mod1 ((== (peName t)) . getName) (propAssign ts prop rhs) i) in
                case update of
                    Nothing -> Nothing
                    Just u -> (Just . Fix) $ ElabF t' n p u
            where
                getName ((Fix (ElabF _ n _ _))) = n

elaborate d@(_ :< CompDef _ n _) e = return e


mod1 p f l = doit [] l p f
  where
        doit ys [] _ _ = Nothing
        doit ys (x:xs) p f
           | p x = case f x of
                     Nothing -> Nothing
                     Just a -> Just ((reverse ys) ++ [a] ++ xs)
           | otherwise = doit (x:ys) xs p f

-- No monoid instance for AST so forced to use ^.. which returns a list
getDef syms scope def = head $ (S.lkup syms scope def) ^.. _Just

elab (ti, syms) = map f ti
    where
        env = ReaderEnv [""] syms M.empty
        st  = ElabState {_msgs = emptyMsgs, _addr = 0, _regwidth = 4}
        f x = runState (runReaderT (instantiate (pos :< CompInst x x Nothing [])) env) st
            where pos = extract $ getDef syms [""] x ^. _2

a =  (elab ret)
f ((Just s,_):_) = s
