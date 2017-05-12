{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Elab (
   out
   ) where


--import GHC.IO

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Comonad.Cofree
import Control.Comonad
import Control.Lens hiding ((:<))

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import SymbolTable2 as S
import Data.Functor.Foldable
import Data.Maybe (isJust)
import Props
import Parser
import Types

import Text.Show.Deriving

$(makePrisms ''Fix)
$(makeLenses ''ElabF)
$(deriveShow1 ''ElabF)

type instance IxValue (Fix ElabF) = Fix ElabF
type instance Index (Fix ElabF) = String

instance Ixed (Fix ElabF) where
    ix k f m = case break (\x -> (unfix x) ^. Elab.name == k) ((unfix m) ^. inst) of
                (_, []) -> pure m
                (i, l:ls) -> f l <&> \x -> Fix $ (unfix m) & inst .~ (i ++ (x:ls))

data Msgs = Msgs {
    _info  :: [String],
    _warn  :: [String],
    _err   :: [String]
} deriving (Show)

$(makeLenses ''Msgs)

data ElabState = ElabState {
    _msgs     :: Msgs,
    _addr     :: Integer,
    _regwidth :: Integer,
    _usedbits :: Set.Set Integer,
    _nextbit  :: Integer
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
    f (ElabF Field a b c _ _) = Fix $ ElabF Field "woo" b c 0 0
    f x = Fix x


getFields x = cata f x where
    f (ElabF Field n _ _ _ _) = [n]
    f (ElabF _ n _ i _ _) = map (\x -> n ++ "." ++ x) (concat i)

getRegs x = cata f x where
    f (ElabF Reg n p _ _ _) = [(n, M.lookup "address" p)]
    f (ElabF _ n _ i _ _) = map (\(x, y) -> (n ++ "." ++ x, y)) (concat i)


logMsg t m = lift $ (msgs . t) %= (m:)


assignBits :: (Maybe Array) -> Maybe (Fix ElabF) -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
assignBits _ Nothing = return Nothing
assignBits Nothing r = assignBits (Just (ArrWidth 1)) r
assignBits (Just arr) (Just (Fix reg)) = do
    used <- lift (use usedbits)
    nb   <- lift (use nextbit)
    let (l, r, set) = case arr of
          ArrLR l r  -> (l, r, Set.fromList $ range l r)
          ArrWidth w -> (nb + w - 1, nb, Set.fromList $ range nb (nb+w-1))
    let intersection = Set.intersection used set
    let union        = Set.union used set
    if null intersection
        then do
            lift (nextbit .= l + 1)
            lift (usedbits .= union)
            ereturn $ (reg & lsb .~ r) & (msb .~ l) & (props %~ assignProp "fieldwidth" (PropNum (l - r + 1)))
        else do
            logMsg err ("Field overlap on bits " ++ (show . Set.toList) intersection)
            return Nothing
    where
        range x y = if (x < y) then [x..y] else [y..x]

resetBits = do
    lift (nextbit .= 0)
    lift (usedbits .= Set.empty)
    return ()

instantiate :: Expr SourcePos -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
instantiate (pos :< CompInst d n arr a) = do
    s   <- lift get
    env <- ask
    case S.lkup (env ^. syms) (env ^. scope) d of
        Nothing -> do
            logMsg err ("Lookup failure: " ++ (show d) ++ " in " ++ (show (env ^. scope)))
            return Nothing
        Just (sc, _ :< def) ->
          case (ctype def, arr) of
            (Field, _)      -> (foo newinst >>= assignBits arr)
            (_, Just (ArrWidth w)) -> do x <- mapM (\x -> instantiate (pos :< CompInst d (show x) Nothing a)) [0..(w-1)]
                                         case (traverse id x) of
                                           Nothing -> return Nothing
                                           Just ff -> ereturn $ ElabF Array n M.empty ff 0 0
            (Reg, Nothing) -> (foo (newinst >>= assignAddress)) <* incrAddress <* resetBits
            otherwise -> (foo newinst) <* incrAddress
         where
           foo x   = withReaderT (scope .~ (sc ++ [d])) $ foldl (>>=) x (map elaborate (expr def))
           newinst = ereturn $ ElabF {
             _etype = (ctype def),
             _name  = n,
             _props =  M.fromList ((traverse . _2) %~ (^. pdefault) $ M.toList (env ^. sprops . at (ctype def) . _Just)),
             _inst  = [],
             _lsb   = 0,
             _msb   = 0}
           assignAddress (Just (Fix a)) = do
             b <- lift (use addr)
             ereturn $ a & props %~ (assignProp "address" (PropNum b))
           incrAddress = do
              gg <- lift (use regwidth)
              lift (addr += gg)
              return ()

elaborate _ Nothing = return Nothing
elaborate ins@(pos :< (CompInst cd cn _ _)) (Just i) = do
    s <- lift get
    if (isJust $ i ^? ix cn)
        then do
          logMsg err ((show pos) ++ cn ++  " already defined")
          return Nothing
        else do
          new <- instantiate ins
          case new of
            Nothing -> do
              logMsg err ((show pos) ++ ": Failed to instantiate " ++ (show cd))
              return Nothing
            Just a -> (return . Just) $ (i & _Fix . inst %~ (++ [a]))

elaborate (pos :< PropAssign path prop rhs) (Just e) =
    case buildPropLens e (map peName path) of
        Left elm -> do
            logMsg err ((show pos) ++ ": invalid path, failed at \"" ++ (show elm) ++ "\"")
            return Nothing
        Right l -> do
            (return . Just)  $ e & l . _Fix . props %~ assignProp prop rhs

elaborate d@(_ :< CompDef _ n _) e = return e

buildPropLens e xs =
  case foldl f (Right id) xs of
    Left y -> Left y
    Right _ -> Right $ foldl (.) id (map ix xs)
  where f (Right x) y = case e ^? x . ix y of
           Nothing -> Left y
           Just a  -> Right (x . ix y)

-- No monoid instance for AST so forced to use ^.. which returns a list
getDef syms scope def = head $ (S.lkup syms scope def) ^.. _Just

elab (ti, syms) = map f ti
    where
        env = ReaderEnv [""] syms defDefs
        st  = ElabState {_msgs = emptyMsgs, _addr = 0, _regwidth = 4, _nextbit = 0, _usedbits = Set.empty}
        f x = runState (runReaderT (instantiate (pos :< CompInst x x Nothing [])) env) st
            where pos = extract $ getDef syms [""] x ^. _2

ereturn = return . Just . Fix
assignProp k v m = M.insert k (Just v) m

a =  (elab ret)
f ((Just s,_):_) = s
out = f a


