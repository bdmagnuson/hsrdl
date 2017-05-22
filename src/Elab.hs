{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Elab (
     elab
   , getMsgs
   ) where


--import GHC.IO

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Comonad.Cofree
import Control.Comonad
import Control.Lens hiding ((:<))
import Debug.Trace

import qualified Data.Map.Strict as M
import qualified Data.Set as Set


import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

import SymbolTable as S
import Data.Functor.Foldable
import Data.Maybe (isJust, fromMaybe)
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
    ix k f m = case break (\x -> unfix x ^. Elab.name == k) (unfix m ^. inst) of
                (_, []) -> pure m
                (i, l:ls) -> f l <&> \x -> Fix $ unfix m & inst .~ (i ++ (x:ls))

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
    _nextbit  :: Integer,
    _baseAddr :: Integer,
    _sprops   :: [M.Map CompType (M.Map String Property)]
} deriving (Show)

$(makeLenses ''ElabState)

type ElabS = State ElabState
type Elab = Fix ElabF


data ReaderEnv = ReaderEnv {
    _rext   :: Maybe Bool,
    _scope  :: [String],
    _syms   :: S.SymTab (Expr SourcePos)
}

makeLenses ''ReaderEnv



logMsg t pos m = lift $ (msgs . t) %= ((sourcePosPretty pos ++ " - " ++ m):)
getMsgs x = (x ^. msgs . info) ++ (x ^. msgs . warn) ++ (x ^. msgs . err)


assignBits :: SourcePos -> Maybe Array -> Maybe (Fix ElabF) -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
assignBits pos _ Nothing = return Nothing
assignBits pos Nothing r = assignBits pos (Just (ArrWidth 1)) r
assignBits pos (Just arr) (Just (Fix reg)) = do
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
            logMsg err pos ("Field overlap on bits " ++ (show . Set.toList) intersection)
            return Nothing
    where
        range x y = if x < y then [x..y] else [y..x]

resetBits = do
    lift (nextbit .= 0)
    lift (usedbits .= Set.empty)
    return ()

roundMod x m =
  case divMod x m of
    (c, 0) -> x
    (c, _) -> (c + 1) * m


pushDefs = do
  lift (sprops %= \a@(x:xs) -> x:a)
  return ()

popDefs = do
  lift (sprops %= \(x:xs) -> xs)
  return ()

modifyDefs prop rhs = do
    mapM_ (\x -> lift (sprops . ix 0 . ix x . ix prop . pdefault .= (Just rhs))) [Signal, Field, Reg, Regfile, Addrmap]
    return ()

instantiate :: Expr SourcePos -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
instantiate (pos :< CompInst iext d n arr align@(Alignment at' mod stride)) = do
    env <- ask
    sp <- lift (use sprops)
    case S.lkup (env ^. syms) (env ^. scope) d of
        Nothing -> do
            logMsg err pos ("Lookup failure: " ++ show d ++ " in " ++ show (env ^. scope))
            return Nothing
        Just (sc, _ :< def) -> do
          case (ctype def, arr) of
            (Field, _)      -> foo newinst >>= assignBits pos arr
            (_, Just (ArrWidth w)) -> do b <- elmAddr
                                         x <- fmap (traverse id) $ mapM (\x -> instantiate (pos :< CompInst isext d (show x) Nothing (arrAlign b x))) [0..(w-1)]
                                         case x of
                                           Nothing -> return Nothing
                                           Just ff -> ereturn $ ElabF Array n M.empty (fromMaybe False isext) ff 0 0
                                         where arrAlign b x = Alignment ((\y -> b + x * y) <$> stride) Nothing Nothing
            (Reg, Nothing) -> foo (newinst >>= assignAddress) <* incrAddress <* resetBits
            otherwise -> setBaseAddress *> foo newinst <* incrAddress
         where
           isext = msum [env ^. rext, Types.ext def, iext]
           foo x   = do
                       withReaderT newenv  $ pushDefs *> foldl (>>=) x (map elaborate (expr def)) <* popDefs
                       where newenv = (scope .~ (sc ++ [d])) . (rext .~ isext)
           newinst =
             ereturn ElabF {
             _etype = ctype def,
             _name  = n,
             _props =  M.fromList ((traverse . _2) %~ (^. pdefault) $ M.toList ((head sp) ^. at (ctype def) . _Just)),
             _inst  = [],
             _ext   = fromMaybe False isext,
             _lsb   = 0,
             _msb   = 0}
           elmAddr = do
             curAddr <- lift (use addr)
             baseAddr <- lift (use baseAddr)
             let b = case (at', mod, stride) of
                      (Just a, _, _) -> baseAddr + a
                      (Nothing, Just a, _) -> (roundMod curAddr a)
                      (Nothing, Nothing,  _) -> curAddr
             lift (addr .= b)
             return b
           assignAddress e = do
             a <- elmAddr
             return $ e & _Just . _Fix . props %~ assignProp "address" (PropNum a)
           incrAddress = do
              gg <- lift (use regwidth)
              lift (addr += gg)
              return ()
           setBaseAddress = do
              b <- lift (use addr)
              lift (baseAddr .= b)
              return ()

elaborate _ Nothing = return Nothing
elaborate ins@(pos :< CompInst _ cd cn _ _) (Just i) = do
    s <- lift get
    if isJust $ i ^? ix cn
        then do
          logMsg err pos (cn ++  " already defined")
          return Nothing
        else do
          new <- instantiate ins
          case new of
            Nothing -> do
              logMsg err pos ("Failed to instantiate " ++ show cn)
              return Nothing
            Just a -> (return . Just) (i & _Fix . inst %~ (++ [a]))


-- For reasons that aren't clear I can't use t1 where t2 is being used
elaborate (pos :< PropAssign path prop rhs) (Just e) =
  case t1 of
    Left elm -> do
      logMsg err pos ("invalid path, failed at \"" ++ show elm ++ "\"")
      return Nothing
    Right l -> do
      legal <- checkAssign pos (e ^? (fromRight t2) . _Fix) prop rhs
      case legal of
        Just () -> (return . Just) $ e & l . _Fix . props %~ assignProp prop rhs
        Nothing -> return Nothing
  where t1 = buildPropLens e (map peName path)
        t2 = buildPropLens e (map peName path)

elaborate d@(_ :< CompDef _ _ n _) e = return e

elaborate (pos :< PropDefault prop rhs) (Just e) = do
  legal <- checkDefaultAssign pos (Just (e ^. _Fix)) prop rhs
  case legal of
    Nothing -> return Nothing
    Just () -> do
      modifyDefs prop rhs
      (return . Just) e

checkAssign pos (Just elm) prop rhs = cExist >>= (cType pos prop rhs) >>= cExclusive
  where
    cExist = do
      sp <- lift (use sprops)
      case sp ^? ix 0 . ix (elm ^. etype) . ix prop of
        Nothing -> do
          logMsg err pos ("Property " ++ prop ++ " not defined for component " ++ (show (elm ^. etype)))
          return Nothing
        Just p -> return (Just p)
    cExclusive (Just _) = return (Just ())
    cExclusive Nothing = return Nothing

checkDefaultAssign pos (Just elm) prop rhs = cExistAny >>= (cType pos prop rhs)
  where
    cExistAny = do
      sp <- lift (use sprops)
      case msum $ map (\x -> sp ^? ix 0 . ix x . ix prop) [Signal, Field, Reg, Regfile, Addrmap] of
        Nothing -> do
          logMsg err pos ("Property " ++ prop ++ " not defined for any component")
          return Nothing
        Just p -> do
          return (Just p)

cType pos prop rhs (Just x) = do
  case checktype (x ^. ptype) rhs of
    False -> do
      logMsg err pos ("Type mismatch: Property '" ++ prop ++ "' expecting " ++ show (x ^. ptype) ++ " found " ++ show (typeOf rhs))
      return Nothing
    True -> return (Just ())
cType _ _ _ Nothing = return Nothing



buildPropLens e xs =
  case foldl f (Right id) xs of
    Left y -> Left y
    Right _ -> Right $ foldl (.) id (map ix xs)
  where f (Right x) y = case e ^? x . ix y of
           Nothing -> Left y
           Just a  -> Right (x . ix y)

-- No monoid instance for AST so forced to use ^.. which returns a list
getDef syms scope def = head $ S.lkup syms scope def ^.. _Just

elab (ti, syms) = do
  map f ti
    where
        env = ReaderEnv {_scope = [""], _syms = syms, _rext = Nothing}
        st  = ElabState {_msgs = emptyMsgs, _addr = 0, _regwidth = 4, _nextbit = 0, _usedbits = Set.empty, _baseAddr = 0, _sprops = [defDefs]}
        emptyMsgs = Msgs [] [] []
        f x = runState (runReaderT (instantiate (pos :< CompInst Nothing x x Nothing (Alignment Nothing Nothing Nothing))) env) st
            where pos = extract $ getDef syms [""] x ^. _2

ereturn = return . Just . Fix
assignProp k v = M.insert k (Just v)

--a =  elab ret
--f ((Just s,_):_) = s
--out = f a
--
fromRight (Right a)  = a
fromRight (Left _) = error "fromRight: is Left"


