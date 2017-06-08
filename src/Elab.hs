{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Elab (
     elab
   , getMsgs
   ) where

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Comonad.Cofree
import Control.Comonad
import Control.Lens hiding ((:<))
import Data.Monoid ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)

import SymbolTable as S
import Data.Functor.Foldable
import Data.Maybe (isJust, fromMaybe, fromJust)
import Props
import Parser
import Types
import qualified Data.Text as T
import Data.Text (Text)

import Text.Show.Deriving

$(makePrisms ''PropRHS)
$(makePrisms ''Fix)
$(makeLenses ''ElabF)
$(deriveShow1 ''ElabF)

type instance IxValue (Fix ElabF) = Fix ElabF
type instance Index (Fix ElabF) = Text

instance Ixed (Fix ElabF) where
    ix k f m = case break (\x -> unfix x ^. Elab.name == k) (unfix m ^. inst) of
                (_, []) -> pure m
                (i, l:ls) -> f l <&> \x -> Fix $ unfix m & inst .~ (i ++ (x:ls))

data Msgs = Msgs {
    _info  :: [Text],
    _warn  :: [Text],
    _err   :: [Text]
} deriving (Show)

$(makeLenses ''Msgs)

data ElabState = ElabState {
    _msgs     :: Msgs,
    _addr     :: Integer,
    _usedbits :: Set.Set Integer,
    _nextbit  :: Integer,
    _baseAddr :: Integer,
    _sprops   :: [M.Map CompType (M.Map Text Property)]
} deriving (Show)

$(makeLenses ''ElabState)

type ElabS = State ElabState

data ReaderEnv = ReaderEnv {
    _rext   :: Maybe Bool,
    _scope  :: [Text],
    _syms   :: S.SymTab (Expr SourcePos)
}

makeLenses ''ReaderEnv

logMsg t pos m = lift $ (msgs . t) %= (((T.pack . sourcePosPretty) pos <> " - " <> m):)
getMsgs x = reverse $ (x ^. msgs . info) ++ (x ^. msgs . warn) ++ (x ^. msgs . err)

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
            logMsg err pos ("Field overlap on bits " <> (T.pack . show . Set.toList) intersection)
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


pushDefs = lift (sprops %= \a@(x:xs) -> x:a)
popDefs  = lift (sprops %= \(x:xs) -> xs)
modifyDefs prop rhs = mapM_ (\x -> lift (sprops . ix 0 . ix x . ix prop . pdefault .= Just rhs)) [Signal, Field, Reg, Regfile, Addrmap]

instantiate :: Expr SourcePos -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
instantiate (pos :< CompInst iext d n arr align@(Alignment at' mod stride)) = do
    env <- ask
    sp <- lift (use sprops)
    case S.lkup (env ^. syms) (env ^. scope) d of
        Nothing -> do
            logMsg err pos ("Lookup failure: " <> d <> " in " <> mconcat (env ^. scope))
            return Nothing
        Just (sc, _ :< def) ->
          case (ctype def, arr) of
            (Field, _)      -> foo newinst >>= assignBits pos arr
            (_, Just (ArrWidth w)) -> do b <- elmAddr 1
                                         x <- myMapM (\x -> instantiate (pos :< CompInst isext d ((T.pack . show) x) Nothing (arrAlign b x))) [0..(w-1)]
                                         case x of
                                           Nothing -> return Nothing
                                           Just ff -> ereturn $ ElabF Array n M.empty (fromMaybe False isext) ff 0 0
                                         where arrAlign b x = Alignment ((\y -> b + x * y) <$> stride) Nothing Nothing
                                               myMapM f = runMaybeT . mapM (MaybeT . f)

            (Reg, Nothing) -> (foo newinst >>= assignAddress) <* resetBits
            otherwise -> setBaseAddress *> foo newinst
         where
           isext = msum [env ^. rext, iext, Types.ext def]
           foo x   = withReaderT newenv  $ pushDefs *> foldl (>>=) x (map elaborate (expr def)) <* popDefs
                     where newenv = (scope .~ (sc ++ [d])) . (rext .~ isext)
           newinst =
             ereturn ElabF
               { _etype = ctype def
               , _name  = n
               , _props = (head sp ^. ix (ctype def)) & traverse %~ (^. pdefault)
               , _inst  = []
               , _ext   = fromMaybe False isext
               , _lsb   = 0
               , _msb   = 0
               }
           elmAddr rw = do
             curAddr <- lift (use addr)
             baseAddr <- lift (use baseAddr)
             let b = case (at', mod, stride) of
                      (Just a, _, _) -> baseAddr + a
                      (Nothing, Just a, _) -> roundMod curAddr a
                      (Nothing, Nothing,  _) -> roundMod curAddr rw
             lift (addr .= b)
             return b
           assignAddress e = do
             let rw = fromJust (e ^? _Just . _Fix . props . ix "regwidth" . _Just . _PropNum) `div` 8
             a <- elmAddr rw
             lift (addr .= a + rw)
             return $ e & _Just . _Fix . props %~ assignProp "address" (PropNum a)
           setBaseAddress = do
             b <- lift (use addr)
             lift (baseAddr .= b)
             return ()

elaborate _ Nothing = return Nothing
elaborate ins@(pos :< CompInst _ cd cn _ _) (Just i) = do
    s <- lift get
    if isJust $ i ^? ix cn
        then do
          logMsg err pos (cn <>  " already defined")
          return Nothing
        else do
          new <- instantiate ins
          case new of
            Nothing -> return Nothing
            Just a -> (return . Just) (i & _Fix . inst %~ (++ [a]))


elaborate (pos :< PropAssign path prop rhs) (Just e) =
  case t of
    Left elm -> do
      logMsg err pos ("invalid path, failed at " <> elm)
      return Nothing
    Right l -> do
      legal <- checkAssign pos (e ^? runTraversal l . _Fix) prop rhs
      case legal of
        Just () -> foldl (>>=) ((return . Just) $ e & runTraversal l . _Fix . props %~ assignProp prop rhs) [cExclusive p | p <- fromMaybe [] (M.lookup prop exMap)]
        Nothing -> return Nothing
  where t = buildPropTraversal e (concatMap buildPath path)
        buildPath (PathElem s Nothing) = [s]
        buildPath (PathElem s (Just (ArrWidth w))) = [s, (T.pack . show) w]
        cExclusive p Nothing = return Nothing
        cExclusive p e = if isPropSet (fromJust $ e ^? _Just . _Fix . props . ix p)
                         then do logMsg warn pos ("Property " <> prop <> " is mutually exlusive with " <> p <> ".  Unsetting " <> p <> ".")
                                 return $ e & _Just . _Fix . props . ix p .~ Nothing
                         else return e


elaborate d@(_ :< CompDef _ _ n _) e = return e

elaborate (pos :< PropDefault prop rhs) (Just e) = do
  legal <- checkDefaultAssign pos (Just (e ^. _Fix)) prop rhs
  case legal of
    Nothing -> return Nothing
    Just () -> do
      modifyDefs prop rhs
      (return . Just) e

checkAssign pos (Just elm) prop rhs = cExist >>= cType pos prop rhs
  where
    cExist = do
      sp <- lift (use sprops)
      case sp ^? ix 0 . ix (elm ^. etype) . ix prop of
        Nothing -> do
          logMsg err pos ("Property " <> prop <> " not defined for component " <> (T.pack . show) (elm ^. etype))
          return Nothing
        Just p -> return (Just p)

checkDefaultAssign pos (Just elm) prop rhs = cExistAny >>= cType pos prop rhs
  where
    cExistAny = do
      sp <- lift (use sprops)
      case msum $ map (\x -> sp ^? ix 0 . ix x . ix prop) [Signal, Field, Reg, Regfile, Addrmap] of
        Nothing -> do
          logMsg err pos ("Property " <> prop <> " not defined for any component")
          return Nothing
        Just p -> return (Just p)

cType pos prop rhs (Just x) =
  case checktype (x ^. ptype) rhs of
    False -> do
      logMsg err pos ("Type mismatch: Property '" <> prop <> "' expecting " <> (T.pack . show) (x ^. ptype) <> " found " <> (T.pack . show) (typeOf rhs))
      return Nothing
    True -> return (Just ())
cType _ _ _ Nothing = return Nothing


buildPropTraversal e x = foldl (>>=) (Right $ Traversal id) (map f x)
  where f y x =
         case e ^? (runTraversal x . ix y) of
            Nothing -> Left y
            Just _  -> Right $ Traversal $ runTraversal x . ix y

getDef syms scope def =
  case S.lkup syms scope def of
    Just s -> s
    Nothing -> error $ T.unpack ("No instance" <> def)

elab (ti, syms) =
  map f ti
    where
        env = ReaderEnv {_scope = [""], _syms = syms, _rext = Nothing}
        st  = ElabState {_msgs = Msgs [] [] [], _addr = 0, _nextbit = 0, _usedbits = Set.empty, _baseAddr = 0, _sprops = [defDefs]}
        f x = runState (runReaderT (instantiate (pos :< CompInst Nothing x x Nothing (Alignment Nothing Nothing Nothing))) env) st
            where pos = extract $ getDef syms [""] x ^. _2

ereturn = return . Just . Fix
assignProp k v = M.insert k (Just v)


