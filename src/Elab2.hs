{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Elab2 (
     elab
   , getMsgs
   , getInstCache
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
import qualified Data.Set as S
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Debug.Trace

import SymbolTable as ST
import Data.Functor.Foldable
import Data.Maybe (isJust, fromMaybe, fromJust)
import Props
import Parser
import Types
import qualified Data.Text as T
import Data.Text (Text)

type instance IxValue (Fix ElabF) = Fix ElabF
type instance Index (Fix ElabF) = Text

instance Ixed (Fix ElabF) where
    ix k f m = case break (\x -> unfix x ^. ename == k) (unfix m ^. einst) of
                (_, []) -> pure m
                (i, l:ls) -> f l <&> \x -> Fix $ unfix m & einst .~ (i ++ (x:ls))

data Msgs = Msgs {
    _info  :: [Text],
    _warn  :: [Text],
    _err   :: [Text]
} deriving (Show)

$(makeLenses ''Msgs)

data ElabState = ElabState {
    _msgs       :: Msgs,
    _addr       :: Integer,
    _usedbits   :: S.Set Integer,
    _baseAddr   :: Integer,
    _nextbit    :: Integer,
    _instCache  :: ST.SymTab (Fix ElabF),
    _sprops     :: [M.Map CompType (M.Map Text Property)]
} deriving (Show)


$(makeLenses ''ElabState)

type ElabS = State ElabState

data ReaderEnv = ReaderEnv {
    _rext   :: Maybe Bool,
    _scope  :: [Text],
    _syms   :: ST.SymTab (Expr SourcePos)
}

makeLenses ''ReaderEnv

getInstCache st = st ^. instCache
logMsg t pos m = lift $ (msgs . t) %= (((T.pack . sourcePosPretty) pos <> " - " <> m):)
getMsgs x = reverse $ (x ^. msgs . info) ++ (x ^. msgs . warn) ++ (x ^. msgs . err)

setProp n p e = e & eprops %~ assignProp n p

assignBits :: SourcePos -> Maybe Array -> Maybe (Fix ElabF) -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
assignBits pos _ Nothing = return Nothing
assignBits pos Nothing r = assignBits pos (Just (ArrWidth 1)) r
assignBits pos (Just arr) (Just (Fix reg)) = do
    used <- lift (use usedbits)
    nb   <- lift (use nextbit)
    let (l, r, set) = case arr of
          ArrLR l r  -> (l, r, S.fromList $ range l r)
          ArrWidth w -> (nb + w - 1, nb, S.fromList $ range nb (nb+w-1))
    let intersection = S.intersection used set
    let union        = S.union used set
    if null intersection
        then do
            lift (nextbit .= l + 1)
            lift (usedbits .= union)
            (return . Just . Fix) $ (setProp "lsb" (PropNum r)) .
                                    (setProp "msb" (PropNum l)) .
                                    (setProp "fieldwidth" (PropNum (l - r + 1))) $ reg
        else do
            logMsg err pos ("Field overlap on bits " <> (T.pack . show . S.toList) intersection)
            return Nothing
    where
        range x y = if x < y then [x..y] else [y..x]

resetBits :: ReaderT ReaderEnv ElabS ()
resetBits = do
    lift (nextbit .= 0)
    lift (usedbits .= S.empty)
    return ()

roundMod x m =
  case divMod x m of
    (c, 0) -> x
    (c, _) -> (c + 1) * m


pushDefs :: ReaderT ReaderEnv ElabS ()
pushDefs = lift (sprops %= \a@(x:xs) -> x:a)

popDefs :: ReaderT ReaderEnv ElabS ()
popDefs  = lift (sprops %= \(x:xs) -> xs)
modifyDefs prop rhs = mapM_ (\x -> lift (sprops . ix 0 . ix x . ix prop . pdefault .= Just rhs)) [Signal, Field, Reg, Regfile, Addrmap]

-- maybeMapM f = runMaybeT . mapM (MaybeT . f)
--


getSize x =
 case x ^. _Fix . etype of
   Field -> 0
   Reg   -> (getNumProp "regwidth" x) `div` 8
   otherwise -> case maximumByOf traverse (\x y -> compare (x ^. _Fix . eoffset) (y ^. _Fix . eoffset)) (x ^. _Fix . einst) of
                  Nothing -> 0
                  Just e -> e ^. _Fix . eoffset + (getSize e)

getAlign (Alignment x y z) = (x, y, z)

calcOffsets :: Maybe (Fix ElabF) -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
calcOffsets Nothing = return Nothing
calcOffsets (Just x) = do
   lift (baseAddr .= 0)
   newInst <- mapM setOffset (x ^. _Fix . einst)
   return $ Just $ x & _Fix . einst .~ newInst

setOffset :: Fix ElabF -> ReaderT ReaderEnv ElabS (Fix ElabF)
setOffset  x = do
  ba <- lift (use baseAddr)
  let b = case (at', mod) of
           (Just a, _) -> a
           (Nothing, Just a) -> roundMod ba a
           (Nothing, Nothing) -> ba

  let x'  = x & _Fix . eoffset .~ b
  let x'' = if (x ^. _Fix . etype == Array)
            then x' & _Fix . estride .~ arrStride
                    & _Fix . einst %~ imap (\i x -> x & _Fix . eoffset .~ b + arrStride * (fromIntegral i))
            else x'

  when   (x ^. _Fix . etype == Array) $ lift (baseAddr .= b + arrStride * (fromIntegral . length $ x ^.  _Fix . einst))
  unless (x ^. _Fix . etype == Array) $ lift (baseAddr .= b + getSize x)
  return x''
  where arrStride = case stride of
                       Nothing -> getSize (fromJust (x ^? _Fix . einst . ix 0))
                       Just a -> a
        (at', mod, stride) = getAlign (x ^. _Fix . ealign)

instantiate :: Expr SourcePos -> ReaderT ReaderEnv ElabS (Maybe (Fix ElabF))
instantiate (pos :< CompInst iext d n arr align) = do
  env   <- ask
  cache <- lift (use instCache)
  case ST.lkup cache (env ^. scope) d of
    Nothing -> case ST.lkup (env ^. syms) (env ^. scope) d of
                 Nothing -> do
                             logMsg err pos ("Unknown definition: " <> d <> " in " <> mconcat (env ^. scope))
                             return Nothing
                 Just  def -> elabInst def >>= calcOffsets >>= addCache (env ^. scope) >>= arrInst
    Just (_, a) -> return (Just a) >>= arrInst >>= calcOffsets

  where
   addCache s (Just i) = do
      lift $ instCache %= ST.add s n i
      return (Just i)

   arrInst (Just x) =
     case (x ^. _Fix . etype, arr) of
       (Field, _) -> assignBits pos arr (Just x)
       (_, Just (ArrWidth w)) ->
          let newinst = x & _Fix . ealign .~ (Alignment Nothing Nothing Nothing)
          in return $ Just $ x & _Fix . etype  .~ Array
                               & _Fix . einst  .~ zipWith (\x y -> y & _Fix . ename .~ (T.pack . show) x) [0..(w-1)] (repeat newinst)
       otherwise -> return (Just x)
   arrInst Nothing = return Nothing

   elabInst (sc, _ :< def) = do
     pushDefs
     when (ctype def == Reg) resetBits
     inst <- withReaderT newenv $ foldl (>>=) initInst (map elaborate (expr def))
     popDefs
     return inst
     where newenv = (scope .~ (sc ++ [d]))-- . (rext .~ isext)
           initInst = do
             sp <- lift (use sprops)
             (return . Just . Fix) $ ElabF
               { _etype      = ctype def
               , _ename      = n
               , _eprops     = (head sp ^. ix (ctype def)) & traverse %~ (^. pdefault)
               , _epostProps = []
               , _einst      = []
               , _ealign     = align
               , _eoffset    = 0
               , _escope     = sc ++ [d]
               , _estride    = 0
               }


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
            Just a -> (return . Just) (i & _Fix . einst %~ (++ [a]))


elaborate (pos :< PropAssign [] prop rhs) (Just e) = do
   legal <- checkAssign pos (e ^? _Fix) prop rhs
   case legal of
     Just () -> foldl (>>=) ((return . Just) $ e &  _Fix . eprops %~ assignProp prop rhs) [cExclusive p | p <- fromMaybe [] (M.lookup prop exMap)]
     Nothing -> return Nothing
   where
     cExclusive p Nothing = return Nothing
     cExclusive p e = if isPropSet (fromJust $ e ^? _Just . _Fix . eprops . ix p)
                      then do logMsg warn pos ("Property " <> prop <> " is mutually exlusive with " <> p <> ".  Unsetting " <> p <> ".")
                              return $ e & _Just . _Fix . eprops . ix p .~ Nothing
                      else return e

elaborate (pos :< PropAssign path prop rhs) (Just e) =
  case t of
    Left elm -> do
      logMsg err pos ("invalid path, failed at " <> elm)
      return Nothing
    Right l -> do
      legal <- checkAssign pos (e ^? runTraversal l . _Fix) prop rhs
      case legal of
        Just () -> (return . Just) $ e & _Fix . epostProps %~ (++ [(path, prop, rhs)])
        Nothing -> return Nothing
      where
  where t = buildPropTraversal e (concatMap buildPath path)
        buildPath (PathElem s Nothing) = [s]
        buildPath (PathElem s (Just (ArrWidth w))) = [s, (T.pack . show) w]


elaborate d@(_ :< CompDef _ _ n _) e = return e

elaborate (pos :< PropDefault prop rhs) (Just e) = do
  legal <- checkDefaultAssign pos (Just (e ^. _Fix)) prop rhs
  case legal of
    Nothing -> return Nothing
    Just () -> do
      modifyDefs prop rhs
      (return . Just) e
  where
    checkDefaultAssign pos (Just elm) prop rhs = cExistAny >>= cType pos prop rhs
    cExistAny = do
      sp <- lift (use sprops)
      case msum $ map (\x -> sp ^? ix 0 . ix x . ix prop) [Signal, Field, Reg, Regfile, Addrmap] of
        Nothing -> do
          logMsg err pos ("Property " <> prop <> " not defined for any component")
          return Nothing
        Just p -> return (Just p)

checkAssign pos (Just elm) prop rhs = cExist >>= cType pos prop rhs
  where
    cExist = do
      sp <- lift (use sprops)
      case sp ^? ix 0 . ix (elm ^. etype) . ix prop of
        Nothing -> do
          logMsg err pos ("Property " <> prop <> " not defined for component " <> (T.pack . show) (elm ^. etype))
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
  case ST.lkup syms scope def of
    Just s -> s
    Nothing -> error $ T.unpack ("No instance" <> def)

elab (ti, syms) =
  map f ti
    where
        env = ReaderEnv {_scope = [""], _syms = syms, _rext = Nothing}
        st  = ElabState {_msgs = Msgs [] [] [], _addr = 0, _nextbit = 0, _usedbits = S.empty, _baseAddr = 0, _sprops = [defDefs], _instCache = M.empty}
        f x = runState (runReaderT (instantiate (pos :< CompInst Nothing x x Nothing (Alignment Nothing Nothing Nothing))) env) st

            where pos = extract $ getDef syms [""] x ^. _2



