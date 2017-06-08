{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Backends.Verilog.Verilog
  ( createNodes
  , doit
  , wrField
  , St (..)
  ) where

import Backends.Verilog.AST
import Types (ElabF(..), CompType(..), PropRHS(..))

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.State
import Control.Lens
import Data.Graph.Inductive hiding ((&))
import Data.Functor.Foldable
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Maybe (catMaybes, fromJust)

import qualified Data.Text as T
import Data.Text (Text)

import Text.Show.Deriving
--data Bits = Bits
--  { _ios        :: [IODecl]
--  , _wires      :: [Expr]
--  , _accRst     :: [Expr]
--  , _accCase    :: [Expr]
--  , _extCase    :: [Expr]
--  , _update     :: [Expr]
--  , _syncReset  :: [Expr]
--  , _syncUpdate :: [Expr]
--  }
--
-- $(makeLenses ''Bits)
--
$(makePrisms ''PropRHS)
$(makePrisms ''Fix)
$(makeLenses ''ElabF)
$(deriveShow1 ''ElabF)


data NodeT
  = NField FieldInfo
  | NExt Text
  | NDecode
  | NModule deriving (Show)

data Con
 = GWire    Text Integer
 | Constant Integer deriving (Show, Eq)

data Port = Port
  { _src  :: Text
  , _dest :: Text
  , _con  :: Con
  } deriving (Show, Eq)

top = NModule

data St = St
  { _graph :: Gr NodeT Port
  , _nmap  :: M.Map Text Node
  , _path  :: [(CompType, Text)]
  } deriving (Show)

data FieldInfo = FieldInfo
  { _fname       :: Text
  , _rname       :: Text
  , _field       :: Fix ElabF
  } deriving (Show)

$(makeLenses ''FieldInfo)
$(makeLenses ''St)
$(makePrisms ''NodeT)
$(makeLenses ''Port)


freeNodeID = do
  g <- use graph
  return $ head (newNodes 1 g)

lkupNode = undefined

mIO       = 0
mDecode   = 1
mConstant = 2


createNodes :: Fix ElabF -> State St ()
createNodes all@(Fix (ElabF Field n p _ i l m)) = do
  g <- use graph
  p <- use path
  ff <- freeNodeID
  graph %= insNode (ff, NField $ FieldInfo {_rname = pathName p, _fname = pathName (p ++ [(Field, n)]), _field = all})

createNodes (Fix (ElabF t n _ _ i _ _)) = (path %= (++ [(t, n)])) >> mapM_ createNodes i >> (path %= init)

pathName = pathName' ""
pathName' p [] = p
pathName' p ((t, n):xs) = let delim = \case
                                      Field -> "__"
                                      Array -> ""
                                      _     -> "_"
                          in pathName' (p <> delim t <> n) xs

ggg :: Text -> LEdge Port -> Bool
ggg dst_port edge = edge ^. _3 . dest == dst_port

connect :: Node -> Text -> Node -> Text -> Con -> State St ()
connect src src_port dst dst_port wire = do
  g <- use graph
  let fff = filter (ggg dst_port) (inn g dst)
  graph .= foldl (.) id (map delLEdge fff) g
  graph %= insEdge (src, dst, Port src_port dst_port wire)
  return ()

connectProp path prop dst dst_port wire = do
  src <- lkupNode path
  connect src prop dst dst_port wire

doit :: (Node, NodeT) -> State St ()
doit (this, fi) = do
  when (pLit "sw" /= "na") (connect mIO "" this "acc" (GWire "acc" 1))
  when ((pLit "sw") `elem` ["r", "rw", "wr"]) (connect mIO "" this "rd" (GWire "rd" 1))
  when ((pLit "sw") `elem` ["w", "rw", "wr"]) (connect mIO "" this "wr" (GWire "wr" 1))

  when ((pLit "hw") `elem` ["w", "rw", "wr"]) $ do
    connect mIO "" this "hw_data" (GWire (wn "hw_wdata") 32)
    case prop "we" of
      Nothing                   -> return ()
      Just (PropBool False)     -> return ()
      Just (PropBool True)      -> connect mIO "we" this "we" (GWire (wn "we") 1)
      Just (PropRef  path prop) -> connectProp path prop this "we" (GWire (wn "we") 1)

  attach "incrvalue"
  attach "decrvalue"
  attach "incrthreshold"
  attach "decrthreshold"
  return ()

  where
    prop p  = fi ^? _NField . field . _Fix . props . ix p . _Just
    pLit  p = ((fromJust $ prop p) ^. _PropLit)

    wn n = fi ^. _NField . fname <> "_" <> n
    attach p = case prop p of
                 Nothing -> return ()
                 Just (PropNum n) -> connect mConstant "" this p (Constant n)
                 Just (PropRef path prop) -> connectProp path prop this p (GWire p 1)

wrField :: Gr NodeT Port -> (Node, NodeT) -> [Stmt]
wrField g (node, l) = catMaybes [rclr, rset, swUpdate, hwUpdate] ++ fromJust counter --, hwclr, hwset, next, stickybit, intr]
  where
    inProps :: M.Map Text Con
    inProps  = M.fromList (map (\(Port _ d w) -> (d, w)) (inn g node ^.. traverse . _3))
    outProps = M.fromList (map (\(Port s _ w) -> (s, w)) (inn g node ^.. traverse . _3))
    ow  w = p2w (outProps ^? ix w)
    iw  w = p2w (inProps ^? ix w)
    p2w f = case f of
              Just (GWire t 1) -> t
              Just (GWire t n) -> t <> "[" <> (T.pack . show) (n - 1) <> ":0]"
              Just (Constant i) -> (T.pack . show) i

    iwLit w = Lit (iw w)
    owLit w = Lit (ow w)
    owLHS w = sLHS (ow w)
    owp w = M.member w outProps
    iwp w = M.member w inProps
    propBool p = fromJust (l ^? _NField . field . _Fix . props . ix p . _Just . _PropBool)
    rclr = case (propBool "rclr", owp "intr") of
                 (False, _) -> Nothing
                 (True, False) -> Just $ simpleIf (iwLit "rd" :&: iwLit "acc") (anonBlock [zNext])
                 (True, True)  -> Just $ simpleIf (iwLit "rd" :&: iwLit "acc") (anonBlock [ zNext
                                                                                          , NBAssign (sLHS "sticky_mask") (Num 0)
                                                                                          , NBAssign (sLHS "intr")        (Num 0)])
    rset  = if propBool "rset" then Just $ simpleIf (iwLit "rd" :&: iwLit "acc") oNext else Nothing
    swUpdate = if not (iwp "wr")
               then Nothing
               else Just $ If (iwLit "wr")
                      (case (propBool "woset", propBool "woclr") of
                        (True,      _) -> NBAssign (sLHS "next") (Lit "next" :|: Lit "sw_wdata")
                        (False,  True) -> anonBlock $ catMaybes [Just $ NBAssign (owLHS "next") ((owLit "next") :|: BitwiseNot (iwLit "sw_wdata"))
                                                            , if owp "intr" then Just $ NBAssign (owLHS "intr") (owLit "intr" :&: owLit "next" :!=: Num 0) else Nothing
                                                            , if owp "intr" && propBool "stickybit" then Just $ NBAssign (owLHS "sticky_mask") (owLit "sticky_mask" :&: BitwiseNot (owLit "sticky_mask")) else Nothing
                                                            ]
                        (False, False) -> anonBlock $ catMaybes [Just $ NBAssign (owLHS "next") (iwLit "sw_wdata")
                                                            , if owp "intr" then (Just $ NBAssign (owLHS "intr") (Num 0)) else Nothing
                                                            , if owp "intr" && propBool "stickybit" then (Just $ NBAssign (owLHS "sticky_mask") (Num 0)) else Nothing
                                                            ])
                      []
                      (if propBool "singlepulse" then Just zNext else Nothing)
    hwUpdate = if not (iwp "hw_wdata")
                 then Nothing
                 else let w = if (propBool "sticky" || propBool "stickybit")
                              then Paren (owLit "next" :&: iwLit "sticky_mask") :|: Paren (iwLit "hw_wdata" :&: BitwiseNot (iwLit "sticky_mask"))
                              else iwLit "hw_wdata"
                      in Just $ case (iwp "we", iwp "wel") of
                                 (True, _) -> simpleIf (iwLit "we") (NBAssign (owLHS "next") w)
                                 (False, True) -> simpleIf (iwLit "wel") (NBAssign (owLHS "next") w)
                                 (False, False) -> NBAssign (owLHS "next") w

    counter = if not (propBool "counter")
              then Nothing
              else Just $ catMaybes [ if owp "underflow" then Just $ (NBAssign (owLHS "underflow") (Num 0)) else Nothing
                             , if owp "overflow" then Just $ (NBAssign (owLHS "overflow") (Num 0)) else Nothing
                             , Just $ case (iwp "incr", iwp "decr") of
                                 (True, False) -> simpleIf (iwLit "incr") incrBlock
                                 (False, True) -> simpleIf (iwLit "decr") decrBlock
                                 (True, True) -> Case (Concat [iwLit "incr", iwLit "decr"])  [ (SizedNum 2 Bin 0, incrBlock)
                                                                                             , (SizedNum 2 Bin 1, decrBlock)
                                                                                             , (SizedNum 2 Bin 2, incrdecrBlock)
                                                                                             ]
                             ]
    incrBlock = anonBlock (catMaybes [-- Just $ Logic "wrap" Nothing
                                   Just $ NBAssign ([("wrap", Nothing), (ow "next", Nothing)]) (owLit "next" :+: iwLit "incrvalue")
                                 , if iwp "incrsaturate" then (Just $ simpleIf (Lit "wrap" :||: owLit "next" :>: iwLit "incrsaturate") (NBAssign (owLHS "next") (iwLit "incrsaturate"))) else Nothing
                                 , if owp "overflow"     then (Just $ NBAssign (owLHS "overflow") (Lit "next")) else Nothing
                                 ])
    decrBlock = anonBlock (catMaybes [ --Just $ Logic "wrap" Nothing
                                   Just $ NBAssign ([("wrap", Nothing), (ow "next", Nothing)]) (owLit "next" :-: iwLit "decrvalue")
                                 , if iwp "decrsaturate" then (Just $ simpleIf (Lit "wrap" :||: owLit "next" :<: iwLit "decrsaturate") (NBAssign (owLHS "next") (iwLit "decrsaturate"))) else Nothing
                                 , if owp "underflow"    then (Just $ NBAssign (owLHS "underflow") (Lit "next")) else Nothing
                                 ])
    incrdecrBlock = let ufc = (Lit "wrap" :&&: iwLit "decrvalue" :>: iwLit "incrvalue")
                        ofc = (Lit "wrap" :&&: iwLit "decrvalue" :<: iwLit "incrvalue")
                    in anonBlock (catMaybes [ --Just $ Logic "wrap" Nothing
                                          Just $ NBAssign [("wrap", Nothing), (ow "next", Nothing)] (owLit "next" :+: iwLit "incrvalue" :-: iwLit "decvalue")
                                        , if owp "underflow"    then (Just $ NBAssign (owLHS "underflow") ufc) else Nothing
                                        , if owp "overrflow"    then (Just $ NBAssign (owLHS "overflow")  ofc) else Nothing
                                        , if iwp "decrsaturate" then (Just $ simpleIf (ufc :|: owLit "next" :<: iwLit "decrsaturate") (NBAssign (owLHS "next") (iwLit "decrsaturate"))) else Nothing
                                        , if iwp "incrsaturate" then (Just $ simpleIf (ofc :|: owLit "next" :>: iwLit "incrsaturate") (NBAssign (owLHS "next") (iwLit "incrsaturate"))) else Nothing
                                        ])




zNext = NBAssign (sLHS "next") (Lit "'0")
oNext = NBAssign (sLHS "next") (Lit "'1")

anonBlock = Block Nothing
simpleIf c x = If c x [] Nothing
clear x = NBAssign (sLHS x) (Num 0)

sLHS :: Text -> [(Text, Maybe a)]
sLHS x = [(x, Nothing)]
