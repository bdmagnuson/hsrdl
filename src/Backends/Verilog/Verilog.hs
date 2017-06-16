{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Backends.Verilog.Verilog
  ( createNodes
  , connectNodes
  , connectFields
  , wrField
  , isFieldNode
  , mkPortMap
  , verilog
  , St (..)
  ) where

import Backends.Verilog.AST
import Types (ElabF(..), CompType(..), PropRHS(..))

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.State
import Control.Lens
import Data.Graph.Inductive hiding ((&), nmap)
import Data.Functor.Foldable
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe)
import Debug.Trace
import qualified Data.Text.Prettyprint.Doc as P

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (nubBy)
import Data.Function (on)

import Text.Show.Deriving

$(makePrisms ''PropRHS)
$(makePrisms ''Fix)
$(makeLenses ''ElabF)
$(deriveShow1 ''ElabF)


data NodeT
  = NField FieldInfo
  | NReg RegInfo
  | NExt Text
  | NDecode
  | NSync
  | NModule deriving (Show)


data Con
 = GWire    Text Integer
 | Constant Integer deriving (Show, Eq)

data Port = Port
  { _src  :: Text
  , _dest :: Text
  , _con  :: Con
  } deriving (Show, Eq)

data St = St
  { _graph :: Gr NodeT Port
  , _nmap  :: M.Map Text Node
  , _path  :: [(CompType, Text)]
  } deriving (Show)

data RegInfo = RegInfo
  { _rname       :: Text
  , _reg         :: Fix ElabF
  } deriving (Show)

data FieldInfo = FieldInfo
  { _fname       :: Text
  , _frname       :: Text
  , _field       :: Fix ElabF
  } deriving (Show)

$(makeLenses ''FieldInfo)
$(makeLenses ''RegInfo)
$(makeLenses ''St)
$(makePrisms ''NodeT)
$(makeLenses ''Port)

isFieldNode :: LNode NodeT -> Bool
isFieldNode n = isJust $ n ^? _2 . _NField
isRegNode :: LNode NodeT -> Bool
isRegNode n = isJust $ n ^? _2 . _NReg

filterNode g p = filter (\x -> isJust $ x ^? _2 . p) (labNodes g)
filterFields g = filterNode g _NField
filterRegs g   = filterNode g _NReg
filterSyncs g  = filterNode g _NSync


traceE x = trace (show x) x

freeNodeID = do
  g <- use graph
  return $ traceE $ head (newNodes 1 g)

lkupNode = undefined

nIO       = 0
nDecode   = 1
nConstant = 2
nSync     = 3

prop  f p = f ^? _NField . field . _Fix . props . ix p . _Just
pLit  f p = ((fromJust $ prop f p) ^. _PropLit)
pEnum f p = ((fromJust $ prop f p) ^. _PropEnum)
pNum  f p = fromJust $ ((fromJust $ prop f p) ^? _PropNum)
wn f n = f ^. _NField . fname <> "_" <> n

createNodes :: Fix ElabF -> State St ()
createNodes all@(Fix (ElabF Field n p _ i l m)) = do
  g <- use graph
  p <- use path
  fID <- freeNodeID
  let name = pathName (p ++ [(Field, n)])
  let f = NField $ FieldInfo {_frname = pathName p, _fname = name , _field = all}
  nmap . at name ?= fID
  graph %= insNode (fID, f)
  sID <- freeNodeID
  graph %= insNode (sID, NSync)
  connect fID "next" sID "d" (GWire (wn f "next") (pNum f "fieldwidth"))
  connect sID "q" fID "prev" (GWire (wn f "prev") (pNum f "fieldwidth"))

createNodes   (Fix (ElabF Addrmap n _ _ i _ _)) = mapM_ createNodes i

createNodes r@(Fix (ElabF Reg     n _ _ i _ _)) = do
  g <- use graph
  p <- use path
  ff <- freeNodeID
  let name = p ++ [(Field, n)]
  path .= name
  nmap . at (pathName name) ?= ff
  graph %= insNode (ff, NReg $ RegInfo {_rname = pathName name, _reg = r})
  mapM_ createNodes i
  path %= init

createNodes   (Fix (ElabF t       n _ _ i _ _)) = do
  path %= (++ [(t, n)])
  mapM_ createNodes i
  path %= init

createInitialNodes = do
  graph %= insNode (nIO, NModule)
  graph %= insNode (nDecode, NDecode)
  graph %= insNode (nConstant, NModule)
  graph %= insNode (nSync, NModule)


pathName = pathName' ""
pathName' p [] = p
pathName' p ((_, n):[]) = p <> n
pathName' p ((t1, n1):xs@((t2, _):_)) = let delim = case (t1, t2) of
                                             (Array, _) -> ""
                                             (_, Field) -> "__"
                                             _          -> "_"
                                        in pathName' (p <> n1 <> delim) xs

lkupNodeByName :: Text -> State St (Maybe Node)
lkupNodeByName n = do
  m <- use nmap
  return $ M.lookup n m

connect :: Node -> Text -> Node -> Text -> Con -> State St ()
connect src src_port dst dst_port wire = do
  g <- use graph
  let fff = filter (\x -> x ^. _3 . dest == dst_port) (inn g dst)
  graph .= foldl (.) id (map delLEdge fff) g
  graph %= insEdge (src, dst, Port src_port dst_port wire)
  return ()

connectProp path prop dst dst_port wire = do
  src <- lkupNode path
  connect src prop dst dst_port wire

connectNodes :: State St ()
connectNodes = do
  g <- use graph
  mapM_ connectFields (filter isFieldNode (labNodes g))
  mapM_ connectRegs (filter isRegNode (labNodes g))


connectRegs :: (Node, NodeT) -> State St ()
connectRegs (this, ri) = do
  connect nDecode (wn "acc") this "acc" (GWire (wn "acc") 1)
  where
    wn n = ri ^. _NReg . rname <> "_" <> n

connectFields :: (Node, NodeT) -> State St ()
connectFields (this, fi) = do
  when (pEnum "sw" /= "na") $ do
    n <- lkupNodeByName (fi ^. _NField . frname)
    case n of
      Nothing -> error "bad rname"
      Just n -> connect n "" this "acc" (GWire (wn' "acc") 1)
  when ((pEnum "sw") `elem` ["r", "rw", "wr"]) (connect nIO "" this "rd" (GWire "rd" 1))

  when ((pEnum "sw") `elem` ["w", "rw", "wr"]) $ do
    (connect nIO "" this "wr" (GWire "wr" 1))
    (connect nIO "" this "sw_wdata" (GWire "wdata" 1))

  when ((pEnum "hw") `elem` ["w", "rw", "wr"]) $ do
    connect nIO "" this "hw_wdata" (GWire (wn' "hw_wdata") 32)
    case prop "we" of
      Nothing                   -> return ()
      Just (PropBool False)     -> return ()
      Just (PropBool True)      -> connect nIO (wn' "we") this "we" (GWire (wn' "we") 1)
      Just (PropRef  path prop) -> connectProp path prop this "we" (GWire (wn' "we") 1)

  attach "incrvalue"
  attach "decrvalue"
  attach "incrthreshold"
  attach "decrthreshold"
  return ()

  where
    wn'      = wn fi
    prop   p = fi ^? _NField . field . _Fix . props . ix p . _Just
    pLit   p = ((fromJust $ prop p) ^. _PropLit)
    pEnum  p = ((fromJust $ prop p) ^. _PropEnum)
    pNum   p = fromJust $ ((fromJust $ prop p) ^? _PropNum)

    attach p = case prop p of
                 Nothing -> return ()
                 Just (PropNum n) -> connect nConstant "" this p (Constant n)
                 Just (PropRef path prop) -> connectProp path prop this p (GWire p 1)

c2w c = case c of
          GWire t 1 -> t
          GWire t n -> t <> "[" <> (T.pack . show) (n - 1) <> ":0]"
          Constant i -> (T.pack . show) i

p2w k m = c2w (lkupPort k m)

lkupPort :: (Node, Text) -> M.Map (Node, Text) Con -> Con
lkupPort k m = case M.lookup k m of
                 Just p -> p
                 Nothing -> error ("non-existant key" ++ show k)

mkPortMap g = foldl f (M.empty, M.empty) (nodes g)
  where f (inp, outp) n = ( M.union inp  (M.fromList (map (\(Port _ d w) -> ((n, d), w)) (inn g n ^.. traverse . _3)))
                          , M.union outp (M.fromList (map (\(Port s _ w) -> ((n, s), w)) (out g n ^.. traverse . _3)))
                          )

wrField :: Gr NodeT Port -> (M.Map (Node, Text) Con, M.Map (Node, Text) Con) -> (Node, NodeT) -> ModuleItem
wrField g (inp, outp) (node, l) = AlwaysComb $ Block Nothing $ catMaybes [rclr, rset, swUpdate, hwUpdate] ++ fromMaybe [] counter --, hwclr, hwset, next, stickybit, intr]
  where
    ow  w = p2w (node, w) outp
    iw  w = p2w (node, w) inp
    iwLit w = Lit (iw w)
    owLit w = Lit (ow w)
    owLHS w = sLHS (ow w)
    owp w = M.member (node, w) outp
    iwp w = M.member (node, w) inp
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
               else Just $ If (iwLit "wr" :&&: iwLit "acc")
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


iDecl = ioDecl Input
oDecl = ioDecl Output

ioDecl :: IODirection -> LEdge Port -> IODecl
ioDecl d x | GWire n w <- x ^. _3 . con = IODecl d n (if w == 1 then Nothing else Just (w - 1, 0))

readMux g (inp, outp) = Case (SizedNum 1 Bin 1) (map f (lsuc g nDecode))
  where
    f (n, l) = (acc, fs)
     where acc = Lit $ c2w (l ^. con)
           fs = anonBlock $ map f' (filter (\(_, l) -> l ^. dest == "acc") (lsuc g n))
           f' (n, _) = NBAssign [("rdata", range n)] (Lit $ p2w (n, "prev") inp)
           range n = (\x -> Just (fromJust $ x ^? _NField . field . _Fix . msb, fromJust $ x ^? _NField . field . _Fix . lsb)) (fromJust $ lab g n)


decodeBlock g pm = AlwaysComb $ anonBlock $ (def ++ [decode, readMux g pm])
  where fs  = map (\x -> (c2w (x ^. _3 . con), fromJust $ lab g (x ^. _2))) (out g nDecode) -- (acc wire, associated reg)
        def = map (\(w, _) -> NBAssign (sLHS w) (Num 0)) fs
        decode = Case (Lit "addr") (map (\(w, f) -> (Num (fromJust $ prop f "address"), NBAssign (sLHS w) (Num 1))) fs)
        prop f p  = f ^? _NReg . reg . _Fix . props . ix p . _Just . _PropNum

sync g (inp, outp) = Always [Posedge "clk", Negedge "rst_l"] (anonBlock [(If (LogicalNot (Lit "rst_l")) rst [] (Just clk))])
  where s = filterSyncs g
        rst = anonBlock $ map (\(n, _) -> Assign (sLHS (p2w (n, "q") outp)) (Num 0)) s
        clk = anonBlock $ map (\(n, _) -> Assign (sLHS (p2w (n, "q") outp)) (Lit $ p2w (n, "d") inp)) s


verilog a = showModule $ Module (a ^. _Fix . name) ios (decode:combUpdate ++ [sync g pm])
  where st = execState (createInitialNodes >> createNodes a >> connectNodes)  (St empty M.empty [])
        g  = _graph st
        pm = mkPortMap g
        combUpdate = map (wrField g pm) (filterFields g)
        ios = map iDecl (deDupLEdges $ inn g nIO) ++ map oDecl (deDupLEdges $ out g nIO)
        decode = decodeBlock g pm

deDupLEdges = nubBy ((==) `on` (view _3))

zNext = NBAssign (sLHS "next") (Lit "'0")
oNext = NBAssign (sLHS "next") (Lit "'1")

anonBlock = Block Nothing
simpleIf c x = If c x [] Nothing

sLHS :: Text -> [(Text, Maybe a)]
sLHS x = [(x, Nothing)]
