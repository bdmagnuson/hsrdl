{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.SRDL.Backends.Verilog
  ( verilog
  ) where

import Control.Lens
import Control.Monad.State
import Data.Functor.Foldable
import Data.Text.Lazy (toStrict)
import Data.Text.Prettyprint.Doc ((<>), (<+>), pretty)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Map as ML
import qualified Data.Set as S
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Data.Text as T
import Text.Blaze.Renderer.Text
import Text.Heterocephalus
import Text.Show.Deriving
import Debug.Trace
import Data.Maybe (catMaybes, fromMaybe, isJust, fromJust)

import Language.SRDL.Elab2
import Language.SRDL.Props
import Language.SRDL.Types

data ExtInfo = ExtInfo {
  _xisext  :: Implementation,
  _xshared :: Bool,
  _xbase   :: Integer,
  _xlast   :: Integer,
  _xname   :: Text
}

makeLenses ''ExtInfo

data RegsFilter = FilterInternal | FilterExternal | FilterNone


getProp t e p =
  case e ^? _Fix . eprops . ix p . _Just . t of
    Just b -> b
    _ -> trace (show (e ^? _Fix . eprops . ix "fqname", show a, p)) (error "you dun f'd up")
   where
     a = e ^? _Fix . eprops . ix p . _Just

getBool = getProp _PropBool
getNum  = getProp _PropNum
getEnum = getProp _PropEnum
getIntr = getProp _PropIntr
getRef  = getProp _PropRef
getLit  = getProp _PropLit

getAsLit e p =
   case e ^? _Fix . eprops . ix p . _Just of
      Nothing -> ""
      Just (PropBool True) -> "1"
      Just (PropBool False) -> "0"
      Just (PropNum a) -> T.pack (show a)
      Just (PropLit a) -> a
      _ -> error "got a problem"

--getProp e p = e ^. _Fix . eprops . at p
--
--getBool e p = case getProp e p of
--                Just (Just (PropBool b)) -> b
--getNum  e p = case getProp e p of
--                Just (Just (PropNum b)) -> b
--getEnum e p = case getProp e p of
--                Just (Just (PropEnum b)) -> b
--getIntr e p = case getProp e p of
--                Just (Just (PropIntr a b)) -> (a, b)
--getLit e p = case getProp e p of
--                Just (Just (PropLit b)) -> b
--getRef e p = case getProp e p of
--                Just (Just (PropRef b)) -> b


safeGetProp t e p = e ^? _Fix . eprops . ix p . _Just . t

safeGetBool = safeGetProp _PropBool
safeGetNum  = safeGetProp _PropNum
safeGetEnum = safeGetProp _PropEnum
safeGetIntr = safeGetProp _PropIntr
safeGetRef  = safeGetProp _PropRef
safeGetLit  = safeGetProp _PropLit

data ExtTree a = Leaf ExtInfo | Node ExtInfo [a] deriving (Functor)

filterExt :: Fix ElabF -> [ExtInfo]
filterExt x = filter (\x -> x ^. xisext == External) (hylo foldExt unfoldExt x)
  where
    unfoldExt e = if e ^. _Fix . etype == Reg
                  then Leaf info
                  else Node info (e ^. _Fix . einst)
      where
        info = ExtInfo
               { _xisext  = e ^. _Fix . eext
               , _xshared = getBool e "sharedextbus"
               , _xbase   = e ^. _Fix . eoffset
               , _xlast   = e ^. _Fix . eoffset + getSize e
               , _xname   = fn e
               }

    foldExt (Leaf e) = [e]
    foldExt (Node e i) =
       case (e ^. xisext, e ^. xshared) of
         (External,    _) -> [e]
         (       _, True) -> if length i == 0
                             then []
                             else [e { _xbase = fromJust $ minimumOf (traverse . xbase) (concat i)
                                     , _xlast = fromJust $ maximumOf (traverse . xlast) (concat i)}]
         (      _, False) -> concat i



fqName :: Fix ElabF -> State [(CompType, Text)] (Fix ElabF)
fqName e = do
  modify (\x -> x ++ [(e ^. _Fix . etype, e ^. _Fix . ename)])
  p <- get
  r <- mapM fqName (e ^. _Fix . einst)
  let !u = e & _Fix . eprops %~ assignProp "fqname" (PropLit (pathName p))
            & _Fix . einst  .~ r
  modify init
  return u
  where
    pathName = pathName' ""
    pathName' p [] = p
    pathName' p [(_, n)] = p <> n
    pathName' p ((t1, n1):xs@((t2, _):_)) = let delim = case (t1, t2) of
                                                 (Array, _) -> ""
                                                 (_, Field) -> "__"
                                                 _          -> "_"
                                            in pathName' (p <> n1 <> delim) xs

clog2 :: Integer -> Integer
clog2 x = ceiling (logBase 2 (fromIntegral x))

wires :: [VIO] -> Text
wires ios = pretty2text $ showIOs ios
  where
    showIOs x = P.vcat $ map (pretty . showIO) (filter p x)
    showIO (VIO InternalReg n w) = "reg " <> array' w <> n <> ";"
    p (VIO InternalReg _ _) = True
    p _ = False

header :: Text -> [VIO] -> Text
header n ios = pretty2text $ pt "`default_nettype none" <> P.line <> pt "module" <+> pretty n <+> P.parens (uHang (showIOs ios <> P.line)) <> P.semi
  where
    showIOs x = P.vcat $  P.punctuate P.comma (map (pretty . showIO) (filter p x))
    showIO (VIO Output n w) = "output reg " <> array' w <> n
    showIO (VIO Input n w) = "input wire " <> array' w <> n
    p (VIO InternalReg _ _) = False
    p _ = True

addExt :: [ExtInfo] -> [VIO] -> [VIO]
addExt = flip (foldl go)
  where go io e = [ VIO Output ((e ^. xname) <> "_rd") 1
                  , VIO Output ((e ^. xname) <> "_wr") 1
                  , VIO Output ((e ^. xname) <> "_addr") (clog2 (e ^. xlast - e ^. xbase + 1))
                  , VIO Input  ((e ^. xname) <> "_wdata") 32
                  , VIO Input  ((e ^. xname) <> "_rdata") 32
                  , VIO Input  ((e ^. xname) <> "_ack") 1
                  ] ++ io

array' :: Integer -> Text
array' 1 = toStrict ""
array' w = toStrict $ renderMarkup [compileText|[#{w - 1}:0] |]
arrayF f = array' (getNum f "fieldwidth")

footer ::  Text
footer = toStrict $ renderMarkup [compileText|
endmodule
`default_nettype wire
|]

fn f = getLit f "fqname"
fns f s = fn f <> "_" <> s

readMux :: [Fix ElabF] -> [ExtInfo] -> Text
readMux rs es  = toStrict $ renderMarkup [compileText|
always @(*) begin : _readMux
%{forall r <- rs}
  reg #{fn r}_acc;
%{endforall}
%{forall e <- es}
  reg #{e ^. xname}_acc;
%{endforall}

  _v_ack = 1'b0;
  _v_err = 1'b0;
  _v_rdata = '0;
%{forall r <- rs}
  #{fn r}_acc = 1'b0;
%{endforall}
%{forall e <- es}
  #{e ^. xname}_acc = 1'b0;
%{endforall}
  if (rd || wr) begin
    _v_ack = 1'b1;
    case (addr)
%{forall rr <- rs}
      #{view (_Fix . eoffset) rr} : #{fn rr}_acc = 1'b1;
%{endforall}
      default : _v_err = 1'b1;
    endcase

%{forall e <- es}
    if ((addr >= #{e ^. xbase}) && (addr <= #{e ^. xlast})) begin
       #{e ^. xname}_acc = 1'b1;
       _v_err = 1'b0;
       _v_ack = 1'b0;
    end
%{endforall}

    case (1'b1)
%{forall r <- rs}
       #{fn r}_acc : begin
%{forall f <- getElem isField Internal r}
          _v_rdata[#{getNumProp "msb" f}:#{getNumProp "lsb" f}] = #{fn f};
%{endforall}
       end
%{endforall}
    endcase
  end
end
|]

syncUpdate rs fs es = toStrict $ renderMarkup [compileText|
always @(posedge clk or negedge rst_l) begin
  if (!rst_l) begin
    ack   <= 1'b0;
    err   <= 1'b0;
    rdata <= '0;
%{forall f <- fs}
    #{fn f} <= #{getNum f "reset"};
%{endforall}
  end else begin
    ack   <= _v_ack;
    err   <= _v_err;
    rdata <= _v_rdata;
%{forall f <- fs}
    #{fn f} <= _combUpdate_#{fn f}.next;
%{if isIntrField f}
    _r_#{fn f}_sticky <= _combUpdate_#{fn f}.sticky;
%{endif}
%{endforall}
%{forall r <- rs}
%{if isIntrReg r}
    #{fn r}_intr <= #{intrSummary r};
%{endif}
%{if isHaltReg r}
    #{fn r}_halt <= #{haltSummary r};
%{endif}
%{endforall}
%{forall e <- es}
    #{e ^. xname}_rd <= rd && _readMux.#{e ^. xname}_acc;
    #{e ^. xname}_wr <= wr && _readMux.#{e ^. xname}_acc;
    if (_readMux.#{e ^. xname}_acc)
       #{e ^. xname}_addr <= addr - #{e ^. xbase};
    if (#{e ^. xname}_ack) begin
      ack <= 1'b1;
      rdata <= #{e ^. xname}_rdata;
    end
%{endforall}
  end
end
|]
  where intrSummary r = punctuate " || " (map intrFields (getFields r))
        intrFields f =
           case (isPropSet f "enable", isPropSet f "enablemask") of
              (False, False) -> base <> ".intr"
              (False, True)  -> parens (base <> ".intr && !" <> getLit f "enablemask")
              (True,  False) -> parens (base <> ".intr && " <> getLit f "enable")
           where base = "_combUpdate_" <> fn f
        haltSummary r = punctuate " || " (map haltFields (getFields r))
        haltFields f =
           case (isPropSet f "haltenable", isPropSet f "haltmask") of
              (False, False) -> base <> ".halt"
              (False, True)  -> parens (base <> ".halt && !" <> getLit f "haltmask")
              (True,  False) -> parens (base <> ".halt && " <> getLit f "haltenable")
           where base = "_combUpdate_" <> fn f

pt :: Text -> P.Doc ann
pt = pretty

ifStmt :: P.Doc a -> [P.Doc a] -> P.Doc a
ifStmt c b =
   case length b of
     1 -> P.hang 3 (P.vcat $ iflead:b)
     _ -> P.vcat [P.hang 3 (P.vcat $ (iflead <+> pt "begin"):b), pt "end"]
   where iflead = pt "if" <+> P.parens c


data VlogF a
  = Stmt Text
  | Block (Maybe Text) [a]
  | If a a
  | AlwaysComb a
  | Case a [(a, a)] deriving (Show, Eq, Functor)


vStmt        = Fix . Stmt
vBlock x y   = Fix $ Block x y
vIf x y      = Fix $ If x y
vCase x y    = Fix $ Case x y
vAlwaysComb  = Fix . AlwaysComb

bAssign lhs rhs = vStmt (lhs <> " = " <> rhs <> ";")


prettyVlog (Stmt a) = pretty a
prettyVlog (Block Nothing [a]) = uHang (P.vcat [P.emptyDoc, a])
prettyVlog (Block Nothing a) = pt "begin" <> uHang (P.vcat a) <> pt "end"
prettyVlog (Block (Just n) a) = pt "begin :" <+> pt n <> uHang (P.vcat a) <> P.line <> pt "end"
prettyVlog (AlwaysComb b) = pt "always @(*)" <+> b

prettyVlog (If e b) = pt "if" <+> P.parens e <> uHang b
prettyVlog (Case e b) = pt "case" <> P.parens e <> uHang (P.vcat (map f b))
   where f (e, b) = e <> P.colon <> uHang b

uHang l = P.nest 3 (P.line <> l)


combUpdate :: [Fix ElabF] -> P.Doc a
combUpdate rs = P.vcat (map goR rs)
  where goR r = (P.vcat . map (cata prettyVlog) . catMaybes) (map (goF r) (getFields r))
        goF r f =  Just $ vAlwaysComb $ vBlock (Just $ "_combUpdate_" <> fn f) $
                   catMaybes ([ (Just . vStmt) "reg next;"
                              , if isIntrField f then (Just . vStmt) "reg intr;" else Nothing
                              , if isIntrField f then (Just . vStmt) ("reg " <> arrayF f <> " sticky;") else Nothing
                              , if isHaltField f then (Just . vStmt) "reg halt;" else Nothing
                              , Just (bAssign "next" (fn f))
                              , fieldHWUpdate f ] ++
                               fieldSWUpdate r f  ++
                             [ fieldCTRUpdate f
                             , fieldIntrUpdate f
                             , fieldStickyUpdate f
                             ])

pretty2text = renderStrict . P.layoutPretty P.defaultLayoutOptions

fieldHWUpdate :: Fix ElabF -> Maybe (Fix VlogF)
fieldHWUpdate f =
   case (hwWritable, isIntrField f, isPropActive f "we", isPropActive f "wel") of
      (False,    _,     _,     _) -> Nothing
      (True,  True, False, False) -> Just maskedAssign
      (True,  True,  True, False) -> Just (vIf we  maskedAssign)
      (True,  True, False,  True) -> Just (vIf wel maskedAssign)
      (True, False, False, False) -> Just assign
      (True, False,  True, False) -> Just (vIf we  assign)
      (True, False, False,  True) -> Just (vIf wel assign)
   where
      hwWritable = getEnum f "hw" `elem` ["rw", "wr", "w"]
      we  = vStmt $ getLit f "we"
      wel = vStmt $ "!" <> getLit f "wel"
      assign = bAssign "next" hw_data
      maskedAssign = bAssign "next" ("(next & " <> sticky <> ") | (" <> 
                                     hw_data <> " & ~" <> sticky <> ")")
      hw_data = fn f <> "_wrdat"
      sticky = "_r_" <> fn f <> "_sticky"


fieldSWUpdate :: Fix ElabF -> Fix ElabF -> [Maybe (Fix VlogF)]
fieldSWUpdate r f =
   if access == RO
   then []
   else case access of
      RW    -> [wracc normal]
      RC    -> [rdacc clr]
      RS    -> [rdacc set]
      WRC   -> [wracc normal, rdacc clr]
      WRS   -> [wracc normal, rdacc set]
      WS    -> [wracc clr]
      WC    -> [wracc set]
      WSRC  -> [wracc set, rdacc clr]
      WCRS  -> [wracc clr, rdacc set]
      W1C   -> [wracc w1clr]
      W1S   -> [wracc w0set]
      W0C   -> [wracc w0clr]
      W0S   -> [wracc w0set]
      W1SRC -> [wracc w1set, rdacc clr]
      W1CRS -> [wracc w1clr, rdacc set]
      W0SRC -> [wracc w0set, rdacc clr]
      W0CRS -> [wracc w0clr, rdacc set]
      WO    -> [wracc normal]
      WOC   -> [wracc clr]
      WOS   -> [wracc set]
   where wracc x  = Just $ vIf (vStmt $ "_readMux." <> fn r <> "_acc" <> " && wr") x
         rdacc x  = Just $ vIf (vStmt $ "_readMux." <> fn r <> "_acc" <> " && rd") x
         clr     = bAssign "next" (zeros (getNumProp "fieldwidth" f))
         set     = bAssign "next" (onesF f)
         normal  = bAssign "next" wdata
         w1clr   = bAssign "next" (fn f <> "_wrdat" <> " && ~"  <> wdata)
         w1set   = bAssign "next" (fn f <> "_wrdat" <> " || "   <> wdata)
         w0clr   = bAssign "next" (fn f <> "_wrdat" <> " && "   <> wdata)
         w0set   = bAssign "next" (fn f <> "_wrdat" <> " || ~"  <> wdata)
         wdata   = "wdata" <> "[" <> msb <> ":" <> lsb <> "]"
         msb     = (T.pack . show) (getNumProp "msb" f)
         lsb     = (T.pack . show) (getNumProp "lsb" f)
         access  = calcAccess f

zeros w = (T.pack . show) w <> "'b0"
ones  w = "{" <> (T.pack . show) w <> "{1'b1}}"

zerosF f = zeros (getNumProp "fieldwidth" f)
onesF f = ones (getNumProp "fieldwidth" f)

fieldIntrUpdate :: Fix ElabF -> Maybe (Fix VlogF)
fieldIntrUpdate f =
   if not (isIntrField f)
   then Nothing
   else Just $ bAssign "intr" (case intrType f of
                                Level    -> "|next"
                                Posedge  -> "|(next & ~" <> prev
                                Negedge  -> "|(~next & " <> prev
                                Bothedge -> "next ~= " <> prev
                              )
   where prev = ""


fieldStickyUpdate :: Fix ElabF -> Maybe (Fix VlogF)
fieldStickyUpdate f =
   if not (isIntrField f)
   then Nothing
   else Just $ case stickyType of
                 NonSticky -> bAssign "sticky" "0"
                 StickyBit -> bAssign "sticky" "sticky | next"
                 Sticky    -> bAssign "sitcky" (ones (getNumProp "fieldwidth" f))
   where
      stickyType = StickyBit

fieldCTRUpdate :: Fix ElabF -> Maybe (Fix VlogF)
fieldCTRUpdate f =
   case (isUpCounter f, isDownCounter f) of
      (False, False) -> Nothing
      (True,  False) -> Just (vBlock (Just "ctr") (upDecl ++ upCtrBlock))
      (False, True)  -> Just (vBlock (Just "ctr") (dwDecl ++ dwCtrBlock))
      (True,  True)  -> Just (vBlock (Just "ctr") (udDecl ++ caseBlock))
   where
      upDecl = [vStmt "reg incrsaturate;", vStmt "reg overflow;"]
      dwDecl = [vStmt "reg decrsaturate;", vStmt "reg underflow;"]
      udDecl = upDecl ++ dwDecl ++ [vStmt "reg wrap"]

      uflowChk = if isPropSet f "decrsaturate"
                 then [vIf (vStmt $ "underflow || next > " <> getAsLit f "decrsaturate")
                          (bAssign "next" (getAsLit f "decrsaturate"))]
                 else []

      oflowChk = if isPropSet f "incrsaturate"
                 then [vIf (vStmt $ "overflow || next > " <> getAsLit f "incrsaturate")
                          (bAssign "next" (getAsLit f "incrsaturate"))]
                 else []

      upCtrBlock =    [vIf (vStmt (getLit f "incr")) (bAssign "{overflow, next}" ("next + " <> getAsLit f "incrvalue"))]
                   ++ oflowChk
                   ++ [bAssign "incrsaturate" ("next == " <> getAsLit f "incrsaturate")]

      dwCtrBlock = [  vIf (vStmt (getLit f "incr")) (bAssign "{underflow, next}" ("next + " <> getAsLit f "decrvalue"))]
                   ++ uflowChk
                   ++ [bAssign "decrsaturate" ("next == " <> getAsLit f "decrsaturate")]

      udCtrBlock = [ bAssign "{wrap, next}" ("next -" <> getAsLit f "decrvalue" <> " + " <> getAsLit f "incrvalue")
                   , vIf (vStmt "wrap") (vBlock Nothing [ bAssign "underflow" (getAsLit f "decrvalue" <> " > " <> getAsLit f "incrvalue")
                                                , bAssign "overflow" "~underflow"])
                   ] ++ uflowChk ++ oflowChk

      caseBlock = [vCase (vStmt $ braces (punctuate "," [getLit f "incr", getLit f "decr"]))
                         [ (vStmt ("2'b01" :: Text), vBlock Nothing dwCtrBlock)
                         , (vStmt ("2'b10" :: Text), vBlock Nothing upCtrBlock)
                         , (vStmt ("2'b11" :: Text), vBlock Nothing udCtrBlock)]]


intrType f    = snd (getIntr f "intr")
isIntrField f = intrType f /= NonIntr
isIntrReg r   = any isIntrField (r ^. _Fix . einst)

isHaltField f = any (isPropSet f) ["haltenable", "haltmask"]
isHaltReg r   = any isHaltField (r ^. _Fix . einst)

punctuate p = go
  where
    go []     = ""
    go [d]    = d
    go (d:ds) = (d <> p) <> go ds

braces p = "{" <> p <> "}"
parens p = "(" <> p <> ")"

pushOffset :: Integer -> Fix ElabF -> Fix ElabF
pushOffset o e =
  case e ^. _Fix . etype of
    Field -> e
    Array -> e & _Fix . eoffset .~ newoffset
               & _Fix . einst . traverse %~ pushOffset o
    _     -> e & _Fix . eoffset .~ newoffset
               & _Fix . einst . traverse %~ pushOffset newoffset

  where newoffset = e ^. _Fix . eoffset + o


data IODir = Input | Output | Inout | InternalReg deriving (Show)
data VIO = VIO IODir Text Integer deriving (Show)

addIO :: VIO -> State [VIO] ()
addIO v = modify (\x -> v:x)

initIO =
  [ VIO Input "rd"      1
  , VIO Input  "wr"     1
  , VIO Input  "wdata" 32
  , VIO Input  "addr" 32
  , VIO Output "rdata" 32
  , VIO Output "ack"    1
  , VIO Output "err"    1
  , VIO Input "rst_l"    1
  , VIO Input "clk"    1
  , VIO InternalReg "_v_rdata" 32
  , VIO InternalReg "_v_ack" 1
  , VIO InternalReg "_v_err" 1
  ]

f' :: [(Bool, Maybe VIO, Maybe PropRHS)] -> Maybe PropRHS -> State [VIO] (Maybe PropRHS)
f' p d = go p
   where go [] = return d
         go ((pred, io, prop):xs) = if pred
                                    then do
                                      mapM_ addIO io
                                      return (if isJust prop then prop else d)
                                    else go xs

writeType = S.fromList ["rw", "wr", "w"]
readType = S.fromList ["rw", "wr", "r"]


isHWReadable f   = S.member (getEnum f "hw") readType
isHWWritable f   = S.member (getEnum f "hw") writeType ||
                   any (isPropActive f) ["we", "wel"]
isIntr f         = snd (getIntr f "intr") /= NonIntr
isCounter f      = getBool f "counter"
isUpCounter f    = getBool f "counter" && (anyIncrSet f || not (anyDecrSet f))
isDownCounter f  = getBool f "counter" && anyDecrSet f
anyIncrSet f = any (isPropSet f) ["incr", "incrsaturate", "incrthreshold"]
anyDecrSet f = any (isPropSet f) ["decr", "decrsaturate", "decrthreshold"]

--Convert RHS references to signal names
--Add IO as needed
convertProps :: Fix ElabF -> State [VIO] (Fix ElabF)
convertProps e = do
  ni <- mapM convertProps (e ^. _Fix . einst)
  let e' = e & _Fix . einst .~ ni
  np <- foldM inspectProp (e' ^. _Fix . eprops) l1
  let e'' = e' & _Fix . eprops .~ np
  when (e'' ^. _Fix . eext == Internal) $
     mapM_ (uncurry when)
       [ (isField e && isHWReadable e'', addIO $ VIO Output (fn e) (getNum e "fieldwidth"))
       , (isField e && isHWWritable e'', addIO $ VIO Input (fn e <> "_wrdat") (getNum e "fieldwidth"))
       , (isField e && not (isHWReadable e''), addIO $ VIO InternalReg (fn e) (getNum e "fieldwidth"))
       , (isReg e   && isIntrReg e'',    addIO $ VIO Output (fn e <> "_intr") 1)
       , (isField e && isIntrField e'',  addIO $ VIO InternalReg ("_r_" <> fn e <> "_sticky") (getNum e "fieldwidth"))
       , (isReg e   && isHaltReg e'',    addIO $ VIO Output (fn e <> "_halt") 1)
       ]
  return e''
  where inspectProp :: M.Map Text (Maybe PropRHS) -> (Text, [(Bool, Maybe VIO, Maybe PropRHS)]) -> State [VIO] (M.Map Text (Maybe PropRHS))
        inspectProp m (k, act) =
          case k `M.lookup` m of
            Just a      -> f' act a >>= \x -> return $ M.insert k x m
            Nothing     -> return m
        upC = isUpCounter e
        l1 = [ ("we",             [ ( isHWWritable e && (safeGetBool e "we" == Just True)
                                    , Just (VIO Input (fn e <> "_" <>  "we") 1)
                                    , Just (PropLit (fn e <> "_" <>  "we"))
                                    )
                                  ])
             , ("incr",           [ ( isCounter e && not (isPropSet e "incr")
                                    , Just (VIO Input (fn e <> "_" <>  "incr") (fromMaybe 1 (safeGetNum e "incrwidth")))
                                    , Just (PropLit (fn e <> "_" <>  "incr"))
                                    )
                                  ])
             , ("incrvalue",      [ ( upC && isJust (safeGetNum e "incrwidth")
                                    , Nothing
                                    , Just (PropLit (fn e <> "_" <>  "incr"))
                                    )
                                  , ( upC && not (isPropSet e "incrvalue")
                                    , Nothing
                                    , Just (PropLit "1")
                                    )
                                  ])
             , ("incrsaturate",   [ ( upC && not (isPropSet e "incrsaturate")
                                    , Nothing
                                    , Just (PropLit (onesF e))
                                    )
                                  ])
             , ("incrthreshold",  [ ( upC && (safeGetBool e "incrthreshold" == Just True)
                                    , Nothing
                                    , Just (PropLit (onesF e))
                                    )
                                  ])
             , ("swmod",          [ ( getBool e "swmod" , Just (VIO Output (fn e <> "_" <>  "swmod") 1), Nothing) ])
             , ("swacc",          [ ( getBool e "swacc" , Just (VIO Output (fn e <> "_" <>  "swacc") 1), Nothing) ])
             ]

pushPostProp :: Fix ElabF -> Fix ElabF
pushPostProp e = foldl (.) id (map f $ e ^. _Fix . epostProps)
                              (e & _Fix . einst . traverse %~ pushPostProp)

   where f (path, t, prop, rhs) =
           case rhs of
             (PropRef rPath rProp) ->
               case rProp of
                  Just rrProp ->
                     case (isCtrSig, isUpdateSig, isWire) of
                       (True,  False, False) -> setPostProp (t, prop, PropLit ("_combUpdate_" <> ff <> ".ctr." <> rrProp))
                       (False,  True, False) -> setPostProp (t, prop, PropLit ("_combUpdate_" <> ff <> "." <> rrProp))
                       (False, False,  True) -> setPostProp (t, prop, PropLit (ff <> "_" <> rrProp))
                     where
                       isCtrSig = rrProp `elem` ["incrsaturate", "overflow", "decrstaturate", "underflow"]
                       isUpdateSig = rrProp `elem` ["next", "intr"]
                       isWire = rrProp `elem` ["we", "wel", "swacc", "swmod", "incr"]
                  Nothing -> setPostProp (t, prop, PropLit ff)
               where ff = (fn . fromJust) (e ^? runTraversal t)
             _          -> setPostProp (t, prop, rhs)


isReg e   = e ^. _Fix . etype == Reg
isField e = e ^. _Fix . etype == Field


getElem :: (Fix ElabF -> Bool) -> Implementation -> Fix ElabF -> [Fix ElabF]
getElem t fe e =
    case (t e, fe == NotSpec || fe == e ^. _Fix . eext) of
       (True, True)  -> [e]
       (True, False) -> []
       (False,    _) -> concatMap (getElem t fe) (e ^. _Fix . einst)

getFields = getElem isField NotSpec


verilog x = P.vcat $ map pretty [header (x ^. _Fix . ename) (addExt es io), wires io, readMux rs es, syncUpdate rs fs es, pretty2text (combUpdate rs), footer]
   where rs = getElem isReg   Internal x''
         fs = getElem isField Internal x''
         es = filterExt x'
         x'  = (pushPostProp . pushOffset 0) (evalState (fqName (pruneMap Addrmap x)) [])
         (x'', io) = runState (convertProps x') initIO

pruneMap :: CompType -> Fix ElabF -> Fix ElabF
pruneMap t e = (e & _Fix . einst %~ filter (\x -> x ^. _Fix . etype /= t)) & _Fix . einst . traverse %~ pruneMap t


