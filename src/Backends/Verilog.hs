{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Backends.Verilog
  ( verilog
  ) where

import Control.Lens
import Control.Monad.State
import Data.Functor.Foldable
import Data.Text.Lazy (toStrict)
import Data.Text.Prettyprint.Doc ((<>), (<+>), pretty)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Data.Text as T
import Text.Blaze.Renderer.Text
import Text.Heterocephalus
import Text.Show.Deriving
import Debug.Trace
import Data.Maybe (catMaybes)

import Elab2
import Props
import Types

data ExtInfo = ExtInfo {
  _xbase :: Integer,
  _xlast :: Integer,
  _xname :: Text
}

makeLenses ''ExtInfo

data RegsFilter = FilterInternal | FilterExternal | FilterNone

getProp t e p =
  case e ^? _Fix . eprops . ix p . _Just . t of
    Just b -> b
    _ -> trace (show (e ^. _Fix . ename, p)) (error "you dun f'd up")

getBool = getProp _PropBool
getNum  = getProp _PropNum
getEnum = getProp _PropEnum
getIntr = getProp _PropIntr
getRef  = getProp _PropRef
getLit  = getProp _PropLit

referenced e p = False -- S.member p (e ^. _Fix . propRef)

getPropF t e p =
  case e ^? eprops . ix p . _Just . t of
    Just b -> b
    _ -> error "you dun f'd up"

getBoolF = getPropF _PropBool
getNumF  = getPropF _PropNum
getEnumF = getPropF _PropEnum
getLitF  = getPropF _PropLit

filterExt :: Fix ElabF -> [ExtInfo]
filterExt x = cata f x
  where f a = 
          case a ^. etype of
            Field -> []
            Array -> childRanges
            otherwise ->
              case getBoolF a "sharedextbus" of
                True  -> case (minimumOf (traverse . xbase) childRanges,
                               maximumOf (traverse . xlast) childRanges) of
                  (Nothing, _) -> []
                  (_, Nothing) -> []
                  (Just b, Just l) -> [ExtInfo {_xbase = b, _xlast = l, _xname = a ^. ename}]
                False -> case (a ^. etype, a ^. eext) of
                           (Reg, False) -> []
                           (Reg, True) -> [ExtInfo {_xbase = l, _xlast = l, _xname = a ^. ename}]
                           otherwise -> childRanges
            where
              childRanges = map (over xname (combine (a ^. etype) (a ^. ename))) (concat (a ^. einst))
              l = a ^. eoffset
              combine Addrmap n = id
              combine Array n = (n <>)
              combine _ n = ((n <> "_") <>)

fqName :: Fix ElabF -> State [(CompType, Text)] (Fix ElabF)
fqName e = do
  modify (\x -> x ++ [(e ^. _Fix . etype, e ^. _Fix . ename)])
  p <- get
  r <- mapM fqName (e ^. _Fix . einst)
  let u = e & _Fix . eprops %~ assignProp "fqname" (PropLit (pathName p))
            & _Fix . einst  .~ r
  modify init
  return u
  where
    pathName = pathName' ""
    pathName' p [] = p
    pathName' p ((_, n):[]) = p <> n
    pathName' p ((t1, n1):xs@((t2, _):_)) = let delim = case (t1, t2) of
                                                 (Array, _) -> ""
                                                 (_, Field) -> "__"
                                                 _          -> "_"
                                            in pathName' (p <> n1 <> delim) xs

pushOffset :: Integer -> Fix ElabF -> Fix ElabF
pushOffset o e =
  case e ^. _Fix . etype of
    Field -> e
    Array -> e & _Fix . eoffset .~ newoffset
               & _Fix . einst . traverse %~ pushOffset o
    _     -> e & _Fix . eoffset .~ newoffset
               & _Fix . einst . traverse %~ pushOffset newoffset

  where newoffset = e ^. _Fix . eoffset + o


isReg e   = e ^. _Fix . etype == Reg
isField e = e ^. _Fix . etype == Field

getElem :: (Fix ElabF -> Bool) -> RegsFilter -> Fix ElabF -> [Fix ElabF]
getElem t fe e =
  if t e
    then case fe of
      FilterNone -> [e]
      FilterInternal -> if e ^. _Fix . eext then [] else [e]
      FilterExternal -> if e ^. _Fix . eext then [e] else []
    else
      concatMap (getElem t fe) (e ^. _Fix . einst)

verilog x = P.vcat $ map pretty [header (x ^. _Fix . ename) rs fs es, wires rs fs es, readMux rs es, syncUpdate rs fs es, pretty2text (combUpdate rs), footer]
   where rs = getElem isReg FilterInternal x'
         fs = getElem isField FilterInternal x'
         es = []--filterExt x'
         x'  = pushOffset 0 (evalState (fqName x) [])

getFields = getElem isField FilterNone

wires rs fs es = toStrict $ renderMarkup [compileText|
reg [31:0] _v_rdata;
reg _v_ack;
reg _v_err;

%{forall f <- fs}
%{if snd (getIntr f "intr") /= NonIntr}
reg #{arrayF f}_r_#{fns f "sticky"};
%{endif}
%{endforall}|]



clog2 :: Integer -> Integer
clog2 x = ceiling (logBase 2 (fromIntegral x))


header :: Text -> [Fix ElabF] -> [Fix ElabF] -> [ExtInfo] -> Text
header n rs fs es = toStrict $ renderMarkup [compileText|
`default_nettype none
module #{n} (
  input  wire clk,
  input  wire rst_l,

%{forall f <- fs}
%{if hw_readable f}
  output reg #{arrayF f}  #{fn f},
%{endif}
%{if hw_writable f}
  input wire #{arrayF f}  #{fn f}_wrdat,
%{endif}
%{if getBool f "we"}
  input wire #{fns f "we"},
%{endif}
%{if getBool f "wel"}
  input wire #{fns f "wel"},
%{endif}
%{if getBool f "swacc"}
  input wire #{fns f "swacc"},
%{endif}
%{if getBool f "swmod"}
  input wire #{fns f "swmod"},
%{endif}
%{if getBool f "counter"}
  input wire #{array' (getNum f "incrwidth")} #{fns f "incr"},
  input wire #{array' (getNum f "incrwidth")} #{fns f "decr"},
%{endif}
%{endforall}
%{forall e <- es}
  output reg #{e ^. xname}_rd,
  output reg #{e ^. xname}_wr,
  output reg [#{eaddr e}:0] #{e ^. xname}_addr,
  output reg [31:0] #{e ^. xname}_wdata,
  input wire [31:0] #{e ^. xname}_rdata,
  input wire [31:0] #{e ^. xname}_ack,
%{endforall}
%{forall r <- rs}
%{if isIntrReg r}
  output reg #{fn r}_intr,
%{endif}
%{endforall}

  input  wire        rd,
  input  wire        wr,
  input  wire [31:0] wdata,
  input  wire [31:0] addr,
  output reg  [31:0] rdata,
  output reg         ack,
  output reg         err
);
|]
  where
    eaddr e = clog2 (e ^. xlast - e ^. xbase) - 1
    hw_writable f = any (getEnum f "hw" ==) ["rw", "wr", "w"]
    hw_readable f = any (getEnum f "hw" ==) ["rw", "wr", "r"]
    iw f w p = toStrict $ renderMarkup [compileText|%{if getBool f p}input wire #{array' w} #{fn f}_#{p},%{endif}|]

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
    if (addr >= #{e ^. xbase}) && (addr <= #{e ^. xlast})
       #{e ^. xname}_acc = 1'b1;
       _v_err = 1'b0;
       _v_ack = 1'b0;
    end
%{endforall}

    case (1'b1)
%{forall r <- rs}
       #{fn r}_acc : begin
%{forall f <- getElem isField FilterInternal r}
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
%{endforall}
%{forall e <- es}
    #{e ^. xname}_rd <= rd && #{e ^. xname}_acc;
    #{e ^. xname}_wr <= wr && #{e ^. xname}_acc;
    if (#{e ^. xname}_acc)
       #{e ^. xname}_addr <= addr - #{e ^. xbase};
%{endforall}
  end
end
|]
  where intrSummary r = punctuate " || " (map (\x -> "_combUpdate_" <> fn x <> ".intr") (getFields r))
      --intrFields r = filter (\x -> snd (getIntr x "intr") /= NonIntr) (r ^. _Fix . einst)
      --intrSigs r = (renderStrict . P.layoutPretty P.defaultLayoutOptions) $ P.hcat (P.punctuate " || " [pretty $ "_v_" <> fns f "intr" | f <- intrFields r])

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


vStmt     = Fix . Stmt
vBlock x y = Fix $ Block x y
vIf x y    = Fix $ If x y
vCase x y  = Fix $ Case x y
vAlwaysComb  = Fix . AlwaysComb

bAssign lhs rhs = vStmt (lhs <> " = " <> rhs <> ";")


prettyVlog (Stmt a) = pretty a
prettyVlog (Block Nothing (a:[])) = uHang (P.vcat [P.emptyDoc, a])
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
                              , fieldHWUpdate f ] ++
                               fieldSWUpdate r f  ++
                             [ fieldCTRUpdate f
                             , fieldIntrUpdate f
                             , fieldStickyUpdate f
                             ])

pretty2text = renderStrict . P.layoutPretty P.defaultLayoutOptions

sfix' f s = f ^. _Fix . ename <> "_" <> s

fieldHWUpdate :: Fix ElabF -> Maybe (Fix VlogF)
fieldHWUpdate f =
   case (hwWritable, isIntrField f, getBoolProp "we" f, getBoolProp "wel" f) of
      (False,    _,     _,     _) -> Nothing
      (True,  True, False, False) -> Just maskedAssign
      (True,  True,  True, False) -> Just (vIf we  maskedAssign)
      (True,  True, False,  True) -> Just (vIf wel maskedAssign)
      (True, False, False, False) -> Just assign
      (True, False,  True, False) -> Just (vIf we  assign)
      (True, False, False,  True) -> Just (vIf wel assign)
   where
      hwWritable = any (getEnum f "hw" ==) ["rw", "wr", "w"]
      we  = vStmt $ sfix' f "we"
      wel = vStmt $ "!" <> sfix' f "wel"
      assign = bAssign "next" hw_data
      maskedAssign = bAssign "next" ("(next & " <> sticky <> ") | (" <> 
                                     hw_data <> " & ~" <> sticky <> ")")
      hw_data = (fn f) <> "_wrdat"
      sticky = "_r_" <> (fn f) <> "_sticky"

--fieldSWUpdate :: Fix ElabF -> [Maybe (Fix VlogF)]
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
   where wracc x  = Just $ vIf (vStmt $ "_readMux." <> (fn r) <> "_acc" <> " && wr") x
         rdacc x  = Just $ vIf (vStmt $ "_readMux." <> (fn r) <> "_acc" <> " && rd") x
         clr     = bAssign "next" (zeros (getNumProp "fieldwidth" f))
         set     = bAssign "next" (ones  (getNumProp "fieldwidth" f))
         normal  = bAssign "next" wdata
         w1clr   = bAssign "next" (sfix' f "" <> " && ~"  <> wdata)
         w1set   = bAssign "next" (sfix' f "" <> " || "   <> wdata)
         w0clr   = bAssign "next" (sfix' f "" <> " && "   <> wdata)
         w0set   = bAssign "next" (sfix' f "" <> " || ~"  <> wdata)
         wdata   = "wdata" <> "[" <> msb <> ":" <> lsb <> "]"
         msb     = (T.pack . show) (getNumProp "msb" f)
         lsb     = (T.pack . show) (getNumProp "lsb" f)
         access  = calcAccess f

zeros w = (T.pack . show) w <> "'b0"
ones  w = "{" <> (T.pack . show) w <> "{1'b0}}"

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

intrSummary :: Fix ElabF -> Maybe (Fix VlogF)
intrSummary r = 
   if not True
   then Nothing
   else Just (vBlock (Just "rintr") s)
   where
      s = [vStmt ("wire intr = " <> "1'b0")]
      --intrFields r = filter (\x -> snd (getIntr x "intr") /= NonIntr) (r ^. _Fix . einst)
      --intrSigs r = (renderStrict . P.layoutPretty P.defaultLayoutOptions) $ P.hcat (P.punctuate " || " [pretty $ "_v_" <> fns f "intr" | f <- intrFields r])

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

propSet :: Text -> Fix ElabF -> Bool
propSet = undefined

fieldCTRUpdate :: Fix ElabF -> Maybe (Fix VlogF)
fieldCTRUpdate f =
   case (upCtr, dnCtr) of
      (False, False) -> Nothing
      (True,  False) -> Just (vBlock (Just "ctr") (upDecl ++ upCtrBlock))
      (False, True)  -> Just (vBlock (Just "ctr") (dwDecl ++ dwCtrBlock))
      (True,  True)  -> Just (vBlock (Just "ctr") (udDecl ++ caseBlock))
   where
      upCtr = False
      dnCtr = False
      incr = ""
      decr = ""
      blockname = "" :: Text
      incrvalue    = "1" :: Text --getNumProp "incrvalue" f
      decrvalue    = "1" :: Text --getNumProp "decrvalue" f
      incrsaturate = "" :: Text --getNumProp "incrsaturate" f
      decrsaturate = "" :: Text --getNumProp "decrsaturate" f

      upDecl = [vStmt "reg incrsaturate", vStmt "reg overflow"]
      dwDecl = [vStmt "reg decrsaturate", vStmt "reg underflow"]
      udDecl = upDecl ++ dwDecl ++ [vStmt "reg wrap"]

      uflowChk = if propSet "decrsaturate" f
                 then [vIf (vStmt $ "underflow || next > " <> decrsaturate)
                          (bAssign "next" decrsaturate)]
                 else []

      oflowChk = if propSet "decrsaturate" f
                 then [vIf (vStmt $ "underflow || next > " <> decrsaturate)
                          (bAssign "next" decrsaturate)]
                 else []

      upCtrBlock =    [vIf (vStmt incr) (bAssign "{overflow, next}" ("next + " <> incrvalue))]
                   ++ oflowChk
                   ++ [bAssign "incrsaturate" ("next = " <> incrsaturate)]

      dwCtrBlock = [  vIf (vStmt decr) (bAssign "{underflow, next}" ("next + " <> decrvalue))]
                   ++ uflowChk
                   ++ [bAssign "decrsaturate" ("next = " <> decrsaturate)]

      udCtrBlock = [ bAssign "{wrap, next}" ("next -" <> decrvalue <> " + " <> incrvalue)
                   , vIf (vStmt "wrap") (vBlock Nothing [ bAssign "underflow" (decrvalue <> " > " <> incrvalue)
                                                , bAssign "overflow" "~underflow"])
                   ] ++ uflowChk ++ oflowChk

      caseBlock = [vCase (vStmt $ braces (punctuate "," [incr, decr]))
                         [ (vStmt ("2'b01" :: Text), vBlock Nothing dwCtrBlock)
                         , (vStmt ("2'b10" :: Text), vBlock Nothing upCtrBlock)
                         , (vStmt ("2'b11" :: Text), vBlock Nothing udCtrBlock)]]


isIntrField f = snd (getIntr f "intr") /= NonIntr
isIntrReg r = any isIntrField (r ^. _Fix . einst)

intrType f    = snd (getIntr f "intr")

punctuate p = go
  where
    go []     = ""
    go [d]    = d
    go (d:ds) = (d <> p) <> go ds

braces p = "{" <> p <> "}"
parens p = "(" <> p <> ")"
