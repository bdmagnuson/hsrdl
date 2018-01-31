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
    _ -> trace (show (e ^? _Fix . eprops . ix "fqname", show a, p)) (error "you dun f'd up")
  where
    a = e ^? _Fix . eprops . ix p . _Just

getBool = getProp _PropBool
getNum  = getProp _PropNum
getEnum = getProp _PropEnum
getIntr = getProp _PropIntr
getRef  = getProp _PropRef
getLit  = getProp _PropLit

safeGetProp t e p = e ^? _Fix . eprops . ix p . _Just . t

safeGetBool = safeGetProp _PropBool
safeGetNum  = safeGetProp _PropNum
safeGetEnum = safeGetProp _PropEnum
safeGetIntr = safeGetProp _PropIntr
safeGetRef  = safeGetProp _PropRef
safeGetLit  = safeGetProp _PropLit


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


header :: Text -> [VIO] -> Text
header n ios = pretty2text $ pt "`default_nettype none" <> P.line <> pt "module" <+> pretty n <+> P.parens (uHang (showIOs ios <> P.line)) <> P.semi
  where
    showIOs x = P.vcat $  P.punctuate (P.comma) (map (pretty . showIO) x)
    showIO (VIO Output n w) = "output reg " <> array' w <> n
    showIO (VIO Input n w) = "input wire " <> array' w <> n

addExt :: [ExtInfo] -> [VIO] -> [VIO]
addExt = flip (foldl go)
  where go io e = [ VIO Output ((e ^. xname) <> "_rd") 1
                  , VIO Output ((e ^. xname) <> "_wr") 1
                  , VIO Output ((e ^. xname) <> "_addr") (clog2 (e ^. xlast - e ^. xbase))
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
   where wracc x  = Just $ vIf (vStmt $ "_readMux." <> (fn r) <> "_acc" <> " && wr") x
         rdacc x  = Just $ vIf (vStmt $ "_readMux." <> (fn r) <> "_acc" <> " && rd") x
         clr     = bAssign "next" (zeros (getNumProp "fieldwidth" f))
         set     = bAssign "next" (onesF f)
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
                 then [vIf (vStmt $ "underflow || next > " <> getLit f "decrsaturate")
                          (bAssign "next" (getLit f "decrsaturate"))]
                 else []

      oflowChk = if isPropSet f "incrsaturate"
                 then [vIf (vStmt $ "overflow || next > " <> (getLit f "incrsaturate"))
                          (bAssign "next" (getLit f "incrsaturate"))]
                 else []

      upCtrBlock =    [vIf (vStmt (getLit f "incr")) (bAssign "{overflow, next}" ("next + " <> getLit f "incrvalue"))]
                   ++ oflowChk
                   ++ [bAssign "incrsaturate" ("next == " <> (getLit f "incrsaturate"))]

      dwCtrBlock = [  vIf (vStmt (getLit f "incr")) (bAssign "{underflow, next}" ("next + " <> getLit f "decrvalue"))]
                   ++ uflowChk
                   ++ [bAssign "decrsaturate" ("next == " <> (getLit f "decrsaturate"))]

      udCtrBlock = [ bAssign "{wrap, next}" ("next -" <> getLit f "decrvalue" <> " + " <> getLit f "incrvalue")
                   , vIf (vStmt "wrap") (vBlock Nothing [ bAssign "underflow" (getLit f "decrvalue" <> " > " <> getLit f "incrvalue")
                                                , bAssign "overflow" "~underflow"])
                   ] ++ uflowChk ++ oflowChk

      caseBlock = [vCase (vStmt $ braces (punctuate "," [getLit f "incr", getLit f "decr"]))
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

pushOffset :: Integer -> Fix ElabF -> Fix ElabF
pushOffset o e =
  case e ^. _Fix . etype of
    Field -> e
    Array -> e & _Fix . eoffset .~ newoffset
               & _Fix . einst . traverse %~ pushOffset o
    _     -> e & _Fix . eoffset .~ newoffset
               & _Fix . einst . traverse %~ pushOffset newoffset

  where newoffset = e ^. _Fix . eoffset + o


data IODir = Input | Output deriving (Show)
data VIO = VIO IODir Text Integer deriving (Show)

addIO :: VIO -> State [VIO] ()
addIO v = modify (\x -> (v:x))

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
  ]

f' :: [(Bool, Maybe VIO, Maybe PropRHS)] -> Maybe PropRHS -> State [VIO] (Maybe PropRHS)
f' p d = go p
   where go [] = return d
         go ((pred, io, prop):xs) = if pred
                                    then do
                                      when (isJust io) (addIO (fromJust io))
                                      return (if isJust prop then prop else d)
                                    else go xs

isHWReadable f   = any (getEnum f "hw" ==) ["rw", "wr", "r"]
isHWWritable f   = any (getEnum f "hw" ==) ["rw", "wr", "w"] ||
                   any (isPropSet f) ["we", "wel"]
isIntr f         = snd (getIntr f "intr") /= NonIntr
isCounter f      = getBool f "counter"
isUpCounter f    = (getBool f "counter") && (anyIncrSet f || (not $ anyDecrSet f))
isDownCounter f  = (getBool f "counter") && anyDecrSet f
anyIncrSet f = any (isPropSet f) ["incr", "incrsaturate", "incrthreshold"]
anyDecrSet f = any (isPropSet f) ["decr", "decrsaturate", "decrthreshold"]

--Convert RHS references to signal names
--Add IO as needed
convertProps :: Fix ElabF -> State [VIO] (Fix ElabF)
convertProps e = do
  ni <- mapM convertProps (e ^. _Fix . einst)
  let e' = e & _Fix . einst .~ ni
  np <- M.traverseWithKey inspectProp (e' ^. _Fix . eprops)
  let e'' = e' & _Fix . eprops .~ np
  when (e ^. _Fix . etype == Field && isHWReadable e'')
       (addIO $ VIO Output (fn e) (getNum e "fieldwidth"))
  when (e ^. _Fix . etype == Reg && isIntrReg e'')
       (addIO $ VIO Output (fn e <> "_intr") 1)
  return e''
  where inspectProp :: Text -> Maybe (PropRHS) -> State [VIO] (Maybe PropRHS)
        inspectProp k v =
          case (k `M.lookup` l1) of
            Just a      -> f' a v
            Nothing     -> return v
        l1 = ML.fromList 
               [ ("we",             [ ( isHWWritable e && (safeGetBool e "we" == Just True)
                                      , Just (VIO Input ((fn e) <> "_" <>  "we") 1)
                                      , Just (PropLit ((fn e) <> "_" <>  "we"))
                                      )
                                    ])
               , ("incr",           [ ( isCounter e && not (isPropSet e "incr")
                                      , Just (VIO Input ((fn e) <> "_" <>  "incr") (fromMaybe 1 (safeGetNum e "incrwidth")))
                                      , Just (PropLit ((fn e) <> "_" <>  "incr"))
                                      )
                                    ])
               , ("incrvalue",      [ ( isUpCounter e && (isJust (safeGetNum e "incrwidth"))
                                      , Nothing
                                      , Just (PropLit ((fn e) <> "_" <>  "incr"))
                                      )
                                    , ( isUpCounter e && not (isPropSet e "incrvalue")
                                      , Nothing
                                      , Just (PropLit "1")
                                      )
                                    ])
               , ("incrsaturate",   [ ( isUpCounter e && not (isPropSet e "incrsaturate")
                                      , Nothing
                                      , Just (PropLit (onesF e))
                                      )
                                    ])
               , ("incrthreshold",  [ ( isUpCounter e && (safeGetBool e "incrthreshold" == Just True)
                                      , Nothing
                                      , Just (PropLit (onesF e))
                                      )
                                    ])
               , ("swmod",          [ ( getBool e "swmod" , Just (VIO Output ((fn e) <> "_" <>  "swmod") 1), Nothing) ])
               , ("swacc",          [ ( getBool e "swacc" , Just (VIO Output ((fn e) <> "_" <>  "swacc") 1), Nothing) ])
               ]

pushPostProp :: Fix ElabF -> Fix ElabF
pushPostProp e = foldl (.) id (map f $ e ^. _Fix . epostProps)
                              (e & _Fix . einst . traverse %~ pushPostProp)

   where f (path, prop, rhs) =
           case rhs of
             (PropRef rPath (Just rProp)) ->
               case (isCtrSig, isUpdateSig, isWire) of
                 (True,  False, False) -> setPostProp (path, prop, PropLit ("_combUpdate_" <> ff <> ".ctr." <> rProp))
                 (False,  True, False) -> setPostProp (path, prop, PropLit ("_combUpdate_" <> ff <> "." <> rProp))
                 (False, False,  True) -> setPostProp (path, prop, PropLit (ff <> "_" <> rProp))
               where t = buildPropTraversal e rPath
                     ff = case t of
                            Left e -> (error . T.unpack) e
                            Right t -> (fn . fromJust) (e ^? runTraversal t)
                     isCtrSig = rProp `elem` ["incrsaturate", "overflow", "decrstaturate", "underflow"]
                     isUpdateSig = rProp `elem` ["next", "intr"]
                     isWire = rProp `elem` ["we", "wel", "swacc", "swmod", "incr"]
             _          -> setPostProp (path, prop, rhs)


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

getFields = getElem isField FilterNone

verilog x = P.vcat $ map pretty [header (x ^. _Fix . ename) (addExt es io), wires rs fs es, readMux rs es, syncUpdate rs fs es, pretty2text (combUpdate rs), footer]
   where rs = getElem isReg FilterInternal x''
         fs = getElem isField FilterInternal x''
         es = []--filterExt x'
         x'  = (pushPostProp . pushOffset 0) (evalState (fqName x) [])
         (x'', io) = runState (convertProps x') initIO



