{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Text.Blaze.Renderer.Text
import Text.Heterocephalus
import Text.Show.Deriving
import Debug.Trace

import Elab
import Props (assignProp)
import Types (ElabF(..), CompType(..), PropRHS(..), IntrType(..))

$(makePrisms ''Fix)
$(makePrisms ''PropRHS)
$(makeLenses ''ElabF)

data ExtInfo = ExtInfo {
  _ebase :: Integer,
  _elast :: Integer,
  _ename :: Text
}

makeLenses ''ExtInfo

data RegsFilter = FilterInternal | FilterExternal | FilterNone

getProp t e p =
  case e ^? _Fix . props . ix p . _Just . t of
    Just b -> b
    _ -> trace (show (e ^. _Fix . name, p)) (error "you dun f'd up")

getBool = getProp _PropBool
getNum  = getProp _PropNum
getEnum = getProp _PropEnum
getIntr = getProp _PropIntr
getRef  = getProp _PropRef
getLit  = getProp _PropLit

referenced e p = S.member p (e ^. _Fix . propRef)

getPropF t e p =
  case e ^? props . ix p . _Just . t of
    Just b -> b
    _ -> error "you dun f'd up"

getBoolF = getPropF _PropBool
getNumF  = getPropF _PropNum
getEnumF = getPropF _PropEnum
getLitF  = getPropF _PropLit

filterExt :: Fix ElabF -> [ExtInfo]
filterExt x = cata f x
  where f a@(ElabF t n p pr e i l m) =
          case t of
            Field -> []
            Array -> childRanges
            otherwise ->
              case getBoolF a "sharedextbus" of
                True  -> case (minimumOf (traverse . ebase) childRanges,
                               maximumOf (traverse . elast) childRanges) of
                  (Nothing, _) -> []
                  (_, Nothing) -> []
                  (Just a, Just b) -> [ExtInfo {_ebase = a, _elast = b, _ename = n}]
                False -> case (t, e) of
                           (Reg, False) -> []
                           (Reg, True) -> [ExtInfo {_ebase = l, _elast = l, _ename = n}]
                           otherwise -> childRanges
            where
              childRanges = map (over ename (combine t n)) (concat i)
              l = getNumF a "address"
              getAddress x  = getNum x  "address"
              combine Addrmap n = id
              combine Array n = (n <>)
              combine _ n = ((n <> "_") <>)

fqName :: Fix ElabF -> State [(CompType, Text)] (Fix ElabF)
fqName e = do
  modify (\x -> x ++ [(e ^. _Fix . etype, e ^. _Fix . name)])
  p <- get
  r <- mapM fqName (e ^. _Fix . inst)
  let u = e & _Fix . props %~ assignProp "fqname" (PropLit (pathName p))
            & _Fix . inst  .~ r
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

isReg e   = e ^. _Fix . etype == Reg
isField e = e ^. _Fix . etype == Field

getElem :: (Fix ElabF -> Bool) -> RegsFilter -> Fix ElabF -> [Fix ElabF]
getElem t fe e =
  if t e
    then case fe of
      FilterNone -> [e]
      FilterInternal -> if e ^. _Fix . ext then [] else [e]
      FilterExternal -> if e ^. _Fix . ext then [e] else []
    else
      concatMap (getElem t fe) (e ^. _Fix . inst)


verilog x = P.vcat $ map pretty [header (x ^. _Fix . name) fs es, wires fs, intrSummary rs, readMux rs es, syncUpdate fs es, combUpdate fs, footer]
   where rs = getElem isReg FilterInternal x'
         fs = getElem isField FilterInternal x'
         es = filterExt x'
         x'  = evalState (fqName x) []

wires fs = toStrict $ renderMarkup [compileText|
wire [31:0] _v_rdata;
wire _v_ack;
wire _v_err;

%{forall f <- fs}
%{case getNum f "fieldwidth"}
%{of 1}
reg _v_#{fns f "next"};
%{of n}
reg [#{n - 1}:0] _v_#{fns f "next"};
%{endcase}
%{if snd (getIntr f "intr") /= NonIntr}
wire #{fns f "intr"};
%{endif}
%{endforall}|]

intrSummary rs = toStrict $ renderMarkup [compileText|
%{forall r <- rs}
%{if hasIntr r}
wire #{fns r "intr"} = #{intrSigs r};
%{endif}
%{endforall}|]
  where
    hasIntr r = not . null $ intrFields r
    intrFields r = filter (\x -> snd (getIntr x "intr") /= NonIntr) (r ^. _Fix . inst)
    intrSigs r = (renderStrict . P.layoutPretty P.defaultLayoutOptions) $ P.hcat (P.punctuate " || " [pretty $ fns f "intr" | f <- intrFields r])


clog2 :: Integer -> Integer
clog2 x = ceiling (logBase 2 (fromIntegral x))


header :: Text -> [Fix ElabF] -> [ExtInfo] -> Text
header n fs es = toStrict $ renderMarkup [compileText|
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
  input wire #{array (getNum f "incrwidth")} #{fns f "incr"},
  input wire #{array (getNum f "incrwidth")} #{fns f "decr"},
%{endif}
%{endforall}
%{forall e <- es}
  output reg #{e ^. ename}_rd,
  output reg #{e ^. ename}_wr,
  output reg [#{eaddr e}:0] #{e ^. ename}_addr,
  output reg [31:0] #{e ^. ename}_wdata,
  input wire [31:0] #{e ^. ename}_rdata,
  input wire [31:0] #{e ^. ename}_ack,
%{endforall}

  input  wire        rd,
  input  wire        wr,
  input  wire [31:0] wdata,
  output reg  [31:0] rdata,
  output reg         ack,
  output reg         err
);
|]
  where
    eaddr e = clog2 (e ^. elast - e ^. ebase) - 1
    array :: Integer -> Text
    array 1 = toStrict ""
    array w = toStrict $ renderMarkup [compileText|[#{w - 1}:0]|]
    arrayF f = array (getNum f "fieldwidth")
    hw_writable f = any (getEnum f "hw" ==) ["rw", "wr", "w"]
    hw_readable f = any (getEnum f "hw" ==) ["rw", "wr", "r"]
    iw f w p = toStrict $ renderMarkup [compileText|%{if getBool f p}input wire #{array w} #{fn f}_#{p},%{endif}|]

footer ::  Text
footer = toStrict $ renderMarkup [compileText|
endmodule
`default_nettype wire
|]

fn f = getLit f "fqname"
fns f s = fn f <> "_" <> s

readMux :: [Fix ElabF] -> [ExtInfo] -> Text
readMux rs es  = toStrict $ renderMarkup [compileText|
always_comb begin
    v_ack = 1'b0;
    v_err = 1'b0;
    v_rdata = '0;
%{forall r <- rs}
    #{fn r} = 1'b0;
%{endforall}
%{forall e <- es}
    #{e ^. ename}_acc = 1'b0;
%{endforall}
  if (rd || wr) begin
    v_ack = 1'b1;
    case (addr)
%{forall r <- rs}
      #{getNum r "address"} : #{fn r}_acc = 1'b1;
%{endforall}
      default : v_err = 1'b1;
    endcase

    case (1'b1)
%{forall e <- es}
       (addr >= #{e ^. ebase}) && (addr <= #{e ^. elast}) : begin
           #{e ^. ename}_acc = 1'b1;
           v_err = 1'b0;
           v_ack = 1'b0;
        end
%{endforall}
    endcase

    case (1'b1)
%{forall r <- rs}
       #{fn r}_acc : begin
%{forall f <- getElem isField FilterInternal r}
          v_rdata[#{view (_Fix . msb) f}:#{view (_Fix . lsb) f}] = _r_#{fn f};
%{endforall}
       end
%{endforall}
    endcase
  end
end
|]

syncUpdate fs es = toStrict $ renderMarkup [compileText|
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
    #{fn f} <= _v_#{fn f};
%{endforall}
%{forall e <- es}
    #{e ^. ename}_rd <= rd && #{e ^. ename}_swacc;
    #{e ^. ename}_wr <= wr && #{e ^. ename}_swacc;
    if (#{e ^. ename}_swacc)
       #{e ^. ename}_addr <= addr - #{e ^. ebase};
%{endforall}
  end
end
|]

combUpdate fs = toStrict $ renderMarkup [compileText|
%{forall f <- fs}

always_comb begin
  #{next f} = r_#{fn f};
%{if isIntr f}
  #{intr f} = r_#{intr f};
  #{fns f "sticky_mask"} = r_#{fns f "sticky_mask"};
%{endif}
%{if getBool f "rclr"}
  if (rd && #{fns f "acc"})
    #{next f} = '0;
%{if isIntr f}
    #{fns f "sticky_mask"} = '0;
    #{intr f} = '0;
%{endif}
  end
%{endif}
%{if getBool f "rset"}
  if (rd && #{fns f "acc"})
    #{next f} = '1;
%{endif}
%{if hw_writable f}

  if (we && #{fns f "acc"}) begin
%{if getBool f "woset"}
    #{next f} = #{next f} | sw_wdata;
%{elseif getBool f "woclr"}
    #{next f} = #{next f} & ~sw_wdata
%{if isIntr f}
    #{intr f} = #{intr f} && #{next f} != 0);
    #{sticky_mask f} = #{sticky_mask f} & ~sw_wdata;
%{endif}
%{else}
    #{next f} = sw_wdata;
%{if isIntr f}
    #{intr f} = 0;
    #{sticky_mask f} = 'b0;
%{endif}
%{endif}
  end
%{endif}
%{if isCounter f}

%{if referenced f "underflow"}
  underflow = 0;
%{endif}
%{if referenced f "overflow"}
  overflow = 0;
%{endif}
   case (#{decr f}, #{incr f})
      reg wrap;
      2'b01 : begin
         {wrap, #{next f} = #{next f} + #{incrvalue f}
%{if getBool f "incrsaturate"}
         if (wrap || #{next f} > #{getNum f "incrsaturate"})
            #{next f} = #{getNum f "incrsaturate"};
%{endif}
%{if referenced f "overflow"}
         #{fns f "overflow"} = wrap;
%{endif}
      end
      2'b10 : begin
         {wrap, #{next f} = #{next f} + #{decrvalue f}
%{if getBool f "decrsaturate"}
         if (wrap || #{next f} < #{getNum f "decrsaturate"})
            #{next f} = #{getNum f "decrsaturate"};
%{endif}
%{if referenced f "underflow"}
         #{fns f "underflow"} = wrap;
%{endif}
      end
      2'b11 : begin
         reg underflow;
         reg overflow;

         {wrap, #{next f}} = #{next f} + #{incrvalue f} - #{decrvalue f};
         underflow = wrap && (#{decrvalue f} > #{incrvalue f})
         overflow  = wrap && (#{decrvalue f} < #{incrvalue f})
%{if referenced f "underflow"}
         #{fns f "underflow"} = underflow;
%{endif}
%{if referenced f "underflow"}
         #{fns f "overflow"} = overflow;
%{endif}
%{if getBool f "decrsaturate"}
         if (underflow || (#{next f} < #{getNum f "decrsaturate"})
            #{next f} = #{getNum f "decrsaturate"}
%{endif}
%{if getBool f "incrsaturate"}
         if (overflow || (#{next f} > #{getNum f "incrsaturate"})
            #{next f} = #{getNum f "incrsaturate"}
%{endif}
      end
   endcase
%{endif}
%{if isIntr f}

%{case intrType f}
%{of Level}
  #{intr f} = #{next f};
%{of Posedge}
  #{intr f} = |(#{next f} & ~#{prev f});
%{of Negedge}
  #{intr f} = |(~#{next f} & #{prev f});
%{of Bothedge}
  #{intr f} = #{next f} != #{prev f};
%{endcase}
%{endif}
end
%{endforall}|]
  where
    sticky f      = fst (getIntr f "intr")
    intrType f    = snd (getIntr f "intr")
    incr f        = fns f "incr"
    decr f        = fns f "decr"
    next f        = "_v_" <> fns f "next"
    intr f        = fns f "intr"
    isIntr f      = snd (getIntr f "intr") /= NonIntr
    incrvalue f   = getNum f "incrvalue"
    decrvalue f   = getNum f "decrvalue"
    prev f        = fn f
    sticky_mask f = fn f <> "_sticky_mask"
    isCounter f   = getBool f "counter"
    hw_writable f = any (getEnum f "hw" ==) ["rw", "wr", "w"]
