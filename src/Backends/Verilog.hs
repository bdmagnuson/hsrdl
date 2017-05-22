{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Backends.Verilog
  ( verilog'
  , fff
  ) where

import Control.Monad (msum, join)
import Data.Functor.Foldable
import Text.StringTemplate
import Control.Lens
import Text.Show.Deriving
import qualified Data.Map.Strict as M

import Text.PrettyPrint.Leijen (Doc, (<>), (<+>))
import qualified Text.PrettyPrint.Leijen as P
import Debug.Trace

import Elab
import Types (ElabF(..), CompType(..), PropRHS(..))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.List (intersperse)

$(makePrisms ''Fix)
$(makePrisms ''PropRHS)
$(makeLenses ''ElabF)

instance Monoid P.Doc where
    mempty = P.empty
    mappend = (<>)

instance Stringable P.Doc where
    stFromString = P.text
    stToString = flip P.displayS "" . P.renderCompact
    --mconcatMap m k = PP.fcat . map k $ m
    --mintercalate = (PP.fcat .) . PP.punctuate
    --mlabel x y = x PP.$$ PP.nest 1 y

data FieldInfo = FieldInfo {
    _fname  :: String,
    _fprops :: M.Map String (Maybe PropRHS),
    _flsb   :: Integer,
    _fmsb   :: Integer
} deriving (Show)

makeLenses ''FieldInfo

data RegInfo = RegInfo {
    _rname   :: String,
    _rprops  :: M.Map String (Maybe PropRHS),
    _rfields :: [FieldInfo]
} deriving (Show)

makeLenses ''RegInfo


filterExt :: Fix ElabF -> [(Integer, Integer)]
filterExt  x = cata f x
  where f a@(ElabF Field n p e i l m) = []
        f a@(ElabF Array n p e i l m) = concat i
        f a@(ElabF t n p e i l m) =
          case fromJust $ ((trace (show n) p) ^? ix "sharedextbus" . _Just . _PropBool) of
            True  -> case (minimumOf (traverse . _1) childRanges,
                           maximumOf (traverse . _2) childRanges) of
              (Nothing, _) -> []
              (_, Nothing) -> []
              (Just a, Just b) -> [(a, b)]
            False -> case (t, e) of
                       (Reg, False) -> []
                       (Reg, True) -> [(l, l)]
                       otherwise -> concat i
          where
            childRanges = concat i
            l = fromJust $ p ^? ix "address" . _Just . _PropNum
            getAddress x  = fromJust (getNumProp x "address")

data RegsFilter = FilterInternal | FilterExternal | FilterNone

getRegs' :: RegsFilter -> Fix ElabF -> [RegInfo]
getRegs' fe (Fix (ElabF t n p ext i l m)) =
  case t of
    Reg -> case fe of
      FilterNone -> r
      FilterInternal -> if ext then [] else r
      FilterExternal -> if ext then r else []
    Addrmap   -> concatMap (getRegs' fe)  i
    Array     -> map (\x -> x & rname %~ \x -> n ++ x)         (concatMap (getRegs' fe) i)
    _ -> map (\x -> x & rname %~ \x -> n ++ "_" ++ x)  (concatMap (getRegs' fe) i)
    where 
      r = [RegInfo {_rname = n, _rprops = p, _rfields = concatMap getFields i}]

getRegs fe x = map f (getRegs' fe x)
    where f a = a & rfields %~ map (\x -> x & fname %~ (\y -> a ^. rname ++ "__" ++ y))

getFields :: Fix ElabF -> [FieldInfo]
getFields (Fix (ElabF Field n p ext i l m)) =
  [FieldInfo {
    _fname  = n,
    _fprops = p,
    _flsb   = l,
    _fmsb   = m
  }]


map1 f1 f2 []  = []
map1 f1 f2 [x] = [f2 x]
map1 f1 f2 (x:xs) = f1 x : map1 f1 f2 xs


block d1 d2 i b  = P.vcat [d1, P.indent i b, d2]

alwaysComb    = block (P.text "always_comb begin") (P.text "end") 3
caseBlock v i = block (P.text "case (" <> v <> P.text ")") (P.text "endcase") 3 (P.vcat i)

ifBlock  c = block (P.text "if (" <> c <> P.text ") begin") (P.text "end") 3
ifElseBlock c p n = ifBlock c p <> block (P.text "else begin") (P.text "end") 3 n

alwaysCR cn rn r a = block open close 3 body
  where open  = P.text "always @(posedge" <+> P.text cn <+> P.text "or negedge" <+> P.text rn <> P.text ") begin"
        close = P.text "end"
        body  = ifElseBlock (P.char '!' <> P.text rn) r a

caseItem :: Doc -> [Doc] -> Doc
caseItem i [x] = i <+> P.colon <+> x
caseItem i  x  = block (i <+> P.colon <+> P.text "begin") (P.text "end") 3 (P.vcat x)


arrayWi 1 = P.empty
arrayWi w = P.brackets (P.integer (w - 1) <> P.colon <> P.integer 0)

arrayW f = arrayWi (f ^. fmsb - f ^. flsb + 1)

arrayLR f
    | m /= l = P.brackets (P.integer m <> P.colon <> P.integer l)
    | otherwise = P.empty
    where m = f ^. fmsb
          l = f ^. flsb

arrayLR' f = P.brackets (P.integer m <> P.colon <> P.integer l)
    where m = f ^. fmsb
          l = f ^. flsb

getProp  e p a = e ^? rprops . ix p . _Just . a
getfProp e p a = e ^? fprops . ix p . _Just . a

getNumProp   e p = getProp  e p _PropNum

getfNumProp  e p = getfProp e p _PropNum
getfBoolProp e p = getfProp e p _PropBool

getfEnumProp e p | Just (v) <- getfProp e p _PropEnum = Just v
getfEnumProp e p | Nothing  <- getfProp e p _PropEnum = error p


fieldInstTemplate = newSTMP $ unlines [
  "srdField u_field_$inst$ #(",
  "   .WIDTH($width$),",
  "   .RCLR($rclr$),",
  "   .RSET($rset$),",
  "   .SW(\"$sw$\"),",
  "   .HW(\"$hw$\"),",
  "   .COUNTER($counter$),",
  "   .INCRWIDTH($incrwidth$),",
  "   .DECRWIDTH($decrwidth$)",
  ") (",
  "   .clk(clk),",
  "   .rst_l(rst_l),",
  "   .sw_wdata(wdata),",
  "   .hw_wdata($hw_wdata$),",
  "   .hw_we($hw_we$),",
  "   .rd(rd),",
  "   .wr(wr),",
  "   .acc($acc$),",
  "   .incr($incr$),",
  "   .decr($decr$),",
  "   .incrvalue($incrvalue$),",
  "   .decrvalue($decrvalue$),",
  "   .overflow($overflow$),",
  "   .underflow($underflow$),",
  "   .incrsaturate_lhs($incrsaturate_lhs$),",
  "   .decrsaturate_lhs($decrsaturate_lhs$),",
  "   .incrsaturate_rhs($rhscrsaturate_rhs$),",
  "   .decrsaturate_rhs($decrsaturate_rhs$),",
  "   .incrthreshold_lhs($incrthreshold_lhs$),",
  "   .decrthreshold_lhs($decrthreshold_lhs$),",
  "   .incrthreshold_rhs($rhscrthreshold_rhs$),",
  "   .decrthreshold_rhs($decrthreshold_rhs$),",
  "   .q($q$)",
  ")",
  ""
  ]

clog2 :: Integer -> Integer
clog2 x = ceiling (logBase 2 (fromIntegral x))

data Sigs =
    EmptyS
  | Lit    String
  | Input  String Integer
  | Output String Integer
  | Wire   String Integer

isInput (Input _ _)   = True
isInput _             = False
isOutput (Output _ _) = True
isOutput _            = False
isWire (Wire _ _)     = True
isWire _              = False

sig2wire EmptyS       = ""
sig2wire (Lit a)      = a
sig2wire (Input  a 1) = a
sig2wire (Output a 1) = a
sig2wire (Wire   a 1) = a
sig2wire (Input  a w) = a ++ "[" ++ show (w - 1) ++ ":0]"
sig2wire (Output a w) = a ++ "[" ++ show (w - 1) ++ ":0]"
sig2wire (Wire   a w) = a ++ "[" ++ show (w - 1) ++ ":0]"

getfprop e p =
  case e ^? fprops . ix p . _Just of
    Nothing -> Nothing
    Just (PropLit a)      -> Just (Lit a)
    Just (PropNum a)      -> Just (Lit (show a))
    Just (PropBool True)  -> Just (Lit "1")
    Just (PropBool False) -> Just (Lit "0")
    Just (PropRef a _)    -> Just (Wire "ref!" 1)
    Just (PropPath a)     -> Just (Wire "path!" 1)
    Just (PropEnum e)     -> Just (Lit e)


fieldInst :: FieldInfo -> ([Sigs], StringTemplate Doc)
fieldInst f = (sigs, foldl (.) id (map fn io ++ map fnp params) fieldInstTemplate)
  where io = ordIO ++ incrIO ++ decrIO
        n  = f ^. fname
        fw = (f ^. fmsb) - (f ^. flsb) + 1
        getBool x  = fromJust $ getfBoolProp f x
        getNum  x  = fromJust $ getfNumProp  f x
        getEnum x  = fromJust $ getfEnumProp f x
        getNumT x  = show (getNum x)
        getBoolT x = if getBool x then "1" else "0"
        ife cond (a, b, c) = if cond then (a, c) else (a, b)
        zero w = Lit $ show w ++ "'b0"
        one  w = Lit $ show w ++ "'b1"
        fn  (p, v) = setAttribute p (sig2wire v)
        fnp (p, v) = setAttribute p v
        sigs = filter (\x -> isInput x || isOutput x || isWire x) (map snd io)
        hw_wr = (getEnum "hw" == "rw") || (getEnum "hw" == "wr") || (getEnum "hw" == "w")
        hw_we = getBool "we"
        params =
          [ ("inst"    , n)
          , ("width"   , getNumT  "fieldwidth")
          , ("rclr"    , getBoolT "rclr")
          , ("rset"    , getBoolT "rset")
          , ("hw"      , getEnum  "hw")
          , ("sw"      , getEnum  "sw")
          , ("counter" , getBoolT "counter")
          ]
        ordIO =
          [ ("q"        , Output n fw)
          , ("hw_wdata" , if hw_wr then Input (n ++ "_wrdat") fw else zero fw)
          , ("hw_we"    , case (hw_wr, hw_we) of
                            (False,     _) -> zero 1
                            ( True, False) -> one 1
                            ( True,  True) -> Input (n ++ "_we") 1)
          , ("acc"      , Wire ("_" ++ n ++ "_swacc") 1)
          ]
        c = fromJust $ getfBoolProp f "counter"
        decrIO :: [(String, Sigs)]
        decrIO = map (ife c) [ ("decr"              , zero 1   , fromMaybe (Input (n ++ "_decr") 1) (getfprop f "decr"))
                             , ("decrvalue"         , zero 1   , fromMaybe (Input (n ++ "_decr") 1) (getfprop f "decr"))
                             , ("underflow"         , EmptyS    , if isJust (getfprop f "decrsaturate") then EmptyS else Lit (n ++ "_underflow"))
                             , ("decrsaturate_lhs"  , zero fw   , fromMaybe (zero fw) (getfprop f "decrsaturate"))
                             , ("decrthreshold_lhs" , zero fw   , fromMaybe (one fw) (getfprop f "decrthreshold"))
                             , ("decrsaturate_rhs"  , EmptyS    , Wire (n ++ "_decrsaturate") 1)
                             , ("decrthreshold_rhs" , EmptyS    , Wire (n ++ "_decrthreshold") 1)
                             ]
        incrIO = map (ife c) [ ("incr"              , zero 1   , fromMaybe (Input (n ++ "_incr") 1) (getfprop f "incr"))
                             , ("incrvalue"         , zero 1   , fromMaybe (Input (n ++ "_incr") 1) (getfprop f "incr"))
                             , ("overflow"          , EmptyS    , if isJust (getfprop f "incrsaturate") then EmptyS else Lit (n ++ "_underflow"))
                             , ("incrsaturate_lhs"  , zero fw   , fromMaybe (zero fw) (getfprop f "incrsaturate"))
                             , ("incrthreshold_lhs" , zero fw   , fromMaybe (one fw) (getfprop f "incrthreshold"))
                             , ("incrsaturate_rhs"  , EmptyS    , Wire (n ++ "_incrsaturate") 1)
                             , ("incrthreshold_rhs" , EmptyS    , Wire (n ++ "_incrthreshold") 1)
                             ]


fff x = show (filterExt x)

verilog' x = P.vcat $ intersperse P.empty [mheader]--, wires, insts, readMux r, P.text "endmodule", P.empty]
  where r = getRegs FilterInternal x
        f = concatMap (^. rfields) r
        (sigs, fields) = unzip (map fieldInst f) & _1 %~ concat
        insts = P.vcat $ map render fields
        mheader = header' (x ^. _Fix . name) sigs
        wires = P.vcat $ map ff (filter isWire sigs)
        ff (Wire a w) = P.text "wire" <+> arrayWi w <+> P.text a <> P.semi


header' n sigs = block d1 (P.text ");") 3 (P.vcat $ map1 f1 f2 (filter (\x -> isInput x || isOutput x) sigs))
    where d1 = P.text "module" <+> P.text n <+> P.lparen
          f1 x = f2 x <> P.char ','
          f2 (Input  a w) = P.text "input"  <+> arrayWi w <+> P.text a
          f2 (Output a w) = P.text "output" <+> arrayWi w <+> P.text a

readMux r = alwaysComb $ P.vcat $ intersperse P.empty [swaccDefault, swaccAssign, readAssign]
  where
    swaccDefault  = P.vcat $ map (\r -> swaccName r <+> P.text "= 1'b0;") r
    swaccAssign   = ifBlock (P.text "rd || wr") (caseBlock (P.text "addr") $ map (\r -> caseItem (getAddress r) [swaccName r <> P.text " = 1'b1;"]) r)
    swaccName x   = P.char '_' <> P.text (x ^. rname) <> P.text "_swacc"
    getAddress x  = P.integer $ fromJust (getNumProp x "address")
    readAssign    = P.text "rdata = 0;" P.<$$> caseBlock (P.text "1'b1") (map (\r -> caseItem (swaccName r) (map swaccFields (r ^. rfields))) r)
    swaccFields f = P.text "rdata" <> arrayLR' f <+> P.equals <+> P.text (f ^. fname) <> P.semi
