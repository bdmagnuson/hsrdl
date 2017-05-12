{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Backends.Verilog (
   ) where

import Control.Monad (msum)
import Data.Functor.Foldable
import Text.StringTemplate
import Control.Lens
import Text.Show.Deriving
import qualified Data.Map.Strict as M

import Text.PrettyPrint.Leijen (Doc, (<>), (<+>))
import qualified Text.PrettyPrint.Leijen as P

import Elab
import Types (ElabF(..), CompType(..), PropRHS(..))
import Data.Maybe (fromJust, fromMaybe)
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



-- io direction width signal
-- internal signal
-- readmux
--   address fields


getRegs' :: Fix ElabF -> [RegInfo]
getRegs' (Fix (ElabF t n p i l m)) =
  case t of
    Reg -> [RegInfo {
      _rname   = n,
      _rprops  = p,
      _rfields = concatMap getFields i
    }]
    Addrmap   -> concatMap getRegs' i
    Array     -> map (\x -> x & rname %~ \x -> n ++ x)         (concatMap getRegs' i)
    otherwise -> map (\x -> x & rname %~ \x -> n ++ "_" ++ x)  (concatMap getRegs' i)

getRegs x = map f (getRegs' x)
    where f a = a & rfields %~ map (\x -> x & fname %~ (\y -> ((a ^. rname) ++ "__" ++ y)))

getFields :: Fix ElabF -> [FieldInfo]
getFields (Fix (ElabF Field n p i l m)) =
  [FieldInfo {
    _fname  = n,
    _fprops = p,
    _flsb   = l,
    _fmsb   = m
  }]

getFields' x = concatMap (\x -> x ^. rfields) (getRegs x)


map1 f1 f2 []  = []
map1 f1 f2 [x] = [f2 x]
map1 f1 f2 (x:xs) = f1 x : map1 f1 f2 xs


block d1 d2 i b  = P.vcat [d1, P.indent i b, d2]

alwaysComb  b = block (P.text "always_comb begin") (P.text "end") 3 b
caseBlock v i = block (P.text "case (" <> v <> P.text ")") (P.text "endcase") 3 (P.vcat i)

ifBlock  c b = block (P.text "if (" <> c <> P.text ") begin") (P.text "end") 3 b
ifElseBlock c p n = ifBlock c p <> block (P.text "else begin") (P.text "end") 3 n

alwaysCR cn rn r a = block open close 3 body
  where open  = P.text "always @(posedge" <+> P.text cn <+> P.text "or negedge" <+> P.text rn <> P.text ") begin"
        close = P.text "end"
        body  = ifElseBlock (P.char '!' <> P.text rn) r a

caseItem :: Doc -> [Doc] -> Doc
caseItem i [x] = i <+> P.colon <+> x
caseItem i  x  = block (i <+> P.colon <+> P.text "begin") (P.text "end") 3 (P.vcat x)

getProperty  e p a = e ^? rprops . ix p . _Just . a
getfProperty e p a = e ^? fprops . ix p . _Just . a

getNumProperty   e p = getProperty  e p _PropNum
getfNumProperty e p = getfProperty e p _PropNum

boolToString False = "0"
boolToString True = "1"

getfBoolProperty e p =
  case getfProperty e p _PropBool of
    Nothing -> "0"
    Just True -> "1"
    Just False -> "0"

getfEnumProperty e p | Just (_ ,v) <- getfProperty e p _PropEnum = v

arrayW f
    | w /= 1 = P.brackets (P.integer (w - 1) <> P.colon <> P.integer 0)
    | otherwise = P.empty
    where w = f ^. fmsb - f ^. flsb + 1

arrayLR f
    | m /= l = P.brackets (P.integer m <> P.colon <> P.integer l)
    | otherwise = P.empty
    where m = f ^. fmsb
          l = f ^. flsb

modulePorts' x = map1 f1 f2 x
    where f1 x = (f2 x) <> P.char ','
          f2 x = dir <+> array <+> name
                 where dir   = P.text "input"
                       array = arrayW x
                       name  = P.text (x ^. fname)

header n a = block d1 (P.text ");") 3 (P.vcat $ modulePorts' a)
    where d1 = P.text "module" <+> P.text n <+> P.lparen

decls a = P.vcat [regs, assigns]
    where
        regs    = P.vcat $ map (\f -> P.text "reg" <+> arrayW f <+> P.text "_r_" <> fn f <> P.text ", _v_" <> fn f <> P.semi) a
        assigns = P.vcat $ map (\f -> P.text "assign" <+> fn f <+> P.equals <+> P.text "_r_" <> fn f <> P.semi) a
        fn = P.text . (^. fname)


readMux r = alwaysComb $ P.vcat [swaccDefault, P.empty, swaccAssign, P.empty, readAssign]
  where
    swaccDefault  = P.vcat $ map (\r -> (swaccName r) <+> P.text "= 1'b0;") r
    swaccAssign   = ifBlock (P.text "rd || wr") (caseBlock (P.text "addr") $ map (\r -> caseItem (getAddress r) [swaccName r <> P.text " = 1'b1;"]) r)
    swaccName x   = P.char '_' <> P.text (x ^. rname) <> P.text "_swacc"
    getAddress x  = P.integer $ fromJust (getNumProperty x "address")
    readAssign    = caseBlock (P.text "1'b1") (map (\r -> caseItem (swaccName r) (map swaccFields (r ^. rfields))) r)
    swaccFields f = P.text "rdata" <> arrayLR f <+> P.text "= _r_" <> P.text (f ^. fname) <> P.semi

syncBlock f = alwaysCR "clk" "rst_l" reset assign
  where lhs x  = P.text (x ^.fname)
        reset  = P.vcat $ map (\x -> (lhs x) <+> P.text "<=" <+> P.integer (fromJust $ getfNumProperty x "reset") <> P.semi) f
        assign = P.vcat $ map (\x -> (lhs x) <+> P.text "<=" <+> P.text "_v_" <> (lhs x) <> P.semi) f

verilog x = P.vcat $ intersperse P.empty [header (x ^. _Fix . name) f, decls f, readMux r, syncBlock f, P.text "endmodule"]
  where r = getRegs x
        f = concatMap (\x -> x ^. rfields) r

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
  ") ("]

clog2 :: Integer -> Integer
clog2 x = ceiling (log (fromIntegral x) / log 2)


fieldInst :: FieldInfo -> StringTemplate String
fieldInst f = foldl (.) id props $ fieldInstTemplate
  where props =
          [ setAttribute "width"     (getfNumProperty  f "fieldwidth")
          , setAttribute "rclr"      (getfBoolProperty f "rclr")
          , setAttribute "rset"      (getfBoolProperty f "rset")
          , setAttribute "hw"        (getfEnumProperty f "hw")
          , setAttribute "sw"        (getfEnumProperty f "sw")
          , setAttribute "counter"   (getfBoolProperty f "counter")
          , setAttribute "incrwidth" (fromMaybe 1 (msum [(getfNumProperty f "incrwidth"), clog2 <$> getfNumProperty f "incrvalue"]))
          , setAttribute "decrwidth" (fromMaybe 1 (msum [(getfNumProperty f "decrwidth"), clog2 <$> getfNumProperty f "decrvalue"]))
          ]

fields = concatMap (\x -> x ^. rfields) (getRegs out)
f1 = fields !! 0



--
--
--ggg :: (Fix ElabF) -> String
--ggg m = let a1 = setAttribute "module" (m ^. _Fix . name) header
--            a2 = setAttribute "ports"  (getF1 m) a1
--        in render a2
--
----a1 = setAttribute "module" "booyah" header
----a2 = setAttribute "ports" ["port1", "port2", "port3"] a1
--
--out2 = render a2 :: String
