{-# LANGUAGE GADTs #-}
module Backends.Verilog.AST
  ( Expr (..)
  , Stmt (..)
  , Base (..)
  , Module (..)
  , ModuleItem (..)
  , IODecl (..)
  , IODirection (..)
  , LHS
  , Sense (..)
  , Range
  , showExpr
  , showStmt
  , showModuleItem
  , showModule
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc ((<>), (<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as P

type Identifier = Text

data Module = Module Identifier [IODecl] [ModuleItem]

type Range = (Integer, Integer)

data IODecl = IODecl IODirection Identifier (Maybe Range)

data IODirection
  = Input
  | Output
  | Inout

data ModuleItem
  = Wire  (Maybe Range) [Identifier]
  | Logic   (Maybe Range) [Identifier]
  | AlwaysComb Stmt
  | Always [Sense] Stmt
  | ContAssign LHS Expr
  | Instance Identifier [(Identifier, Maybe Expr)] Identifier [(Identifier, Maybe Expr)] deriving (Show)

data Sense
  = Posedge  Identifier
  | Negedge  Identifier
  | Bothedge Identifier deriving (Show)

data Stmt =
    If Expr Stmt [(Expr, Stmt)] (Maybe Stmt)
  | Block (Maybe Identifier) [Stmt]
  | Case Expr [(Expr, Stmt)]
  | NBAssign LHS Expr
  | Assign LHS Expr deriving (Show)

type LHS = [(Identifier, Maybe Range)]

data Base
  = Dec
  | Hex
  | Bin
  | Oct deriving (Show)

instance P.Pretty Base where
  pretty Dec = pretty 'b'
  pretty Hex = pretty 'h'
  pretty Bin = pretty 'b'
  pretty Oct = pretty 'o'

instance P.Pretty Sense where
  pretty (Posedge i) = pretty "posedge" <+> pretty i
  pretty (Negedge i) = pretty "negedge" <+> pretty i
  pretty (Bothedge i) = pretty i

data Expr where
  SizedNum    :: Integer -> Base -> Integer -> Expr
  Num         :: Integer -> Expr
  Lit         :: Text -> Expr
  LogicalNot  :: Expr -> Expr
  BitwiseNot  :: Expr -> Expr
  (:&&:)      :: Expr -> Expr -> Expr
  (:||:)      :: Expr -> Expr -> Expr
  (:&:)       :: Expr -> Expr -> Expr
  (:|:)       :: Expr -> Expr -> Expr
  (:==:)      :: Expr -> Expr -> Expr
  (:!=:)      :: Expr -> Expr -> Expr
  (:>:)       :: Expr -> Expr -> Expr
  (:<:)       :: Expr -> Expr -> Expr
  (:>=:)      :: Expr -> Expr -> Expr
  (:<=:)      :: Expr -> Expr -> Expr
  (:-:)       :: Expr -> Expr -> Expr
  (:+:)       :: Expr -> Expr -> Expr
  Paren       :: Expr -> Expr
  Concat      :: [Expr] -> Expr
  deriving (Show)

block d1 d2 i b  = P.vcat [d1, P.nest i b, d2]

showStmt (Block _ []) = pretty "begin end"
showStmt (Block (Just n) x) = pretty "begin : " <> pretty n <> P.hardline <> P.nest 3 (P.vsep $ map showStmt x) <> P.hardline <> pretty "end"
showStmt (Block Nothing x)  = pretty "begin" <> P.nest 3 (P.vcat $ (P.emptyDoc:(map showStmt x))) <> P.hardline <> pretty "end"

showStmt (Case e cs) = pretty "case" <+> P.parens (showExpr e) <> P.nest 3 (P.vcat $ (P.emptyDoc:(map showCaseItem cs))) <> P.hardline <> pretty "endcase"
  where showCaseItem (e, s) = showExpr e <+> P.colon <+> showStmt s

showStmt (If cond iftrue elseif els) =
  pretty "if" <+> P.parens (showExpr cond) <+> ptrue <+> pelseif <+> pelse
  where
    ptrue   = showStmt iftrue
    pelseif = P.vcat $ map (\(c, b) -> pretty "else if" <+> P.parens (showExpr c) <+> showStmt b) elseif
    pelse   = case els of
               Nothing -> P.emptyDoc
               (Just b) -> pretty "else" <+> showStmt b

showStmt (NBAssign lhs expr) = showLHS lhs <+> P.equals <+> showExpr expr <> P.semi
showStmt (Assign lhs expr) = showLHS lhs <+> pretty "<=" <+> showExpr expr <> P.semi

vconcat = P.encloseSep P.lbracket P.rbracket P.comma

showLHS [x] = showLHSelem x
showLHS x = vconcat (map showLHSelem x)
showLHSelem (i, Nothing) = pretty i
showLHSelem (i, Just (l, r)) = pretty i <> P.brackets (pretty l <> P.colon <> pretty r)

showExpr (SizedNum w b v) = pretty w <> pretty "'" <> pretty b <> pretty v
showExpr (Num i) = pretty i
showExpr (Lit l) = pretty l
showExpr (BitwiseNot e) = pretty "~" <> showExpr e
showExpr (LogicalNot e) = pretty "!" <> showExpr e
showExpr (e1 :&&: e2) = showExpr e1 <+> pretty "&&" <+> showExpr e2
showExpr (e1 :||: e2) = showExpr e1 <+> pretty "||" <+> showExpr e2
showExpr (e1 :&:  e2) = showExpr e1 <+> pretty "&"  <+> showExpr e2
showExpr (e1 :|:  e2) = showExpr e1 <+> pretty "|"  <+> showExpr e2
showExpr (e1 :+:  e2) = showExpr e1 <+> pretty "+"  <+> showExpr e2
showExpr (e1 :-:  e2) = showExpr e1 <+> pretty "-"  <+> showExpr e2
showExpr (e1 :==: e2) = showExpr e1 <+> pretty "==" <+> showExpr e2
showExpr (e1 :!=: e2) = showExpr e1 <+> pretty "!=" <+> showExpr e2
showExpr (e1 :>:  e2) = showExpr e1 <+> pretty ">"  <+> showExpr e2
showExpr (e1 :>=: e2) = showExpr e1 <+> pretty ">=" <+> showExpr e2
showExpr (e1 :<:  e2) = showExpr e1 <+> pretty "<"  <+> showExpr e2
showExpr (e1 :<=: e2) = showExpr e1 <+> pretty "<=" <+> showExpr e2
showExpr (Paren e1) = P.parens (showExpr e1)
showExpr (Concat e) = vconcat (map showExpr e)

showModuleItem (AlwaysComb b) = pretty "always_comb" <+> showStmt b

showModuleItem (Always sense stmt) = pretty "always @" <> P.parens (showSense sense) <+> showStmt stmt
  where showSense [] = P.emptyDoc
        showSense (s:[]) = pretty s
        showSense (s:ss) = pretty s <+> pretty "or" <+> showSense ss

sep = P.encloseSep P.emptyDoc P.emptyDoc
vcatL x = P.vcat $ (P.emptyDoc:x)

showIO (IODecl d n r) = showD <+> showR <+> showN
  where showD = case d of
                 Input  -> pretty "input"
                 Output -> pretty "output"
                 Inout  -> pretty "input"
        showR = case r of
                  Nothing -> P.emptyDoc
                  Just (m, l) -> P.brackets (pretty m <> P.colon <> pretty l)
        showN = pretty n

showModule (Module id ios i) = pretty "module" <+> pretty id <+> P.tupled (map showIO ios) <> P.semi <> vcatL (map showModuleItem i) <> pretty "endmodule"

