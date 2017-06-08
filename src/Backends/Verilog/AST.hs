{-# LANGUAGE GADTs #-}
module Backends.Verilog.AST
  ( Expr (..)
  , Stmt (..)
  , Base (..)
  , ModuleItem (..)
  , IODecl
  , LHS
  ) where

import Data.Text (Text)

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
  | Instance Identifier [(Identifier, Maybe Expr)] Identifier [(Identifier, Maybe Expr)]

data Sense
  = Posedge  Identifier
  | Negedge  Identifier
  | Bothedge Identifier

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
  (:-:)       :: Expr -> Expr -> Expr
  (:+:)       :: Expr -> Expr -> Expr
  Paren       :: Expr -> Expr
  Concat      :: [Expr] -> Expr
  deriving (Show)

