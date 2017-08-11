{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Backends.Verilog.AST
  ( Expr (..)
  , Stmt (..)
  , Base (..)
  , Module (..)
  , ModuleItem (..)
  , IODecl (..)
  , IODirection (..)
  , Sense (..)
  , Range
  , replaceM
  , replaceE
  , replaceS
  , reduceM
  , reduceE
  , reduceS
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc ((<>), (<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as P
import Data.Bifunctor
import qualified Data.BitVector as BV
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

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
  | ContAssign Expr Expr
  | Instance Identifier [(Identifier, Maybe Expr)] Identifier [(Identifier, Maybe Expr)] deriving (Show, Eq)

data Sense
  = Posedge  Identifier
  | Negedge  Identifier
  | Bothedge Identifier deriving (Show, Eq)

data Stmt =
    If (Expr, Stmt) [(Expr, Stmt)] (Maybe Stmt)
  | Block (Maybe Identifier) [Stmt]
  | Case Expr [(Expr, Stmt)]
  | NBAssign Expr Expr
  | Assign Expr Expr
  | Null deriving (Show, Eq)

data Base
  = Dec
  | Hex
  | Bin
  | Oct deriving (Show, Eq, Ord)

infixr 2 :||:
infixr 2 :|:
infixr 3 :&&:
infixr 3 :&:
infix 4 :==:
infix 4 :!=:
infix 4 :>=:
infix 4 :<=:
infix 4 :>:
infix 4 :<:
infixl 6 :+:
infixl 6 :-:

data Expr where
  SizedNum       :: Base -> BV.BV -> Expr
  Num            :: BV.BV -> Expr
  Ident          :: Text -> Maybe Range -> Expr
  LogicalNot     :: Expr -> Expr
  BitwiseNot     :: Expr -> Expr
  Ternary        :: Expr -> Expr -> Expr -> Expr
  ReductionAnd   :: Expr -> Expr
  ReductionOr    :: Expr -> Expr
  ReductionOrNot :: Expr -> Expr
  ReductionXor   :: Expr -> Expr
  StringLit      :: Text -> Expr
  (:&&:)         :: Expr -> Expr -> Expr
  (:||:)         :: Expr -> Expr -> Expr
  (:&:)          :: Expr -> Expr -> Expr
  (:|:)          :: Expr -> Expr -> Expr
  (:==:)         :: Expr -> Expr -> Expr
  (:!=:)         :: Expr -> Expr -> Expr
  (:>:)          :: Expr -> Expr -> Expr
  (:<:)          :: Expr -> Expr -> Expr
  (:>=:)         :: Expr -> Expr -> Expr
  (:<=:)         :: Expr -> Expr -> Expr
  (:-:)          :: Expr -> Expr -> Expr
  (:+:)          :: Expr -> Expr -> Expr
  Paren          :: Expr -> Expr
  Concat         :: [Expr] -> Expr
  deriving (Show, Eq, Ord)


replaceM :: M.Map Expr Expr -> ModuleItem -> ModuleItem
replaceM m (AlwaysComb s)     = AlwaysComb (replaceS m s)
replaceM m (Always sn s)      = Always sn (replaceS m s)
replaceM m (ContAssign e1 e2) = ContAssign (replaceE m e1) (replaceE m e2)
replaceM _ m = m

replaceES m  = bimap (replaceE m) (replaceS m)

replaceS m (If e elseif els)  = If (replaceES m e) (map (replaceES m) elseif) (fmap (replaceS m) els)
replaceS m (Block ident s)    = Block ident (map (replaceS m) s)
replaceS m (Case e c)         = Case (replaceE m e) (map (replaceES m) c)
replaceS m (NBAssign e1 e2)   = NBAssign (replaceE m e1) (replaceE m e2)
replaceS m (Assign e1 e2)     = Assign (replaceE m e1) (replaceE m e2)
replaceS m Null               = Null

replaceE m i@(Ident _ _)      = fromMaybe i (M.lookup i m)
replaceE m (LogicalNot e)     = LogicalNot (replaceE m e)
replaceE m (BitwiseNot e)     = BitwiseNot (replaceE m e)
replaceE m (ReductionAnd e)   = ReductionAnd (replaceE m e)
replaceE m (ReductionOr e)    = ReductionOr (replaceE m e)
replaceE m (ReductionOrNot e) = ReductionOrNot (replaceE m e)
replaceE m (ReductionXor e)   = ReductionXor (replaceE m e)
replaceE m (e1 :&&: e2)       = (replaceE m e1) :&&: (replaceE m e2)
replaceE m (e1 :||: e2)       = (replaceE m e1) :||: (replaceE m e2)
replaceE m (e1 :&:  e2)       = (replaceE m e1) :&:  (replaceE m e2)
replaceE m (e1 :|:  e2)       = (replaceE m e1) :|:  (replaceE m e2)
replaceE m (e1 :==: e2)       = (replaceE m e1) :==: (replaceE m e2)
replaceE m (e1 :!=: e2)       = (replaceE m e1) :!=: (replaceE m e2)
replaceE m (e1 :<:  e2)       = (replaceE m e1) :<:  (replaceE m e2)
replaceE m (e1 :>:  e2)       = (replaceE m e1) :>:  (replaceE m e2)
replaceE m (e1 :<=: e2)       = (replaceE m e1) :<=: (replaceE m e2)
replaceE m (e1 :>=: e2)       = (replaceE m e1) :>=: (replaceE m e2)
replaceE m (e1 :-:  e2)       = (replaceE m e1) :-:  (replaceE m e2)
replaceE m (e1 :+:  e2)       = (replaceE m e1) :+:  (replaceE m e2)
replaceE m (Paren e)          = Paren (replaceE m e)
replaceE m (Concat e)         = Concat (map (replaceE m) e)
replaceE _ e = e

reduceES  = bimap reduceE reduceS

pattern Zero    <- Num ((0 ==) . BV.int -> True)
pattern NonZero <- Num ((0 ==) . BV.int -> False)

zero = Num (BV.zeros 1)
one  = Num (BV.ones 1)

zeroBV = BV.zeros 1
oneBV  = BV.ones 1

foldrBV f s l = foldr f s (BV.split (BV.size l) l)

bitBlast v = BV.split (BV.size v) v

reduceM' :: ModuleItem -> ModuleItem
reduceM' (AlwaysComb s)     = AlwaysComb (reduceS s)
reduceM' (Always sn s)      = Always sn (reduceS s)
reduceM' (ContAssign e1 e2) = ContAssign (reduceE e1) (reduceE e2)
reduceM' m = m

reduceS' (If (Zero, s) (e:es) els) = If (reduceES e) (map reduceES es) (fmap reduceS els)
reduceS' (If (Zero, s) [] Nothing) = Null
reduceS' (If (Zero, s) [] (Just els)) = els
reduceS' (If (NonZero, s) _ _) = s


reduceE' (LogicalNot     Zero) = one
reduceE' (LogicalNot     NonZero) = zero
reduceE' (BitwiseNot     (Num b)) = Num (BV.not b)
reduceE' (ReductionAnd   (Num b)) = Num (BV.and (bitBlast b))
reduceE' (ReductionOr    (Num b)) = Num (BV.or  (bitBlast b))
--reduceE' (ReductionXor   (Num b)) = Num (BV.xor (bitBlast b))
reduceE' (ReductionOrNot (Num b)) = Num (BV.not (BV.or (bitBlast b)))

reduceE' (Zero    :&&:       _) = zero
reduceE' (_       :&&:    Zero) = zero
reduceE' (NonZero :&&: NonZero) = one

reduceE' (NonZero :||:       _) = one
reduceE' (_       :||: NonZero) = one
reduceE' (Zero    :||:     Zero) = zero

reduceE' (Num e1 :|: Num e2)  = Num ((BV..|.) e1  e2)
reduceE' (Num e1 :&: Num e2)  = Num ((BV..&.) e1  e2)

reduceE' (Num e1 :==: Num e2)  = if e1 == e2 then one else zero
reduceE' (Num e1 :!=: Num e2)  = if (BV./=.) e1  e2 then one else zero
reduceE' (Num e1 :>:  Num e2)  = if (BV.>.)  e1 e2 then one else zero
reduceE' (Num e1 :<:  Num e2)  = if (BV.<.)  e1 e2 then one else zero
reduceE' (Num e1 :>=: Num e2)  = if (BV.>=.) e1  e2 then one else zero
reduceE' (Num e1 :<=: Num e2)  = if (BV.<=.) e1  e2 then one else zero

reduceE' (e1 :==: e2)  = (reduceE e1 :==: reduceE e2)
reduceE' (e1 :!=: e2)  = (reduceE e1 :!=: reduceE e2)
reduceE' (e1 :>:  e2)  = (reduceE e1 :>:  reduceE e2)
reduceE' (e1 :<:  e2)  = (reduceE e1 :<:  reduceE e2)
reduceE' (e1 :>=: e2)  = (reduceE e1 :>=: reduceE e2)
reduceE' (e1 :<=: e2)  = (reduceE e1 :<=: reduceE e2)

reduceE' e = e

reduceE e = reapply reduceE' e
reduceS s = reapply reduceS' s
reduceM s = reapply reduceM' s


reapply f s = g $ iterate f s
  where g (x:x':xs) = if x == x' then x else g (x':xs)




