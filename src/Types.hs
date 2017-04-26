{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Types (
   ExprF(..)
 , Expr
 , Alignment
 , PathElem(..)
 , SymTab(..)
 , PropRHS(..)
 , CompType (..)
 , Array(..)
 , EnumDef(..)
 , PropType(..)
 , Property(..)
 ) where

import Control.Comonad.Cofree
import qualified Data.Map.Strict as M

type Identifier = String
type ElemPath = [(Identifier, Maybe Array)]

data SymTab a = SymTab Int (M.Map String a)

data PathElem = PathElem {
    peName  :: String,
    array :: (Maybe Array)
} deriving (Show, Eq)

data ExprF a =
     CompDef   {
        ctype  :: CompType,
        dname  :: String,
        expr   :: [a]
     }
   | CompInst {
        def    :: String,
        name   :: String,
        arr    :: Maybe Array,
        align  :: [Alignment]
     }
   | PropDef {
        name     :: String,
        propType :: PropType,
        ctypes   :: [CompType],
        value    :: Maybe PropRHS
     }
   | PropAssign {
        path  :: [PathElem],
        prop  :: String,
        rhs   :: PropRHS
     }
   | TopExpr {
        exprs :: [a]
     }
   deriving (Show, Functor, Traversable, Foldable, Eq)

type Expr a = Cofree ExprF a

data Alignment =
     At Integer
   | Mod Integer
   | Stride Integer deriving (Show, Eq)

data Array =
     ArrWidth {
       width :: Integer
     }
   | ArrLR {
       left :: Integer,
       right :: Integer
     } deriving (Show,Eq)

data CompType =
     Addrmap
   | Regfile
   | Reg
   | Field
   | Signal
   | Array deriving (Ord,Eq)

instance Show (CompType) where
   show Addrmap = "addrmap"
   show Regfile = "regfile"
   show Reg     = "reg"
   show Field   = "field"
   show Signal  = "signal"
   show Array   = "array"

data PropType =
     PropLitT
   | PropNumT
   | PropBoolT
   | PropRefT
   | PropPathT
   | PropEnumT EnumDef deriving (Show,Eq)

data PropRHS =
     PropLit  String
   | PropNum  Integer
   | PropBool Bool
   | PropRef  ElemPath Identifier
   | PropPath ElemPath
   | PropEnum EnumDef Identifier deriving (Show,Eq)

data Property = Property {
   ptype    :: PropType,
   pdefault :: Maybe PropRHS
} deriving (Show,Eq)

type PropDefs = M.Map CompType (M.Map Identifier Property)

data EnumDef = EnumDef {
   ename   :: String,
   values :: M.Map Identifier Integer
} deriving (Show,Eq)
