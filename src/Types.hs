{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module Types (
   ExprF(..)
 , Expr
 , Alignment (..)
 , PathElem(..)
 , SymTab(..)
 , PropRHS(..)
 , CompType (..)
 , Array(..)
 , EnumDef(..)
 , PropType(..)
 , Property(..)
 , ElabF (..)
 , ptype
 , pdefault
 , IsSticky (..)
 , IntrType (..)
 ) where

import Control.Comonad.Cofree
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad.State
import Control.Applicative
import qualified Data.Set as Set

type Identifier = String
type ElemPath = [(Identifier, Maybe Array)]

data SymTab a = SymTab Int (M.Map String a)

data PathElem = PathElem {
    peName  :: String,
    array   :: Maybe Array
} deriving (Show, Eq)

data ExprF a =
     CompDef   {
        ctype  :: CompType,
        name   :: String,
        expr   :: [a]
     }
   | CompInst {
        def    :: String,
        name   :: String,
        arr    :: Maybe Array,
        align  :: Alignment
     }
   | PropDef {
        name     :: String,
        propType :: PropType,
        ctypes   :: [CompType],
        value    :: Maybe PropRHS
     }
   | PropDefault {
        prop  :: String,
        rhs   :: PropRHS
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

data Alignment = Alignment
  { _alignAt     :: Maybe Integer
  , _alignMod    :: Maybe Integer
  , _alignStride :: Maybe Integer
  } deriving (Show, Eq)

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

instance Show CompType where
   show Addrmap = "addrmap"
   show Regfile = "regfile"
   show Reg     = "reg"
   show Field   = "field"
   show Signal  = "signal"
   show Array   = "array"

data EnumDef = EnumDef {
   values :: M.Map Identifier Integer
} deriving (Show,Eq)

data PropType =
     PropLitT
   | PropNumT
   | PropBoolT
   | PropRefT
   | PropPathT
   | PropIntrT
   | PropEnumT deriving (Eq)

instance Show PropType where
  show PropLitT  = "String"
  show PropNumT  = "Numeric"
  show PropBoolT = "Boolean"
  show PropRefT  = "Reference"
  show PropPathT = "Path"
  show PropIntrT = "intr"
  show PropEnumT = "Enumeration"

data IsSticky = Sticky | NonSticky deriving (Show, Eq)
data IntrType = Level | Posedge | Negedge | Bothedge deriving (Show, Eq)

data PropRHS =
     PropLit  String
   | PropNum  Integer
   | PropBool Bool
   | PropRef  ElemPath Identifier
   | PropPath ElemPath
   | PropIntr IsSticky IntrType
   | PropEnum Identifier deriving (Show,Eq)

data Property = Property {
   _ptype    :: PropType,
   _pdefault :: Maybe PropRHS
} deriving (Show,Eq)

makeLenses ''Property

type PropDefs = M.Map CompType (M.Map Identifier Property)

data ElabF a = ElabF {
    _etype :: CompType,
    _name  :: String,
    _props :: M.Map String (Maybe PropRHS),
    _inst  :: [a],
    _lsb   :: Integer,
    _msb   :: Integer
} deriving (Show, Functor, Traversable, Foldable)

