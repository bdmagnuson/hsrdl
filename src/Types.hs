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
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

type Identifier = Text
type ElemPath = [(Identifier, Maybe Array)]

data SymTab a = SymTab Int (M.Map Text a)

data PathElem = PathElem {
    peName  :: Text,
    array   :: Maybe Array
} deriving (Show, Eq)

data ExprF a =
     CompDef   {
        ext    :: Maybe Bool,
        ctype  :: CompType,
        name   :: Text,
        expr   :: [a]
     }
   | CompInst {
        ext    :: Maybe Bool,
        def    :: Text,
        name   :: Text,
        arr    :: Maybe Array,
        align  :: Alignment
     }
   | PropDef {
        name     :: Text,
        propType :: PropType,
        ctypes   :: [CompType],
        value    :: Maybe PropRHS
     }
   | PropDefault {
        prop  :: Text,
        rhs   :: PropRHS
     }
   | PropAssign {
        path  :: [PathElem],
        prop  :: Text,
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
  show PropLitT  = "Text"
  show PropNumT  = "Numeric"
  show PropBoolT = "Boolean"
  show PropRefT  = "Reference"
  show PropPathT = "Path"
  show PropIntrT = "intr"
  show PropEnumT = "Enumeration"

data IsSticky = Sticky | NonSticky deriving (Show, Eq)
data IntrType = NonIntr | Level | Posedge | Negedge | Bothedge deriving (Show, Eq)

data PropRHS =
     PropLit  Text
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
    _name  :: Text,
    _props :: M.Map Text (Maybe PropRHS),
    _propRef :: S.Set Text,
    _ext   :: Bool,
    _inst  :: [a],
    _lsb   :: Integer,
    _msb   :: Integer
} deriving (Show, Functor, Traversable, Foldable)



