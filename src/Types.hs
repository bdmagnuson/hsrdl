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
 , AccessType (..)
 , AccessBehavior (..)
 , _Fix
 , etype
 , ename
 , eprops
 , epostProps
 , einst
 , ealign
 , eoffset
 , escope
 , estride
 , eext
 , _PropLit
 , _PropNum
 , _PropBool
 , _PropRef
 , _PropPath
 , _PropIntr
 , _PropEnum
 ) where

import Control.Comonad.Cofree
import qualified Data.Map.Strict as M
import Control.Lens
import Control.Monad.State
import Control.Applicative
import Data.Functor.Foldable
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)

import Text.Show.Deriving
import Data.Eq.Deriving

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
   deriving (Show, Functor,  Eq)

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

data IsSticky = Sticky | StickyBit | NonSticky deriving (Show, Eq)
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
    _etype      :: CompType,
    _ename      :: Text,
    _eprops     :: M.Map Text (Maybe PropRHS),
    _epostProps :: [([PathElem], Text, PropRHS)],
    _einst      :: [a],
    _ealign     :: Alignment,
    _eoffset    :: Integer,
    _escope     :: [Text],
    _estride    :: Integer,
    _eext       :: Bool
} deriving (Show, Functor)

$(makePrisms ''Fix)
$(makePrisms ''PropRHS)
$(makeLenses ''ElabF)
$(deriveShow1 ''ElabF)
$(deriveEq1 ''ElabF)

data AccessType
  = RO     --W: no effect, R: no effect
  | RW     --W: as-is, R: no effect
  | RC     --W: no effect, R: clears all bits
  | RS     --W: no effect, R: sets all bits
  | WRC    --W: as-is, R: clears all bits
  | WRS    --W: as-is, R: sets all bits
  | WS     --W: clears all bits, R: no effect
  | WC     --W: sets all bits, R: no effect
  | WSRC   --W: sets all bits, R: clears all bits
  | WCRS   --W: clears all bits, R: sets all bits
  | W1C    --W: 1/0 clears/no effect on matching bit, R: no effect
  | W1S    --W: 1/0 sets/no effect on matching bit, R: no effect
  | W0C    --W: 1/0 no effect on/clears matching bit, R: no effect
  | W0S    --W: 1/0 no effect on/sets matching bit, R: no effect
  | W1SRC  --W: 1/0 sets/no effect on matching bit, R: clears all bits
  | W1CRS  --W: 1/0 clears/no effect on matching bit, R: sets all bits
  | W0SRC  --W: 1/0 no effect on/sets matching bit, R: clears all bits
  | W0CRS  --W: 1/0 no effect on/clears matching bit, R: sets all bits
  | WO     --W: as-is, R: error
  | WOC    --W: clears all bits, R: error
  | WOS deriving (Show, Eq)    --W: sets all bits, R: error

data AccessBehavior
 = Disallowed
 | Normal
 | Set
 | Clear
 | OneSet
 | OneClear
 | ZeroSet
 | ZeroClear deriving (Show, Eq)

