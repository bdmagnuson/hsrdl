{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Language.SRDL.Types (
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
 , Implementation (..)
 , getFields
 , getRegs
 , isReg
 , isField
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
import qualified Data.HashMap.Strict as HM

import Data.Text.Prettyprint.Doc ((<>), (<+>), pretty)
import qualified Data.Text.Prettyprint.Doc as P

type Identifier = Text

data SymTab a = SymTab Int (M.Map Text a)

data PathElem = PathElem {
    peName  :: Text,
    array   :: Maybe Array
} deriving (Show, Eq)

data Implementation = Internal | External | NotSpec deriving (Show, Eq)

data ExprF a =
     CompDef   {
        ext    :: Implementation,
        ctype  :: CompType,
        name   :: Text,
        expr   :: [a],
        anon   :: [a]
     }
   | AnonCompInst {
        name   :: Text,
        arr    :: Maybe Array,
        align  :: Alignment
     }
   | ExpCompInst {
        ext    :: Implementation,
        def    :: Text,
        anon   :: [a]
     }
   | CompInst {
        ext    :: Implementation,
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
   | PropIntrT
   | PropEnumT deriving (Eq)

instance Show PropType where
  show PropLitT  = "Text"
  show PropNumT  = "Numeric"
  show PropBoolT = "Boolean"
  show PropRefT  = "Reference"
  show PropIntrT = "intr"
  show PropEnumT = "Enumeration"

data IsSticky = Sticky | StickyBit | NonSticky deriving (Show, Eq)
data IntrType = Level | Posedge | Negedge | Bothedge deriving (Show, Eq)

data PropRHS =
     PropLit  !Text
   | PropNum  !Integer
   | PropBool !Bool
   | PropRef  ![PathElem] (Maybe Identifier)
   | PropIntr !IsSticky !IntrType
   | PropEnum !Identifier deriving (Show,Eq)

instance P.Pretty PropRHS where
   pretty (PropLit a)    = pretty a
   pretty (PropNum a)    = pretty a
   pretty (PropBool a)   = pretty (if a then "1" else "0")
   pretty (PropRef a b)  = pretty "ref"
   pretty (PropIntr a b) = pretty "intr"
   pretty (PropEnum a)   = pretty "enum"

data Property = Property {
   _ptype    :: [PropType],
   _pdefault :: Maybe PropRHS
} deriving (Show,Eq)

makeLenses ''Property

type PropDefs = M.Map CompType (M.Map Identifier Property)

instance (a ~ Fix ElabF) => Eq (ReifiedTraversal a a a a) where
   (==) a b = True

instance (a ~ Fix ElabF) => Show (ReifiedTraversal a a a a) where
   show a = show ""

data ElabF a = ElabF {
    _etype      :: !CompType,
    _ename      :: !Text,
    _eprops     :: !(HM.HashMap Text (Maybe PropRHS)),
    _epostProps :: ![([PathElem], ReifiedTraversal (Fix ElabF) (Fix ElabF) (Fix ElabF) (Fix ElabF), Text, PropRHS)],
    _einst      :: ![a],
    _ealign     :: !Alignment,
    _eoffset    :: !Integer,
    _escope     :: ![Text],
    _estride    :: !Integer,
    _eext       :: !Implementation
} deriving (Functor, Foldable, Traversable)


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

isReg e   = e ^. _Fix . etype == Reg
isField e = e ^. _Fix . etype == Field

getElem :: (Fix ElabF -> Bool) -> Implementation -> Fix ElabF -> [Fix ElabF]
getElem t fe e =
    case (t e, fe == NotSpec || fe == e ^. _Fix . eext) of
       (True, True)  -> [e]
       (True, False) -> []
       (False,    _) -> concatMap (getElem t fe) (e ^. _Fix . einst)

getFields = getElem isField Internal
getRegs   = getElem isReg   Internal
