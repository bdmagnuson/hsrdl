{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
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
 , ptype
 , pdefault
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
    array :: (Maybe Array)
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


--collapseMaybe :: [Maybe a] -> Maybe [a]
--collapseMaybe [] = Just []
--collapseMaybe (x:xs) = (:) <$> x <*> (collapseMaybe xs)
--
--assignBits x = (msb - 1, traverse id g)
--    where (g, (msb, _)) = runState (sequence (map f x)) (0, Set.empty)
--          f :: Array -> State (Integer, Set.Set Integer) (Either String Array)
--          f a@(ArrLR l r) = do
--            (_, used) <- get
--            let set          = Set.fromList [r..l]
--            let intersection = Set.intersection used set
--            let union        = Set.union used set
--            if null intersection
--                then do
--                    put (l + 1, union)
--                    return $ Right a
--                else return (Left ("Field overlap on bits " ++ (show . Set.toList) intersection))
--          f (ArrWidth w) = do
--            (lsb, used) <- get
--            put (lsb + w, Set.union used (Set.fromList [lsb..(lsb + w - 1)]))
--            return $ Right $ ArrLR {left = lsb + w - 1, right = lsb}

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

data EnumDef = EnumDef {
   ename   :: String,
   values :: M.Map Identifier Integer
} deriving (Show,Eq)

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
   _ptype    :: PropType,
   _pdefault :: Maybe PropRHS
} deriving (Show,Eq)

makeLenses ''Property

type PropDefs = M.Map CompType (M.Map Identifier Property)

