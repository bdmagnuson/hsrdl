{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Check (
   ) where

import GHC.IO -- remove debugging only

import Control.Comonad
import Control.Comonad.Cofree
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import Props
import Parser
import SymbolTable as S
import Text.Megaparsec (SourcePos)

import Data.Functor.Foldable


foo file = do
   let r = unsafePerformIO (hsrdlParseFile file)
   case r of (Left e) -> error "error"
             (Right p) -> p


data NodeAnn = NodeAnn {
   nodeId :: Int,
   vars   :: M.Map String (Expr NodeAnn),
   pos    :: SourcePos
} deriving (Show)

--extendAnn :: Expr SourcePos -> Expr NodeAnn
--extendAnn = extend (\x -> NodeAnn { nodeId = 0, pos = extract x })

type TypeCheck a = State Int a

freshVarId :: TypeCheck Int
freshVarId = do
  v <- get
  modify $ \s -> s + 1
  return $ v

addAnn :: Expr SourcePos -> TypeCheck NodeAnn
addAnn a = do
   num <- freshVarId
   return $ NodeAnn { nodeId = num, vars = M.empty, pos = extract a }

modifyAnn f i t = evalState (sequence $ extend f t) i

attribute :: Expr SourcePos -> Expr NodeAnn
attribute c = evalState (sequence $ extend addAnn c) 0


--addName :: (Expr a) -> State Int (Expr a)
--addName c@(ann :< s@(CompDef _ Nothing _ _)) = do
--   cnt <- get
--   modify $ succ
--   return $ mod (\x -> x { dd = Just $ ann :< (Identifier $ "__anon" ++ (show cnt)) })
--
--walkTree :: Expr SourcePos -> MonadWriter [String] Identity
walkTree x = writer ((), (show $ extract x) ++ "\n")


typeCheck :: Expr SourcePos -> Bool
typeCheck (_ :< TopExpr e) = all typeCheck e
typeCheck (a :< PropAssign (_ :< Identifier i) rhs) = checktype (getPropType i) rhs

typeCheck _ = True

--rsvdCheck :: Expr SourcePos -> _
--rsvdCheck (_ :< CompDef _ _ e _)      = mapM rsvdCheck e
--rsvdCheck (_ :< Identifier i)         = writer("one", ["two"])
--rsvdCheck (_ :< PathElem e _)         = rsvdCheck e
--rsvdCheck (_ :< ExpCompInst n c)      = (rsvdCheck n) >>= (mapM rsvdCheck c)
--rsvdCheck (_ :< CompInst n _)         = (rsvdCheck n)
--rsvdCheck (_ :< PropDef n _ _ _ )     = rsvdCheck n
--rsvdCheck (_ :< PostPropAssign p r _) = (mapM rsvdCheck p) >>= (rsvdCheck r)
--rsvdCheck (_ :< PropAssign p _)       = rsvdCheck p
--rsvdCheck (_ :< TopExpr e)            = mapM rsvdCheck e

--validate :: Expr SourcePos -> State ParseState (Expr SourcePos)
--validate (_ <: TopExpr e) = sequence validate e


--this is hoistCofree
foome :: (Functor f) => (forall a. f a -> f a) -> Cofree f a -> Cofree f a
foome f (a :< b) = a :< f (foome f <$> b)


data EExpr a =
     EExpr {
        _ectype :: CompType,
        _ename  :: Identifier,
        _einsts  :: [a],
        _eprops  :: M.Map Identifier PropRHS
     } deriving (Show)


type Unannotated = Fix ExprF
type Annotated a = Cofree ExprF a

runUnannotated :: Functor f => (f x -> x) -> Fix f -> x
runUnannotated phi (Fix f) = phi (fmap (runUnannotated phi) f)

runAnnotated :: Functor f => (f x -> x) -> Cofree f a -> x
runAnnotated phi (_ :< f) = phi (fmap (runAnnotated phi) f)

change (Identifier _) = Identifier "foo"
change x = x

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

--
--
--type ElabState = Int
--type ElabS b = State (ElabState) b
--
--topInst :: (ExprF a) -> Identifier -> ElabS (EExpr b)
--topInst d n = foldl (>>=) (return newInst) (map inst (expr d))
--   where newInst = EExpr {
--                     _ectype = ctype d,
--                     _ename  = n,
--                     _einsts  = [],
--                     _eprops  = M.empty
--                   }
--
--inst d@(_ :< CompDef _ (Just n) _ _) e = do
--    addSymbol n d
--    return e
--
--inst d@(CompInst name Nothing align) e = do
--    def <- lookupSym def
--    inst (lookupSym a) name 
--
--elab d@(_ :< PropDef n t c v) = do
--
--
--
--
--
--
--
--
--
--
--
--
