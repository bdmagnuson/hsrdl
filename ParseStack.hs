{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Perm
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"


lexeme p = do
   l   <- L.lexeme sc p
   return $ l

rword w = string w *> notFollowedBy alphaNumChar *> sc

identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

data SymTab a = SymTab Int (M.Map String a)

data ParseLoc =
      TOP
    | CHILD
    | ANON_DEF deriving (Show)

data ParseState = ParseState {
    loc :: ParseLoc,
    nam :: String
} deriving (Show)

--type SrdlParser = StateT ParseState Parser
type SrdlParser = ReaderT (SymTab Int) (StateT ParseState Parser)



data ExprF a =
     CompDef {
        expr  :: [String]
     }
   | Identifier {
        id    :: String
     }
   | Other {
        id    :: String
     } deriving (Show, Functor)

type Expr a = Cofree ExprF a

parseExpr :: SrdlParser (Expr SourcePos)
parseExpr = do
   pos <- getPosition
   st <- lift get
   lift (modify (\s -> s { nam = "foo" }))
   case (loc st) of
      TOP -> do
        w <- identifier
        return $ pos :< (Identifier w)
      ANON_DEF -> do
        w <- identifier
        return $ pos :< (Other w)

pp = evalStateT (runReaderT parseExpr (SymTab 0 M.empty)) (ParseState ANON_DEF "")

--pp1 = runReaderT parseExpr (SymTab 0 M.empty)
--pp2 = evalStateT pp1 (ParseState TOP "")

--ppp = runReaderT parseExpr (SymTab 0 M.empty)
--ppp = execStateT  parseExpr (ParseState ANON_DEF "aa")

test p s = parse p "file" s
