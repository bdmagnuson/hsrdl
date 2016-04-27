{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}


import Data.Maybe (fromMaybe)
import Data.Functor.Foldable
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Comonad
import Control.Comonad.Cofree


import Control.Lens hiding (noneOf, (:<))
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Perm
import qualified Text.Megaparsec.Lexer as L

data ParseState = ParseState {
   _cdefCount :: Int
}

makeLenses ''ParseState

type MyParser = StateT ParseState Parser

sc :: MyParser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

symbol = L.symbol sc
braces = between lbrace rbrace
lbrace = symbol "{"
rbrace = symbol "}"

lexeme p = do
   pos <- getPosition
   l   <- L.lexeme sc p
   return $ pos :< Ident l

identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

parseExpr = parseBracket

parseBracket = do
   pos  <- getPosition
   name <- identifier
   next <- braces (many parseExpr)
   return $ pos :< Bracket name next

data ExprF a =
     Brace   a [a]
   | Bracket a [a]
   | Ident   String
   deriving (Show, Functor)


type Expr = Cofree ExprF SourcePos

initState = ParseState 1

parseString p s = parse (evalStateT p initState) "file" s

fromRight (Left _)  = error "Either.Unwrap.fromRight: Argument takes form 'Left _'" -- yuck
fromRight (Right x) = x

replace :: Cofree ExprF SourcePos -> Int
replace _ = 4

unann :: Expr -> Fix ExprF
unann x = f (unwrap x)
    where
        f (Brace a b)   = Fix $ Brace   (unann a) (map unann b)
        f (Bracket a b) = Fix $ Bracket (unann a) (map unann b)
        f (Ident a)     = Fix $ Ident a



changeIdent (Ident _) = Ident "new"
changeIdent x = x




