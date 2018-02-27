{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.SRDL.StreamParser
     ( parseStream
     ) where

import           Control.Monad
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           Control.Lens hiding (noneOf)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Pos
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Text.IO (readFile)
import qualified Data.Text as T
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import           Data.Void
import           Data.Monoid ((<>))
import           System.Environment

import           Debug.Trace

data ParseState = ParseState
  { _accept  :: [Bool]
  , _defines :: M.Map T.Text T.Text
  , _stack   :: [(SourcePos, T.Text)]
  } deriving (Eq, Show)

$(makeLenses ''ParseState)

type StreamParser = StateT ParseState (ParsecT (ErrorFancy Void) T.Text IO)

parseStream = runParserT (evalStateT parseStream' (ParseState [True] M.empty []))

parseStream' :: StreamParser T.Text
parseStream' = T.concat <$> many p
   where p = do
          b <- tick <|> try env <|> block
          end <- option False (True <$ hidden eof)
          h <- use stack
          when (end && (h /= [])) $ do
            let p = h ^?! ix 0 . _1
            setPosition p
            setInput    (marker' p <> (h ^?! ix 0 . _2))
            stack %= tail
          return b

block :: StreamParser T.Text
block = takeWhile1P Nothing (\x -> x /= '`' && x /= '$')

dquote = char '"'
lbrace = char '{'
rbrace = char '}'

env = do
   char '$'
   var <- between lbrace rbrace (many (alphaNumChar <|> char '_'))
   pos <- getPosition
   lkup <- liftIO $ lookupEnv var
   case lkup of
     Just a -> return $ T.pack a <> marker' pos
     Nothing -> return T.empty


tick :: StreamParser T.Text
tick = do
   start <- getPosition
   void (char '`')
   d <- identifier
   case d of
      "define" -> do
         name  <- identifier
         def   <- takeWhile1P Nothing (/= '\n')
         defines . at name ?= def
         return T.empty
      "include" -> do
         file <- L.lexeme sc $ between dquote dquote (many (alphaNumChar <|> char '.' <|> char '/' <|> char '_'))
         s    <- liftIO (Data.Text.IO.readFile file)
         p    <- getPosition
         i    <- getInput
         stack %= (:) (p, i)
         setPosition (initialPos file)
         setInput s
         return $ marker' (initialPos file)
      _ -> do
            foo <- use (defines . at d)
            case foo of
              Nothing -> fail ("Unknown define: " ++ (T.unpack d))
              Just span -> do
                input <- getInput
                setInput (span <> marker "define expansion" start <> input)
                setPosition start
                return T.empty

marker n pos = "##" <> T.pack (show (pos { sourceName = n })) <> "##"
marker' pos = "##" <> T.pack (show pos) <> "##"

sc :: (MonadParsec e s m, s ~ T.Text) => m ()
sc = L.space space1 lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

identifier :: StreamParser T.Text
identifier = do
  i <- L.lexeme sc $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  return $ T.pack i




















