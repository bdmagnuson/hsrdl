{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.SRDL.StreamParser
     ( parseStream
     , module Language.SRDL.Stream
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

import           Debug.Trace

import Language.SRDL.Stream


data ParseState = ParseState
  { _accept  :: [Bool]
  , _defines :: M.Map T.Text Span
  , _stack   :: [(SourcePos, T.Text)]
  } deriving (Eq, Show)

$(makeLenses ''ParseState)

type StreamParser = StateT ParseState (ParsecT (ErrorFancy Void) T.Text IO)

parseStream = runParserT (evalStateT parseStream' (ParseState [True] M.empty []))

parseStream' :: StreamParser [Span]
parseStream' = concat <$> many p
   where p = do
          b <- tick <|> block
          end <- option False (True <$ hidden eof)
          h <- use stack
          when (end && (h /= [])) $ do
            --setPosition (h ^?! ix 0 . _1)
            popPosition
            setInput    (h ^?! ix 0 . _2)
            stack %= tail
          return b

block :: StreamParser [Span]
block = do
   st   <- getPosition
   file <- takeWhile1P Nothing (/= '`')
   ff   <- getInput
   end  <- getPosition
   return $ [Span st end file]

dquote = char '"'

tick :: StreamParser [Span]
tick = do
   void (char '`')
   d <- identifier
   case d of
      "define" -> do
         name  <- identifier
         start <- getPosition
         def   <- takeWhile1P Nothing (/= '\n')
         end   <- getPosition
         defines . at name ?= Span start end (def <> " ")
      "include" -> do
         file <- between dquote dquote (many (alphaNumChar <|> char '.' <|> char '/' <|> char '_'))
         s    <- liftIO (Data.Text.IO.readFile file)
         p    <- getPosition
         i    <- getInput
         pushPosition p
         stack %= (:) (p, i)
         setPosition (initialPos file)
         setInput s
      _ -> do
            foo <- use (defines . at d)
            case foo of
              Nothing -> fail ("Unknown define: " ++ (T.unpack d))
              Just span -> do
                pos <- getPosition
                input <- getInput
                stack %= (:) (pos, input)
                pushPosition pos
                setPosition (spanStart span)
                setInput (spanBody span)
   return []

sc :: (MonadParsec e s m, s ~ T.Text) => m ()
sc = L.space space1 lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

identifier :: StreamParser T.Text
identifier = do
  i <- L.lexeme sc $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  return $ T.pack i




















