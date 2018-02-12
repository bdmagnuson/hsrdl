{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.SRDL.SpanLexer
     (
       spaceChar
     , lexeme
     , letterChar
     , char
     , digitChar
     , alphaNumChar
     , noneOf
     , space
     , skipLineComment
     , skipBlockComment
     , decimal
     , string
     ) where

import Control.Monad
import qualified Text.Megaparsec as M hiding (string, spaceChar)
import Text.Megaparsec (Token, MonadParsec, Stream, Parsec, Tokens, initialPos, token, tokens)
import qualified Data.Text as T
import Data.Text (Text, empty)
import Data.Void
import Data.Char
import Data.Proxy
import qualified Data.Set as E
import Data.List.NonEmpty (NonEmpty(..))
import Debug.Trace

import Language.SRDL.Stream


spanPred p = (T.foldl (flip $ (&&) . p) True) . spanBody

spaceChar :: (MonadParsec e s m, s ~ [Span]) => m (Token s)
spaceChar = satisfy (spanPred isSpace)

digitChar :: (MonadParsec e s m, s ~ [Span]) => m (Token s)
digitChar = satisfy (spanPred isDigit)

letterChar :: (MonadParsec e s m, s ~ [Span]) => m (Token s)
letterChar = satisfy (spanPred isLetter)

alphaNumChar :: (MonadParsec e s m, s ~ [Span]) => m (Token s)
alphaNumChar = satisfy (spanPred isAlphaNum) M.<?> "alphanumeric character"

noneOf :: (Foldable f, MonadParsec e s m, s ~ [Span]) => f (Char) -> m (Token s)
noneOf cs = satisfy $ spanPred (`notElem` cs)


satisfy :: MonadParsec e s m
  => (Token s -> Bool) -- ^ Predicate to apply
  -> m (Token s)
satisfy f = token testChar Nothing
  where
    testChar x =
      if f x
        then Right x
        else Left (pure (M.Tokens (x:|[])), E.empty)

char :: (MonadParsec e s m, Token s ~ Span) => Char -> m (Token s)
char c = satisfy $ bodyEqual (Span (initialPos "") (initialPos "") (T.singleton c))


bodyEqual x y = (spanBody x == spanBody y)

xEqual :: (s ~ [Span]) => Tokens s -> Tokens s -> Bool
xEqual x y = (T.concat (map spanBody x)) == (T.concat (map spanBody y))

string :: (MonadParsec e s m, s ~ [Span]) => Tokens s -> m (Tokens s)
string = tokens xEqual


space :: MonadParsec e s m
  => m () -- ^ A parser for space characters which does not accept empty
          -- input (e.g. 'C.space1')
  -> m () -- ^ A parser for a line comment (e.g. 'skipLineComment')
  -> m () -- ^ A parser for a block comment (e.g. 'skipBlockComment')
  -> m ()
space sp line block = M.skipMany $ M.choice
  [M.hidden sp, M.hidden line, M.hidden block]

lexeme :: (MonadParsec e s m) => m () -> m a -> m a 
lexeme spc p = p <* spc

ip :: Text -> Span
ip x = Span (initialPos "") (initialPos "") x

parseMaybe :: (Stream s, s ~ [Span]) => Parsec (M.ErrorFancy Void) s a -> s -> Maybe a
parseMaybe = M.parseMaybe


decimal
  :: forall e s m a. (MonadParsec e s m, s ~ [Span])
  => m Integer
decimal = decimal_ M.<?> "integer"

decimal_
  :: forall e s m a. (MonadParsec e s m, s ~ [Span])
  => m Integer
decimal_ = mkNum <$> M.takeWhile1P (Just "digit") (isDigit . T.head . spanBody)
  where
    mkNum :: Tokens s -> Integer
    mkNum x = (read . T.unpack) (T.concat (map spanBody x))


sp :: (MonadParsec e s m, s ~ [Span]) => m ()
sp = void (M.many spaceChar)


skipLineComment :: (MonadParsec e s m, s ~ [Span])
  => Tokens s
  -> m ()
skipLineComment prefix =
  string prefix *> void (M.takeWhileP (Just "character") (\x -> (T.head . spanBody) x /= '\n'))
--
skipBlockComment :: (MonadParsec e s m, s ~ [Span])
  => Tokens s          -- ^ Start of block comment
  -> Tokens s          -- ^ End of block comment
  -> m ()
skipBlockComment start end = p >> void (M.manyTill (satisfy (const True))  n)
  where
    p = string start
    n = string end

