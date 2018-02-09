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

--sc = skipMany $ (hidden (void spaceChar))
--
--symbol' :: forall e s m. (MonadParsec e s m, Token s ~ Span, Tokens s ~ [Span])
--  => m ()              -- ^ How to consume white space after lexeme
--  -> Tokens s          -- ^ Symbol to parse
--  -> m (Tokens s)
--symbol' spc = L.lexeme spc . string
--
--symbol x = symbol' sc [(Span (initialPos "") (initialPos "") x)]
--
--lexeme = L.lexeme sc
--
--string :: forall e s m. (MonadParsec e s m, Token s ~ Span, Tokens s ~ [Span]) => Tokens s -> m (Tokens s)
--string = tokens f
--  where f x y = let xt = chunkToTokens (Proxy :: Proxy s) x
--                    yt = chunkToTokens (Proxy :: Proxy s) y
--                    result = and (zipWith (==) (trace (show xt) xt) (trace (show yt) yt))
--                in trace (show result) result

--spaceChar = satisfy ((T.foldl (flip $ (&&) . isSpace) True) . spanBody)  <?> "white space"
--

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

string :: (MonadParsec e s m, s ~ [Span]) => Tokens s -> m (Tokens s)
string = tokens xEqual

bodyEqual x y = (spanBody x == spanBody y)

xEqual :: (s ~ [Span]) => Tokens s -> Tokens s -> Bool
xEqual x y = (T.concat (map spanBody x)) == (T.concat (map spanBody y))


space :: MonadParsec e s m
  => m () -- ^ A parser for space characters which does not accept empty
          -- input (e.g. 'C.space1')
  -> m () -- ^ A parser for a line comment (e.g. 'skipLineComment')
  -> m () -- ^ A parser for a block comment (e.g. 'skipBlockComment')
  -> m ()
space sp line block = M.skipMany $ M.choice
  [M.hidden sp, M.hidden line, M.hidden block]

-- | This is a wrapper for lexemes. Typical usage is to supply the first
-- argument (parser that consumes white space, probably defined via 'space')
-- and use the resulting function to wrap parsers for every lexeme.
--
-- > lexeme  = L.lexeme spaceConsumer
-- > integer = lexeme L.decimal
--
lexeme :: (MonadParsec e s m) => m () -> m a -> m a 
lexeme spc p = p <* spc

--symbol :: (MonadParsec e s m, Token s ~ Span)
--  => m ()              -- ^ How to consume white space after lexeme
--  -> Text
--  -> m (Token s)
--symbol spc s = lexeme spc (string s)

ip :: Text -> Span
ip x = Span (initialPos "") (initialPos "") x

fff :: (Stream s, s ~ [Span]) => s -> Maybe Span
fff = parseMaybe (letterChar)

parseMaybe :: (Stream s, s ~ [Span]) => Parsec (M.ErrorFancy Void) s a -> s -> Maybe a
parseMaybe = M.parseMaybe


decimal
  :: forall e s m a. (MonadParsec e s m, s ~ [Span])
  => m Integer
decimal = decimal_ M.<?> "integer"
{-# INLINEABLE decimal #-}

decimal_
  :: forall e s m a. (MonadParsec e s m, s ~ [Span])
  => m Integer
decimal_ = mkNum <$> M.takeWhile1P (Just "digit") (isDigit . T.head . spanBody)
  where
    mkNum :: Tokens s -> Integer
    mkNum x = (read . T.unpack) (T.concat (map spanBody x))


--foo :: (MonadParsec e s m, Token s ~ Span) => m (Token s)
--foo = string "hi"
--
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
