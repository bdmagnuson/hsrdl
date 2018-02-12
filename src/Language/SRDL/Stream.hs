{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.SRDL.Stream
     (
       Span (..)
     ) where

import Control.Monad
import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Trans.State
import Control.Lens hiding (noneOf)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Stream
import Text.Megaparsec.Pos
import qualified Data.Text as T
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Void
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set                    as E
import Data.Semigroup ((<>))

import Data.Maybe (fromJust)
import Debug.Trace

data Span = Span
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  , spanBody  :: T.Text
  } deriving (Eq, Ord, Show)

instance Stream [Span] where
  type Token  [Span] = Span
  type Tokens [Span] = [Span]
  tokenToChunk  Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength   Proxy = foldl1 (+) . map (T.length . spanBody)
  chunkEmpty    Proxy = all ((== 0) . T.length . spanBody)

  positionAt1 Proxy pos (Span start _ _) = trace ("pos1" ++ show start) start
  positionAtN Proxy pos [] = pos
  positionAtN Proxy _ (Span start _ _:_) = trace ("posN" ++ show start) start

  advance1 Proxy _ _ (Span _ end _) = end
  advanceN Proxy _ pos [] = pos
  advanceN Proxy _ _ ts = let Span _ end _ = last ts in end

  take1_ []     = Nothing
  take1_ s      = case takeN_ 1 s of
                    Nothing -> Nothing
                    Just (sp, s') -> Just (head sp, s')

  takeN_ _ [] = Nothing
  takeN_ n s@(t:ts)
    | n <= 0 = Just ([t {spanEnd = spanStart t, spanBody = ""}], s)
    | n <  (T.length . spanBody) t = let (l, r) = T.splitAt n (spanBody t)
                                         sL = spanStart t
                                         eL = foldl (defaultAdvance1 (mkPos 3)) sL (T.unpack (T.tail l))
                                         sR = defaultAdvance1 (mkPos 3) eL (T.last l)
                                         eR = spanEnd t
                                         l' = [Span sL eL l]
                                         r' = (Span sR eR r):ts
                                     in Just (trace (show n) l', r')
    | n == (T.length . spanBody) t = Just ([t], ts)
    | otherwise = case takeN_ (n - T.length (spanBody t)) ts of
                     Nothing -> Just ([t], [])
                     Just (t', ts') -> Just (t:t', ts')


  takeWhile_ p s = fromJust $ takeN_ (go 0 s) s
    where go n s = case take1_ s of
                      Nothing -> n
                      Just (c, s') -> if p c
                                      then go (n + 1) s'
                                      else n



instance ShowToken Span where
  showTokens = (T.unpack . T.concat . map spanBody . NE.toList)


defaultAdvance1 :: Enum t
  => Pos               -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> t                 -- ^ Current token
  -> SourcePos         -- ^ Incremented position
defaultAdvance1 width (SourcePos n l c) t = npos
  where
    w  = unPos width
    c' = unPos c
    npos =
      case fromEnum t of
        10 -> SourcePos n (l <> pos1) pos1
        9  -> SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w))
        _  -> SourcePos n l (c <> pos1)





















