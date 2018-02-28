{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.SRDL.Parser (
       parseSRDL
     ) where

import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Perm
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Text.IO (readFile)
import Control.Monad.IO.Class

import qualified Data.Text as T
import Data.Text (Text, empty)
import Data.Monoid ((<>))
import Debug.Trace
import Data.Void

import Language.SRDL.Props
import Language.SRDL.Types hiding (ElabF)
import Language.SRDL.StreamParser

data ParseState = ParseState {
    pext     :: Implementation,
    anonIdx  :: Int
} deriving (Show)


type SrdlParser = StateT ParseState (ParsecT (ErrorFancy Void) Text IO)

sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lbrace = symbol "{"
rbrace = symbol "}"
semi   = symbol ";"
comma  = symbol ","
dot    = symbol "."
dref   = symbol "->"
equal  = symbol "="
pipe   = symbol "|"
dquote = symbol "\""

lexeme = marker . L.lexeme sc
symbol = marker . L.symbol sc

rword' :: Text -> SrdlParser ()
rword' w = lexeme $ string w *> notFollowedBy alphaNumChar *> sc

rword w = marker (rword' w)

braces = between lbrace rbrace

marker :: SrdlParser a -> SrdlParser a
marker p = hidden (optional (many (L.lexeme sc m))) >> p
  where m :: SrdlParser ()
        m = do
             string "##"
             pos <- takeWhile1P Nothing (/= '#')
             string "##"
             setPosition (read (T.unpack pos))
             return ()

identifier' = do
  foo <- lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  return (T.pack foo)

identifier = marker identifier'

parseIdentifier' = identifier
parseIdentifier  = do
  id <- identifier
  when (id `elem` rws) (fail (T.unpack $ "Can't use reserved word '" <> id <> "' as identifier"))
  return id

parseTop = do
    pos <- getPosition
    e <- many parseExpr
    return $ pos :< (TopExpr (concatMap flatten e))

parseExpr' :: SrdlParser (Expr SourcePos)
parseExpr' = do
      parseCompDef
  <|> try parsePropDef
  <|> parsePropAssign
  <|> parseExpCompInst

parseExpr     = marker parseExpr'
parseCompName = marker parseCompName'

parseCompName' = do
    pos <- getPosition
    parseIdentifier <|> do
      idx <- get
      modify (\s -> s {anonIdx = anonIdx s + 1})
      return $ "_ad" <> (T.pack . show) (anonIdx idx)


parseCompDef = do
   pos   <- getPosition
   ext   <- option NotSpec (parseRsvdRet "external" External <|> parseRsvdRet "internal" Internal)
   cType <- parseCompType
   name  <- parseCompName
   expr  <- braces $ many parseExpr
   anon  <- parseCompInst `sepBy` comma
   semi
   return $ pos :< CompDef ext cType name expr anon


parseCompType =
       parseRsvdRet "addrmap" Addrmap
   <|> parseRsvdRet "field"   Field
   <|> parseRsvdRet "regfile" Regfile
   <|> parseRsvdRet "reg"     Reg
   <|> parseRsvdRet "signal"  Signal


parseRsvdRet :: Text -> b -> SrdlParser b
parseRsvdRet a b = do
   try (rword a)
   return b

--parseRsvdRet a b = marker (parseRsvdRet' a b)

parseExpCompInst' = do
   pos  <- getPosition
   ext  <- option NotSpec (parseRsvdRet "external" External <|> parseRsvdRet "internal" Internal)
   inst <- parseIdentifier
   anon <- many parseCompInst
   semi
   return $ pos :< ExpCompInst ext inst anon

parseCompInst' = do
   pos   <- getPosition
   name  <- parseIdentifier
   arr   <- optional parseArray
   align <- parseAlign
   return $ pos :< AnonCompInst name arr align

parseExpCompInst = marker parseExpCompInst'
parseCompInst = marker parseCompInst'

parseArray = try parseArray1 <|> parseArray2
parseArray1 = do
   size <- symbol "[" *> parseNumeric <* symbol "]"
   return ArrWidth {width = size}

parseArray2 = do
   symbol "["
   left <- parseNumeric
   symbol ":"
   right <- parseNumeric
   symbol "]"
   return ArrLR {left = left, right = right}

pathElem = do
   id  <- parseIdentifier
   arr <- optional parseArray1
   return $ PathElem id arr

parsePropAssign =
      try parseIntr
  <|> try parsePropAssign'
  <|> try parsePostPropAssign

parseIntr = do
  pos <- getPosition
  d <- optional (join parseRsvdRet "default")
  s <- option Sticky (parseRsvdRet "nonsticky" NonSticky)
  t <- option Level (    parseRsvdRet "level" Level
                     <|> parseRsvdRet "bothedge" Bothedge
                     <|> parseRsvdRet "posedge" Posedge
                     <|> parseRsvdRet "negedge" Negedge)
  rword "intr"
  semi
  case d of
    Nothing -> return $ pos :< PropAssign [] "intr" (PropIntr s t)
    Just _ -> return $ pos :< PropDefault "intr" (PropIntr s t)


parsePropAssign' = do
   pos  <- getPosition
   d    <- optional (join parseRsvdRet "default")
   prop <- parseIdentifier'
   rhs  <- option (PropBool True) (equal *> parseRHS prop)
   semi
   case d of
     Nothing -> return $ pos :< PropAssign [] prop rhs
     Just _  -> return $ pos :< PropDefault prop rhs

parsePostPropRef = do
   path <- pathElem `sepBy` dot
   prop <- dref *> parseIdentifier'
   return (path, prop)

parsePostPropAssign = do
   pos <- getPosition
   (path, prop) <- parsePostPropRef
   equal
   rhs <- parseRHS prop
   semi
   return $ pos :< PropAssign path prop rhs

parseAlign = do
 (at, mod, stride) <- makePermParser $ (,,) <$?> (Nothing, p1) <|?> (Nothing, p2) <|?> (Nothing, p3)
 case (at, mod, stride) of
   (Just _, Just _, _) -> fail "@ and %= operators are mutualy exclusive"
   _ -> return $ Alignment at mod stride
 where
    p1 = do a <- char '@' *> parseNumeric; return (Just a)
    p2 = do a <- symbol "%=" *> parseNumeric; return (Just a)
    p3 = do a <- symbol "+=" *> parseNumeric; return  (Just a)

parsePropDefBody = makePermParser $ (,,) <$$> p1 <||> p2 <|?> (Nothing, p3)
   where
      p1 = rword "type" *> equal *> parseType <* semi
      p2 = rword "component" *> equal *> sepBy1 parseCompType pipe <* semi
      p3 = do
         rhs <- rword "default" *> equal *> parseRHS "" <* semi
         return $ Just rhs
      parseType =
           parseRsvdRet "string"  PropLitT
       <|> parseRsvdRet "number"  PropNumT
       <|> parseRsvdRet "boolean" PropBoolT
       <|> parseRsvdRet "ref"     PropRefT

parsePropDef = do
   pos <- getPosition
   rword "property"
   id <- parseIdentifier
   (t, c, d) <- braces parsePropDefBody
   semi
   return $ pos :< PropDef id t c d

parseNumeric = marker (lexeme parseNumeric')
parseNumeric' =
      try (char '0' >> char' 'x' >> L.hexadecimal)
  <|> L.decimal


parseRHS :: Text -> SrdlParser PropRHS
parseRHS prop = if isEnum prop then parseEnum else parseNum <|> parseBool <|> parseLit <|> parseRef
   where parseLit = do
            a <- between dquote dquote (many (noneOf ("\"" :: String)))
            return $ PropLit (T.pack a)
         parseNum = do
            a <- parseNumeric
            return $ PropNum a
         parseBool = do
            a <- rword "true" *> return True <|> rword "false" *> return False
            return $ PropBool a
         parseEnum = do
            a <- parseIdentifier'
            if a `elem` getEnumValues prop
            then return (PropEnum a)
            else (fail . T.unpack) $ "Legal values for " <> prop <> " are " <> (T.pack . show) (getEnumValues prop)
         parseRef = do
            path <- pathElem `sepBy` dot
            prop <- optional $ dref *> parseIdentifier'
            return $ PropRef path prop

rws = [ "accesswidth", "activehigh", "activelow", "addressing", "addrmap",
        "alias", "alignment", "all", "anded", "arbiter", "async", "bigendian",
        "bothedge", "bridge", "clock", "compact", "counter", "cpuif_reset",
        "decr", "decrsaturate", "decrthreshold", "decrvalue", "decrwidth",
        "default", "desc", "dontcompare", "donttest", "enable", "encode", "enum",
        "errextbus", "external", "false", "field", "field_reset", "fieldwidth",
        "fullalign", "halt", "haltenable", "haltmask", "hw", "hwclr", "hwenable",
        "hwmask", "hwset", "incr", "incrvalue", "incrwidth", "internal", "intr",
        "level", "littleendian", "lsb0", "mask", "msb0", "na", "name",
        "negedge", "next", "nonsticky", "ored", "overflow", "posedge",
        "precedence", "property", "r", "rclr", "reg", "regalign", "regfile",
        "regwidth", "reset", "resetsignal", "rset", "rsvdset", "rsvdsetX", "rw",
        "saturate", "shared", "sharedextbus", "signal", "signalwidth",
        "singlepulse", "sticky", "stickybit", "sw", "swacc", "swmod", "swwe",
        "swwel", "sync", "threshold", "true", "underflow", "w", "we", "wel",
        "woclr", "woset", "wr", "xored", "then", "else", "while", "do", "skip",
        "true", "false", "not", "and", "or" ]

flatten (p :< CompDef ext c n e a) = (p :< CompDef ext c n (concatMap flatten e) []):(map f a)
   where f (p :< AnonCompInst name arr align) = (p :< CompInst NotSpec n name arr align)

flatten (p :< ExpCompInst e n a) = map f a
   where f (p :< AnonCompInst name arr align) = (p :< CompInst e n name arr align)

flatten x = [x]

pp x = runStateT x (ParseState NotSpec 0)

hsrdlParseFile file = do
  f <- Data.Text.IO.readFile file
  s <- parseStream file f
  return $ case s of
             Left err -> Left (parseErrorPretty err)
             Right p  -> Right p

hsrdlParseStream f (Left e) = return $ Left e
hsrdlParseStream f (Right s) = do
  r <- runParserT (pp parseTop) f s
  return $ case r of
             Left err -> Left (parseErrorPretty err)
             Right p  -> Right p

parseSRDL file = do
  hsrdlParseFile file >>= hsrdlParseStream file >>= \case
    Left err -> do
      putStrLn err
      return Nothing
    Right (t, s) -> return $ Just t

