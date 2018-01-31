{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
       parseFile
     ) where

import GHC.IO

import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Text
import Text.Megaparsec.Perm
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Text.IO (readFile)

import qualified Data.Text as T
import Data.Text (Text, empty)
import Data.Monoid ((<>))

import Props
import Types hiding (ElabF)
import qualified SymbolTable as S

data ParseLoc =
      CHILD
    | ANON_DEF deriving (Show)

data ParseState = ParseState {
    loc      :: ParseLoc,
    nam      :: Text,
    ext      :: Maybe Bool,
    anonIdx  :: Int,
    syms     :: S.SymTab (Expr SourcePos),
    topInst  :: [Text]
} deriving (Show)

type SrdlParser = ReaderT ReaderEnv (StateT ParseState Parser)

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

lexeme = L.lexeme sc

identifier = do
  foo <- lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  return (T.pack foo)

parseIdentifier' = identifier
parseIdentifier  = do
  id <- identifier
  when (id `elem` rws) (fail (T.unpack $ "Can't use reserved word '" <> id <> "' as identifier"))
  return id

symbol = L.symbol sc

data ReaderEnv = ReaderEnv {
    scope :: [Text],
    level :: Int
}

rword :: Text -> ReaderT ReaderEnv (StateT ParseState Parser) ()
rword w = lexeme $ string (T.unpack w) *> notFollowedBy alphaNumChar *> sc

braces = between lbrace rbrace


parseTop = do
    pos <- getPosition
    e <- many parseExpr
    return $ pos :< TopExpr e

parseExpr :: SrdlParser (Expr SourcePos)
parseExpr = do
   st <- lift get
   case loc st of
      ANON_DEF -> parseCompInst <* choice [c, s]
        where
            s = do
                a <- semi
                lift (modify (\s -> s {loc = CHILD}))
                return a
            c = comma
      CHILD ->
            (try parseCompDef)
        <|> parsePropAssign
        <|> parsePropDef
        <|> parseExpCompInst

parseCompName = do
    pos <- getPosition
    parseIdentifier <|> do
      idx <- lift get
      lift (modify (\s -> s {anonIdx = anonIdx s + 1}))
      return $ "_ad" <> (T.pack . show) (anonIdx idx)

addTopDef c name = do
    env <- ask
    return ()

parseCompDef = do
   pos   <- getPosition
   ext   <- optional (parseRsvdRet "external" True <|> parseRsvdRet "internal" False)
   cType <- parseCompType
   name  <- parseCompName
   expr  <- withReaderT (\s -> s {level = level s + 1, scope = scope s ++ [name]}) $ braces $ many parseExpr
   env   <- ask
   when ((cType == Addrmap) && (level env == 0)) (lift (modify (\s -> s {topInst = topInst s ++ [name]})))
   let def = pos :< CompDef ext cType name expr
   lift (modify $ \s -> s { syms = S.add (scope env) name def (syms s)})
   _ <- try semi <|> do
                        lift (modify $ \s -> s { loc = ANON_DEF, nam = name })
                        return ""
   return def

parseCompType =
       parseRsvdRet "addrmap" Addrmap
   <|> parseRsvdRet "field"   Field
   <|> parseRsvdRet "reg"     Reg
   <|> parseRsvdRet "regfile" Regfile
   <|> parseRsvdRet "signal"  Signal

parseRsvdRet :: Text -> b -> ReaderT ReaderEnv (StateT ParseState Parser) b
parseRsvdRet a b = do
   try (rword a)
   return b


parseExpCompInst = do
   pos  <- getPosition
   ext' <- optional (parseRsvdRet "external" True <|> parseRsvdRet "internal" False)
   inst <- parseIdentifier
   lift (modify $ \s -> s { loc = ANON_DEF, nam = inst, Parser.ext = ext' })
   parseExpr

parseCompInst = do
   s     <- lift get
   env   <- ask
   pos   <- getPosition
   name  <- parseIdentifier
   arr   <- optional parseArray
   align <- parseAlign
   _     <- f $ S.lkup (syms s) (scope env) (nam s)
   return $ pos :< CompInst (Parser.ext s) (nam s) name arr align
        where f (Just ([empty], _ :< CompDef _ t n _)) = do when (t == Addrmap) (lift (modify (\s -> s {topInst = filter (/= n) (topInst s)})))
                                                            return ()
              f _ = return ()

parseArray = try parseArray1 <|> parseArray2
parseArray1 = do
   size <- symbol "[" *> L.decimal <* symbol "]"
   return ArrWidth {width = size}

parseArray2 = do
   symbol "["
   left <- L.decimal
   symbol ":"
   right <- L.decimal
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
   rhs  <- option (PropBool True) (equal *> (parseRHS prop))
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
   otherwise -> return $ Alignment at mod stride
 where
    p1 = do a <- char '@' *> parseNumeric; return (Just a)
    p2 = do a <- symbol "%=" *> parseNumeric; return (Just a)
    p3 = do a <- symbol "+=" *> parseNumeric; return  (Just a)

parsePropDefBody = makePermParser $ (,,) <$$> p1 <||> p2 <|?> (Nothing, p3)
   where
      p1 = rword "type" *> equal *> parseType <* semi
      p2 = rword "component" *> equal *> sepBy1 parseCompType pipe <* semi
      p3 = do
         rhs <- rword "default" *> equal *> (parseRHS "") <* semi
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

parseNumeric = lexeme L.decimal

type Foo a = ReaderT ReaderEnv (StateT ParseState Parser) a

parseRHS :: Text -> ReaderT ReaderEnv (StateT ParseState Parser) PropRHS
parseRHS prop = if isEnum prop then parseEnum else parseNum <|> parseBool <|> parseLit <|> parseRef
   where parseLit = do
            a <- between dquote dquote (many (Text.Megaparsec.noneOf ("\"" :: [Char])))
            return $ PropLit (T.pack a)
         parseNum = do
            a <- parseNumeric
            return $ PropNum a
         parseBool = do
            a <- rword "true" *> return True <|> rword "false" *> return False
            return $ PropBool a
         parseEnum = do
            a <- parseIdentifier'
            case a `elem` (getEnumValues prop) of
              False -> (fail . T.unpack) $ "Legal values for " <> prop <> " are " <> (T.pack . show) (getEnumValues prop)
              True -> return (PropEnum a)
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

pp = runStateT (runReaderT parseTop (ReaderEnv [""] 0)) (ParseState CHILD "" Nothing 0 M.empty [])

hsrdlParseFile file = runParser pp file <$> Data.Text.IO.readFile file

parseFile file = do
  p <- hsrdlParseFile file
  case p of
    Left err -> do
      putStrLn (parseErrorPretty err)
      return Nothing
    Right (t, s) -> return $ Just (topInst s, syms s)

