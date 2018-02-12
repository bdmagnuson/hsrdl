{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Language.SRDL.Parser (
       parseSRDL
     ) where

import Data.Proxy
import Debug.Trace
import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Perm
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Text.IO (readFile)
import Control.Monad.IO.Class

import qualified Language.SRDL.SpanLexer as L

import qualified Data.Text as T
import Data.Text (Text, empty)
import Data.Void
import Data.Monoid ((<>))
import Debug.Trace
import Data.Maybe (fromJust)

import Language.SRDL.Props
import Language.SRDL.Types hiding (ElabF)
import qualified Language.SRDL.SymbolTable as S
import Language.SRDL.StreamParser

data ParseLoc =
      CHILD
    | ANON_DEF deriving (Show)

data ParseState = ParseState {
    loc      :: ParseLoc,
    nam      :: Text,
    pext     :: Implementation,
    anonIdx  :: Int,
    syms     :: S.SymTab (Expr SourcePos),
    topInst  :: [Text],
    input    :: [Text]
} deriving (Show)

type SrdlParser = ReaderT ReaderEnv (StateT ParseState (ParsecT (ErrorFancy Void) [Span] IO))

sc = L.space (void L.spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment [(ip "//")]
        blockCmnt = L.skipBlockComment [(ip "/*")] [(ip "*/")]

ip :: Text -> Span
ip x = Span (initialPos "") (initialPos "") x

symbol x = L.lexeme sc (L.string [(ip x)])

lbrace = symbol "{"
rbrace = symbol "}"
comma  = symbol ","
equal  = symbol "="
pipe   = symbol "|"
dquote = symbol "\""
semi   = symbol ";"

dot    = L.char '.'
dref   = symbol "->"

tcat x = T.concat (map spanBody x)


identifier = do
  foo <- L.lexeme sc $ (:) <$> L.letterChar <*> many (L.alphaNumChar <|> L.char '_')
  return $ T.concat (map spanBody foo)

parseIdentifier' = identifier
parseIdentifier  = do
  id <- identifier
  when (id `elem` rws) (fail (T.unpack $ "Can't use reserved word '" <> id <> "' as identifier"))
  return id


data ReaderEnv = ReaderEnv {
    scope :: [Text],
    level :: Int
}

rword :: Text -> SrdlParser ()
rword w = symbol w *> return ()


braces = between lbrace rbrace
braces' = between lbrace rbrace


parseTop = do
    pos <- getPosition
    e   <- many parseExpr
    return $ pos :< TopExpr e

parseExpr :: SrdlParser (Expr SourcePos)
parseExpr = do
   st <- lift get
--   void $ optional (try parseInclude)
   e <- case loc st of
          ANON_DEF -> parseCompInst <* choice [c, s]
            where
                s = do
                    a <- semi
                    lift (modify (\s -> s {loc = CHILD}))
                    return a
                c = comma
          CHILD ->
                try parseCompDef
            <|> parsePropAssign
            <|> parsePropDef
            <|> parseExpCompInst
--   end <- option False (True <$ hidden eof)
--   s <- lift get
--   when (end && (input s /= [])) $ do
--                                  popPosition
--                                  setInput (head $ input s)
--                                  lift (modify (\s -> s {input = tail (input s)}))
   return e


--parseInclude = do
--   rword "`include"
--   file <- between dquote dquote (many (alphaNumChar <|> char '.' <|> char '/' <|> char '_'))
--   s <- liftIO (Data.Text.IO.readFile file)
--   p <- getPosition
--   i <- getInput
--   lift (modify (\s -> s {input = i:input s}))
--   pushPosition p
--   setPosition (initialPos file)
--   setInput s


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
   ext   <- option NotSpec (parseRsvdRet "external" External <|> parseRsvdRet "internal" Internal)
   cType <- parseCompType
   name  <- parseCompName
   expr  <- withReaderT (\s -> s {level = level s + 1, scope = scope s ++ [name]}) $ braces' (many parseExpr)
   env   <- ask
   when ((cType == Addrmap) && (level env == 0)) (lift (modify (\s -> s {topInst = topInst s ++ [name]})))
   let def = pos :< CompDef ext cType name expr
   lift (modify $ \s -> s { syms = S.add (scope env) name def (syms s)})
   _ <- try (void $ semi) <|> lift (modify $ \s -> s { loc = ANON_DEF, nam = name })
   return def

parseCompType =
       parseRsvdRet "addrmap" Addrmap
   <|> parseRsvdRet "field"   Field
   <|> parseRsvdRet "reg"     Reg
   <|> parseRsvdRet "regfile" Regfile
   <|> parseRsvdRet "signal"  Signal

parseRsvdRet :: Text -> b -> SrdlParser b
parseRsvdRet a b = do
   try (rword a)
   return b


parseExpCompInst = do
   pos  <- getPosition
   ext' <- option NotSpec (parseRsvdRet "external" External <|> parseRsvdRet "internal" Internal)
   inst <- parseIdentifier
   lift (modify $ \s -> s { loc = ANON_DEF, nam = inst, pext = ext' })
   parseExpr

parseCompInst = do
   s     <- lift get
   env   <- ask
   pos   <- getPosition
   name  <- parseIdentifier
   arr   <- optional parseArray
   align <- parseAlign
   _     <- f $ S.lkup (syms s) (scope env) (nam s)
   return $ pos :< CompInst (pext s) (nam s) name arr align
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
    p1 = do a <- L.char '@' *> parseNumeric; return (Just a)
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

parseNumeric = L.decimal


parseRHS :: Text -> SrdlParser PropRHS
parseRHS prop = if isEnum prop then parseEnum else parseNum <|> parseBool <|> parseLit <|> parseRef
   where parseLit = do
            a <- between dquote dquote (many (L.noneOf ("\"" :: String)))
            return $ PropLit (tcat a)
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

pp p = runStateT (runReaderT p (ReaderEnv [""] 0)) (ParseState CHILD "" NotSpec 0 M.empty [] [])

fromRight (Right a) = a

hsrdlParseFile file = do
   f <- Data.Text.IO.readFile file
   s <- parseStream file f
   return $ case s of
             Left err -> Left (parseErrorPretty err)
             Right p  -> Right p

hsrdlParseStream (Left e) = return $ Left e
hsrdlParseStream (Right s) = do
   r <- runParserT (pp parseTop) "" s
   return $ case r of
             Left err -> Left (parseErrorPretty err)
             Right p  -> Right p

parseSRDL file = do
    hsrdlParseFile file >>= hsrdlParseStream >>= \case
      Left err -> do
        putStrLn err
        return Nothing
      Right (t, s) -> return $ Just (topInst s, syms s)
--
