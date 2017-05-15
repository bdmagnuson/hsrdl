{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Parser (
       rws
     , SourcePos
     , hsrdlParseFile
     , t, s, ret
     ) where

import GHC.IO

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

import Props
import Types
import qualified SymbolTable as S

data ParseLoc =
      CHILD
    | ANON_DEF deriving (Show)

data ParseState = ParseState {
    loc      :: ParseLoc,
    nam      :: String,
    anonIdx :: Int,
    syms     :: S.SymTab (Expr SourcePos),
    topInst  :: [String]
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

identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

parseIdentifier = identifier

symbol = L.symbol sc

data ReaderEnv = ReaderEnv {
    scope :: [String],
    level :: Int
}

rword :: String -> ReaderT ReaderEnv (StateT ParseState Parser) ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

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
            parseCompDef
        <|> parsePropAssign
        <|> parsePropDef
        <|> parseExpCompInst

parseCompName = do
    pos <- getPosition
    parseIdentifier <|> do
      idx <- lift get
      lift (modify (\s -> s {anonIdx = anonIdx s + 1}))
      return $ "__anon_def" ++ show (anonIdx idx)

addTopDef c name = do
    env <- ask
    return ()

parseCompDef = do
   pos   <- getPosition
   cType <- parseCompType
   name  <- parseCompName
   expr  <- withReaderT (\s -> s {level = level s + 1, scope = scope s ++ [name]}) $ braces $ many parseExpr
   env   <- ask
   when ((cType == Addrmap) && (level env == 0)) (lift (modify (\s -> s {topInst = topInst s ++ [name]})))
   let def = pos :< CompDef cType name expr
   lift (modify $ \s -> s { syms = S.add (syms s) (scope env) name def})
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

parseRsvdRet :: String -> b -> ReaderT ReaderEnv (StateT ParseState Parser) b
parseRsvdRet a b = do
   try (rword a)
   return b


parseExpCompInst = do
   pos  <- getPosition
   inst <- parseIdentifier
   lift (modify $ \s -> s { loc = ANON_DEF, nam = inst })
   parseExpr

parseCompInst = do
   s    <- lift get
   env  <- ask
   pos  <- getPosition
   name <- parseIdentifier
   arr  <- optional parseArray
   _    <- f $ S.lkup (syms s) (scope env) (nam s)
   return $ pos :< CompInst (nam s) name arr []
        where f (Just ([""], _ :< CompDef t n _)) = do when (t == Addrmap) (lift (modify (\s -> s {topInst = filter (/= n) (topInst s)})))
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

parsePropAssign = try parseDefPropAssign <|> try parseExpPropAssign <|> try parsePostPropAssign

parseDefPropAssign = do
   pos <- getPosition
   prop <- parseIdentifier
   semi
   return $ pos :< PropAssign [] prop (PropBool True)

parseExpPropAssign = do
   pos <- getPosition
   prop <- parseIdentifier
   equal
   rhs  <- parseRHS
   semi
   return $ pos :< PropAssign [] prop rhs

parsePostPropAssign = do
   pos <- getPosition
   path <- pathElem `sepBy` dot
   prop <- dref *> parseIdentifier
   equal
   rhs <- parseRHS
   semi
   return $ pos :< PropAssign path prop rhs

parsePropDefBody = makePermParser $ (,,) <$$> p1 <||> p2 <|?> (Nothing, p3)
   where
      p1 = rword "type" *> equal *> parseType <* semi
      p2 = rword "component" *> equal *> sepBy1 parseCompType pipe <* semi
      p3 = do
         rhs <- rword "default" *> equal *> parseRHS <* semi
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


parseRHS =
   parseLit <|> parseNum <|> try parseBool
   where parseLit = do
            a <- between dquote dquote (many (noneOf "\""))
            return $ PropLit a
         parseNum = do
            a <- L.decimal
            return $ PropNum a
         parseBool = do
            a <- rword "true" *> return True <|> rword "false" *> return False
            return $ PropBool a

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

pp = runStateT (runReaderT parseTop (ReaderEnv [""] 0)) (ParseState CHILD "" 0 M.empty [])

hsrdlParseFile file = runParser pp file <$> readFile file

a = hsrdlParseFile "test/srdl/user_prop.srdl" 
b = unsafePerformIO a
f (Right a) = a
(t, s) = f b

ret = (topInst s, syms s)



--parseString p s = parse p "file" s
--hsrdlParseString s = parse parseSrdl "file" s
