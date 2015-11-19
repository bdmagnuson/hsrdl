{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Parser (
       Expr (..)
     , Array (..)
     , Alignment (..)
     , PropRef (..)
     , PropRHS (..)
     , Identifier
     , CompType (..)
     , hsrdlParseString
     , hsrdlParseFile
     ) where

import System.IO

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Text.PrettyPrint.Leijen (Doc, (<>), (<+>))
import qualified Text.PrettyPrint.Leijen as P

type MyParser = StateT Int Parser

type Identifier = String

sc :: MyParser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: MyParser a -> MyParser a
lexeme = L.lexeme sc

symbol :: String -> MyParser String
symbol = L.symbol sc

braces :: MyParser a -> MyParser a
braces = between lbrace rbrace

lbrace = symbol "{"
rbrace = symbol "}"
semi   = symbol ";"
comma  = symbol ","
dot    = symbol "."
dref   = symbol "->"
equal  = symbol "="
dquote = symbol "\""

rword :: String -> MyParser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

identifier = do
   pos <- getPosition
   id <- lexeme (p >>= check)
   return $ id
   where p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
--         check x = if x `elem` rws
--                   then fail $ "keyword " ++ show x ++ " cannot be an identifier"
--                   else return x
         check x = return x

data CompType =
     Addrmap
   | Regfile
   | Reg
   | Field
   | Signal
   | Array

instance Show (CompType) where
   show Addrmap = "addrmap"
   show Regfile = "regfile"
   show Reg     = "reg"
   show Field   = "field"
   show Signal  = "signal"
   show Array  = "array"

data Expr =
     CompDef   {
        ctype :: CompType,
        dd    :: Maybe Identifier,
        expr  :: [Expr],
        insts :: [Expr]
     }
   | ExpCompInst    Identifier [Expr]
   | CompInst       Identifier (Maybe Array) [Alignment]
   | DefPropAssign  Identifier
   | ExpPropAssign  Identifier PropRHS
   | PostPropAssign PropRef PropRHS deriving (Show)


data PropRef = PropRef {
   path :: [(Identifier, Maybe Array)],
   prop :: Identifier
} deriving(Show)

data PropRHS =
     PropLit String
   | PropNum Integer
   | PropBool Bool
   | PropRef' PropRef deriving(Show)

data Array =
     ArrWidth { width :: Integer }
   | ArrLR { left :: Integer, right :: Integer } deriving (Show)

data Alignment =
     At Integer
   | Mod Integer
   | Stride Integer  deriving (Show)

parseExpr = parseCompDef <|> parsePropAssign <|> parseExpCompInst

parseCompDef = do
   cType <-     parseCompType "addrmap" Addrmap
            <|> parseCompType "field"   Field
            <|> parseCompType "reg"     Reg
            <|> parseCompType "regfile" Regfile
            <|> parseCompType "signal"  Signal
   name <- optional identifier
   expr <- braces $ many parseExpr
   anon <- (sepBy parseCompInst comma)
   semi
   return $ CompDef cType name expr anon

parseCompType a b = do
   try (rword a)
   return b

parseExpCompInst = do
   inst <- identifier
   elem <- sepBy parseCompInst comma
   semi
   return $ ExpCompInst inst elem

parseCompInst = do
   name <- identifier
   arr  <- optional parseArray
   return $ CompInst name arr []

parseArray = try parseArray1 <|> parseArray2
parseArray1 = do
   size <- (symbol "[") *> L.decimal <* (symbol "]")
   return $ ArrWidth {width = size}

parseArray2 = do
   symbol "["
   left <- L.decimal
   symbol ":"
   right <- L.decimal
   symbol "]"
   return $ ArrLR {left = left, right = right}

parsePropAssign =
       try parseDefPropAssign
   <|> try parseExpPropAssign
   <|> try parsePostPropAssign

parseDefPropAssign = do
   prop <- identifier
   semi
   return $ DefPropAssign prop

parseExpPropAssign = do
   lhs <- identifier
   equal
   rhs  <- parseRHS
   semi
   return $ ExpPropAssign lhs rhs

pathElem = do
   id <- identifier
   name <- optional parseArray1
   return (id, name)

parsePath = pathElem `sepBy` dot

parsePostPropAssign = do
   path <- parsePath
   prop <- dref *> identifier
   equal
   rhs <- parseRHS
   semi
   return $ PostPropAssign (PropRef path prop) rhs

parseRHS =
   parseLit <|> parseNum <|> try parseBool
   where parseLit = do
            a <- between dquote dquote (many (noneOf "\""))
            return $ PropLit a
         parseNum = do
            a <- L.decimal
            return $ PropNum a
         parseBool = do
            a <- (rword "true") *> return True <|> (rword "false") *> return False
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

hsrdlParseString s = parse (evalStateT parseExpr 0) "file" s
hsrdlParseFile file = runParser (evalStateT (sc *> parseExpr) 0) file <$> readFile file

