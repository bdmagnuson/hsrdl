{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Parser (
       ExprF (..)
     , Expr
     , hsrdlParseString
     , hsrdlParseFile
     , Alignment
     , rws
     ) where

import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Perm
import qualified Text.Megaparsec.Lexer as L

import Props

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

lexeme p = do
   l   <- L.lexeme sc p
   return $ l

identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

parseIdentifier = do
    pos <- getPosition
    id  <- identifier
    return $ pos :< (Identifier id)

symbol = L.symbol sc
rword w = string w *> notFollowedBy alphaNumChar *> sc

braces = between lbrace rbrace

data ExprF a =
     CompDef   {
        ctype :: CompType,
        dd    :: Maybe a,
        expr  :: [a],
        insts :: [a]
     }
   | Identifier {
        id    :: String
     }
   | PathElem {
        name  :: a,
        arr   :: Maybe Array
     }
   | ExpCompInst {
        name  :: a,
        comp  :: [a]
     }
   | CompInst {
        compp :: a,
        arr   :: Maybe Array,
        align :: [Alignment]
     }
   | PropDef {
        name     :: a,
        propType :: PropType,
        ctypes   :: [CompType],
        value    :: Maybe PropRHS
     }
   | PostPropAssign {
        path  :: [a],
        prop  :: a,
        rhs   :: PropRHS
     }
   | PropAssign {
        prop  :: a,
        rhs   :: PropRHS
     }
   | TopExpr {
        exprs :: [a]
     }
   deriving (Show, Functor, Traversable, Foldable)

type Expr a = Cofree ExprF a

data Alignment =
     At Integer
   | Mod Integer
   | Stride Integer deriving (Show)

parseTop = do
    pos <- getPosition
    expr <- many parseTopExpr
    return $ pos :< TopExpr expr

parseTopExpr =
       parseCompDef
   <|> parsePropAssign
   <|> parsePropDef
   <|> parseExpCompInst

parseExpr =
       parseCompDef
   <|> parsePropAssign
   <|> parseExpCompInst

parseCompDef = do
   pos   <- getPosition
   cType <- parseCompType
   name  <- optional parseIdentifier
   expr  <- braces $ many parseExpr
   anon  <- (sepBy parseCompInst comma)
   semi
   return $ pos :< CompDef cType name expr anon

parseCompType =
       parseRsvdRet "addrmap" Addrmap
   <|> parseRsvdRet "field"   Field
   <|> parseRsvdRet "reg"     Reg
   <|> parseRsvdRet "regfile" Regfile
   <|> parseRsvdRet "signal"  Signal

parseRsvdRet a b = do
   try (rword a)
   return b

parseExpCompInst = do
   pos  <- getPosition
   inst <- parseIdentifier
   elem <- sepBy parseCompInst comma
   semi
   return $ pos :< ExpCompInst inst elem

parseCompInst = do
   pos  <- getPosition
   name <- pathElem
   arr  <- optional parseArray
   return $ pos :< CompInst name arr []

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

pathElem = do
   pos <- getPosition
   id  <- parseIdentifier
   arr <- optional parseArray1
   return $ pos :< PathElem id arr

parsePropAssign = try parseDefPropAssign <|> try parseExpPropAssign <|> try parsePostPropAssign

parseDefPropAssign = do
   pos <- getPosition
   prop <- parseIdentifier
   semi
   return $ pos :< PropAssign prop (PropBool True)

parseExpPropAssign = do
   pos <- getPosition
   prop <- parseIdentifier
   equal
   rhs  <- parseRHS
   semi
   return $ pos :< PropAssign prop rhs

parsePostPropAssign = do
   pos <- getPosition
   path <- pathElem `sepBy` dot
   prop <- dref *> parseIdentifier
   equal
   rhs <- parseRHS
   semi
   return $ pos :< PostPropAssign path prop rhs

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

--   if checktype t d
--      then propDefs %= foldl1 (.) (map (\x y -> y & ix x . at id .~ Just (Property t d)) c)
--      else fail "default not of specified type"


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


parseSrdl = parseTop
parseString p s = parse p "file" s

hsrdlParseString s = parse parseSrdl "file" s
hsrdlParseFile file = runParser (sc *> parseSrdl) file <$> readFile file

