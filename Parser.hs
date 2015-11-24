{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser (
       Expr (..)
     , hsrdlParseString
     , hsrdlParseFile
     , Alignment
     ) where

import System.IO
import Debug.Trace

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Lens hiding (noneOf)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Perm
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map.Strict as M

import Props

data ParseState = ParseState {
   _cdefCount :: Int,
   _propDefs  :: PropDefs,
   _currentComp :: CompType
}

makeLenses ''ParseState

type MyParser = StateT ParseState Parser

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
pipe   = symbol "|"
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
   | PostPropAssign ElemPath Identifier PropRHS deriving (Show)


data Alignment =
     At Integer
   | Mod Integer
   | Stride Integer deriving (Show)

parseSrdl = many (try parsePropDef >> parseExpr)

parseExpr = parseCompDef <|> parsePropAssign <|> parseExpCompInst

parseCompDef = do
   cnt   <- cdefCount %%= (\x -> (x, x + 1))
   cType <- parseCompType
   currentComp .= cType
   name  <- option ("__anon" ++ (show cnt)) identifier
   expr  <- braces $ many parseExpr
   anon  <- (sepBy parseCompInst comma)
   semi
   return $ CompDef cType (Just name) expr anon

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

checkAssign :: Identifier -> PropRHS -> MyParser ()
checkAssign prop rhs = do
   comp <- use currentComp
   def <- use $ propDefs . at comp . non M.empty . at prop
   case def of
      Nothing -> unexpected ((show prop) ++ " is not a valid property for component type " ++ (show comp))
      Just (Property ptype _) -> case checktype ptype (Just rhs) of
                                    False -> fail ("Type mismatch: Found " ++ getTypeString rhs ++ " expected " ++ (show ptype))
                                    True -> trace "success" (return ())

pathElem = do
   id <- identifier
   name <- optional parseArray1
   return (id, name)

parseDefPropAssign = do
   prop <- identifier
   checkAssign prop (PropBool True)
   semi
   return $ DefPropAssign prop

parseExpPropAssign = do
   lhs <- identifier
   equal
   rhs  <- parseRHS
   semi
   return $ ExpPropAssign lhs rhs

parsePostPropAssign = do
   path <- pathElem `sepBy` dot
   prop <- dref *> identifier
   equal
   rhs <- parseRHS
   semi
   return $ PostPropAssign path prop rhs

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

checktype :: PropType -> Maybe PropRHS -> Bool
checktype _ Nothing = True
checktype PropNumT  (Just (PropNum  _)) = True
checktype PropLitT  (Just (PropLit  _)) = True
checktype PropBoolT (Just (PropBool _)) = True
checktype PropRefT  (Just (PropRef  _ _)) = True
checktype _ _ = False

parsePropDef = do
   rword "property"
   id <- identifier
   (t, c, d) <- braces parsePropDefBody
   semi
   if checktype t d
      then propDefs %= foldl1 (.) (map (\x y -> y & ix x . at id .~ Just (Property t d)) c)
      else fail "default not of specified type"


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

parseString p s = parse (evalStateT p 0) "file" s

initState = ParseState {
   _cdefCount   = 0,
   _propDefs    = defDefs,
   _currentComp = Addrmap
}

hsrdlParseString s = parse (evalStateT parseSrdl initState) "file" s
hsrdlParseFile file = runParser (evalStateT (sc *> parseSrdl) initState) file <$> readFile file

