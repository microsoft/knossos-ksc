module Parse  where

import Lang

import Text.Parsec( (<|>), try, many, parse, ParseError )
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad
import qualified Text.PrettyPrint as PP

---------------------
testParse :: Pretty a => Parser a -> String -> IO ()
testParse  p s = case runParser p s of
                   Left err -> putStrLn ("Failed: " ++ show err)
                   Right r  -> putStrLn (PP.render (ppr r))

runParser :: Parser a -> String -> Either ParseError a
runParser p s = parse p "" s

------- Parser -------
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
--  , Tok.identLetter     = alphaNum <|> oneOf "_'"
--  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
--  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.identStart      = letter <|> oneOf "_':!#$%&*+./<=>?@\\^|-~"
  , Tok.identLetter     = alphaNum <|> oneOf "_':!#$%&*+./<=>?@\\^|-~"
  , Tok.opStart         = mzero
  , Tok.opLetter        = mzero
  , Tok.reservedNames   = [ "def", "let", "if", "assert", "call" ]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

integer :: Parser Integer
integer = Tok.integer lexer

comma :: Parser ()
comma = char ',' >> return ()

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

expr :: Parser Expr
expr =     try iteExpr
       <|> try letExpr
       <|> try lamExpr
       <|> try assertExpr
       <|> try callExpr
       <|> aexpr

aexpr :: Parser Expr
aexpr =    try ((Konst . KInteger) <$> integer)
       <|> try ((Var . Simple) <$> identifier)
       <|> try tupleExpr
       <|> parens expr

tupleExpr :: Parser Expr
tupleExpr = parens $ do { es <- manyComma expr
                        ; case es of
                            [e] -> return e
                            _   -> return (Tuple es) }

manyComma :: Parser a -> Parser [a]
-- Parse  e1, e2, ..., en
-- where n>=0
manyComma p = do { e1 <- p; es <- many (comma >> spaces >> p); return (e1:es) }
              <|> return []

callExpr :: Parser Expr
-- (f e)
callExpr = do { f <- identifier
              ; e <- aexpr
              ; return (Call (Fun (SFun f)) e) }

iteExpr :: Parser Expr
-- (if e1 e2 e3)
iteExpr = do { reserved "if"
             ; cond <- aexpr
             ; tr   <- aexpr
             ; fl   <- aexpr
             ; return (If cond tr fl) }

assertExpr :: Parser Expr
-- (assert e1 e2)
assertExpr = do { reserved "assert"
                ; e1 <- aexpr
                ; e2 <- expr
                ; return (Assert e1 e2) }

lamExpr :: Parser Expr
-- (lam i e)
lamExpr = do { reserved "lam"
             ; i <- identifier
             ; e <- expr
             ; return (Lam (Simple i) e) }

letExpr :: Parser Expr
-- (let (x r) b)
letExpr = do { reserved "let"
             ; (x,r) <- parens $ do { x <- identifier
                                    ; r <- aexpr
                                    ; return (x,r) }
             ; e <- expr
             ; return (Let (Simple x) r e) }

parseDef :: Parser Def
-- (def f (x, y, z) rhs)
parseDef = parens $ do { reserved "def"
                       ; f <- identifier
                       ; xs <- parens (manyComma identifier)
                       ; rhs <- aexpr
                       ; return (Def (Fun (SFun f)) (map Simple xs) rhs) }


-- Constants
zero :: Parser Expr
zero  = reservedOp "0"   >> return (Konst KZero)

{-
-- Operator expressions
opExpr :: Parser Expr  -- Arithmetic expressions
opExpr = Ex.buildExpressionParser table aexpr
  where
    table :: Ex.OperatorTable String () Identity Expr
    table = [ [
                infixOp "+" (mkInfixCall "+")
              ]
            ]

aexpr :: Parser Expr
aexpr =     liftM KInteger integer
        <|> liftM Var identifier
        <|> parens expr

expr :: Parser Expr
expr =     iteExpr
       <|> letExpr
       <|> opExpr
       <|> aexpr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = semiSep expr

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s




-}