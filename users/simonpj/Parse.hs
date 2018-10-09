module Parse  where

import Lang

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad

------- Parser -------
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
--  , Tok.identLetter     = alphaNum <|> oneOf "_'"
--  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
--  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.identLetter     = alphaNum <|> oneOf "_':!#$%&*+./<=>?@\\^|-~"
  , Tok.opStart         = mzero
  , Tok.opLetter        = mzero
  , Tok.reservedNames   = [ "def", "let", "if"
                          , "assert", "call" ]
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
       <|> try assertExpr
       <|> try callExpr
       <|> try tupleExpr
       <|> try ((Konst . KInteger) <$> integer)
       <|> try ((Var . Simple) <$> identifier)

tupleExpr :: Parser Expr
tupleExpr = parens $ do { es <- many (expr <* comma)
                        ; e  <- expr
                        ; return (Tuple (es ++ [e])) }
                  <|> return (Tuple [])

callExpr :: Parser Expr
-- (call f e)
callExpr = parens $ do { reserved "call"
                      ; f <- identifier
                      ; e <- expr
                      ; return (Call (Fun (SFun f)) e) }

iteExpr :: Parser Expr
-- (if e1 e2 e3)
iteExpr = parens $ do { reserved "if"
                      ; cond <- expr
                      ; tr <- expr
                      ; fl <- expr
                      ; return (If cond tr fl) }

assertExpr :: Parser Expr
-- (assert e1 e2)
assertExpr = parens $ do { reserved "assert"
                         ; e1 <- expr
                         ; e2 <- expr
                         ; return (Assert e1 e2) }

letExpr :: Parser Expr
-- (let (x r) b)
letExpr = parens $ do { reserved "let"
                      ; (x,r) <- parens $ do { x <- identifier
                                             ; r <- expr
                                             ; return (x,r) }
                      ; e <- expr
                      ; return (Let (Simple x) r e) }

parseDef :: Parser Def
-- (def f (x, y, z) rhs)
parseDef = parens $ do { reserved "def"
                       ; f <- identifier
                       ; x <- identifier
                       ; reservedOp "="
                       ; rhs <- expr
                       ; return (Def (Fun (SFun f)) [Simple x] rhs) }


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