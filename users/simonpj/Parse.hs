module Parse  where

{- The language we parse
~~~~~~~~~~~~~~~~~~~~~~~~
Here's the BNF for our language:

<def> ::= ( def <var> <vars> <expr> )

<expr> ::= <atom> | ( <kexpr> )

<kexpr> ::= let <bind>                <expr>
        |   let (<bind>1 ... <bind>n) <epxr>      n >= 0
        |   assert <expr> <expr>
        |   lam <var> <expr>
        |   if <expr> <expr> <expr>
        |   tuple <expr>1 ... <expr>n      n >= 0
        |   <var> <exrp>1 ... <expr>n      calls, n >= 1
        |   atom

atom ::= <konst> | <var>

<binds> ::= (<var> <expr>)

An example
  (def f7 (x y )
       (assert (== (size(x)) (size(y)))
         (sum (build (size x)
                     (lam i (* (index i x)
                               (index i y)
   ))))))


Notes:

* (f e1 e2)  means the call    f (e1,e2)
  (f (tuple e1 e2)) also means f (e1,e2)
  (f e)      means the call f e
  (f)        means just the variable f

* Explicit tuples (not the argument of calls)
  are written (tuple e1 .. en)

* If you want a nullary application f (),
  write  (f (tuple))
  i.e with an explicit, 0-ary tuple

* The call (f x) and (f (x)), where x is a
  variable, mean the same: a call of f(x)

* As in Lisp there are two forms of let
  - One binding   (let (x rhs) body)
  - Multi binding (let ((x1 rhs2) (x2 rhs2)) body)

-}


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
  , Tok.reservedNames   = [ "def", "let", "if", "assert", "call", "tuple" ]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

pReseerved :: String -> Parser ()
pReseerved = Tok.reserved lexer

pInteger :: Parser Integer
pInteger = Tok.integer lexer

pIdentifier :: Parser String
pIdentifier = Tok.identifier lexer

pKonst :: Parser Expr
pKonst = (Konst . KInteger) <$> pInteger

pExpr :: Parser Expr
pExpr = pKonst
   <|> (Var . Simple) <$> pIdentifier
   <|> parens pKExpr

pKExpr :: Parser Expr
-- Al these start with a keyword
pKExpr =    pIfThenElse
       <|> pLet
       <|> pLam
       <|> pAssert
       <|> pCall
       <|> pTuple
       <|> pKonst


pCall :: Parser Expr
-- (f e)
pCall = do { f <- pIdentifier
           ; es <- many pExpr
           ; case es of
               []  -> return (Var (Simple f))
               [e] -> return (Call (Fun (SFun f)) e)
               _   -> return (Call (Fun (SFun f)) (Tuple es)) }

pIfThenElse :: Parser Expr
-- (if e1 e2 e3)
pIfThenElse = do { pReseerved "if"
                 ; cond <- pExpr
                 ; tr   <- pExpr
                 ; fl   <- pExpr
                 ; return (If cond tr fl) }

pAssert :: Parser Expr
-- (assert e1 e2)
pAssert = do { pReseerved "assert"
             ; e1 <- pExpr
             ; e2 <- pExpr
             ; return (Assert e1 e2) }

pTuple :: Parser Expr
-- (assert e1 e2)
pTuple = do { pReseerved "tuple"
            ; es <- many pExpr
            ; return (Tuple es) }

pLam :: Parser Expr
-- (lam i e)
pLam = do { pReseerved "lam"
          ; i <- pIdentifier
          ; e <- pExpr
          ; return (Lam (Simple i) e) }

pBind :: Parser (Var, Expr)
-- var rhs
pBind = do { v <- pIdentifier
           ; e <- pExpr
          ; return (Simple v,e) }

pLet :: Parser Expr
-- (let (x r) b)
pLet = do { pReseerved "let"
          ; pairs <- parens $ do { b <- pBind
                                 ; return [b] }
                          <|> many (parens pBind)
          ; e <- pExpr
          ; return (foldr (\(v,r) e -> Let v r e) e pairs) }

parseDef :: Parser Def
-- (def f (x, y, z) rhs)
parseDef = parens $ do { pReseerved "def"
                       ; f <- pIdentifier
                       ; xs <- parens (many pIdentifier)
                       ; rhs <- pExpr
                       ; return (Def (Fun (SFun f)) (map Simple xs) rhs) }
