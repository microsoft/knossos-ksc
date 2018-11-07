module Parse  where

{- The language we parse
~~~~~~~~~~~~~~~~~~~~~~~~
Here's the BNF for our language:

<def> ::= ( def <var> <params> <expr> )

<expr> ::= <atom> | ( <kexpr> )

<param> ::= <var> | (<var> ":" <type>)

<type> ::= "Integer" | "Float" | "Vec" <type> | "Tuple" (<types>)

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
  (def f7 ((x : Vec Float) (y : Vec Float))
       (assert (== (size(x)) (size(y)))
         (sum (build (size x)
                     (lam i (* (index i x)
                               (index i y)
   ))))))


We have comments, thus
   -- Comment to end of line
   {- block comment -}

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

import Test.Hspec
import Debug.Trace


mkTunk:: ExprX -> Expr
mkTunk e = Expr TypeUnknown e

mkFunk :: String -> TFun
mkFunk f = TFun TypeUnknown (Fun (SFun f)) 

---------------------
testParse :: Pretty a => Parser a -> String -> IO ()
testParse  p s = case runParser p s of
                   Left err -> putStrLn ("Failed: " ++ show err)
                   Right r  -> putStrLn (PP.render (ppr r))

runParser :: Parser a -> String -> Either ParseError a
runParser p s = parse p "" s

parseF :: String -> IO [Def]
parseF file = do
        cts <- readFile file
        case runParser pDefs cts of
                    Left err   -> error ("Failed parse: " ++ show err)
                    Right defs -> return defs

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
  , Tok.reservedNames   = [ "def", "let", "if", "assert", "call", "tuple", ":" ]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

pReserved :: String -> Parser ()
pReserved = Tok.reserved lexer

pInteger :: Parser Integer
pInteger = Tok.integer lexer

pDouble :: Parser Double
pDouble = Tok.float lexer

pIdentifier :: Parser String
pIdentifier = Tok.identifier lexer

pVar :: Parser Var
pVar = Simple <$> pIdentifier 

pParam :: Parser TVar
pParam = parens (do {
                    v <- pVar;
                    pReserved ":";
                    ty <- pType;
                    return (TVar ty v)
         })

pKonst :: Parser Expr
pKonst =   try ((Expr TypeFloat . Konst . KFloat) <$> pDouble)
       <|> ((Expr TypeInteger . Konst . KInteger) <$> pInteger)

pExpr :: Parser Expr
pExpr = pKonst
   <|> (mkTunk . Var . Simple) <$> pIdentifier
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

pType :: Parser Type
pType = do {
          id <- pIdentifier;
          case id of
          "Integer" -> return TypeInteger
          "Float" -> return TypeFloat
          "Vec" -> TypeVec <$> pType
          "Tuple" -> TypeTuple <$> parens (many pType) 
          _ -> error $ "Unknown type [" ++ id ++ "]"
        }

pCall :: Parser Expr
-- (f e)
pCall = do { f <- pIdentifier
           ; es <- many pExpr
           ; case es of
               []  -> return (mkTunk $ Var (Simple f))
               [e] -> return (mkTunk $ Call (mkFunk f) e)
               _   -> return (mkTunk $ Call (mkFunk f) (mkTuple es))
        }

pIfThenElse :: Parser Expr
-- (if e1 e2 e3)
pIfThenElse = do { pReserved "if"
                 ; cond <- pExpr
                 ; tr   <- pExpr
                 ; fl   <- pExpr
                 ; return $ mkTunk $ If cond tr fl }

pAssert :: Parser Expr
-- (assert e1 e2)
pAssert = do { pReserved "assert"
             ; e1 <- pExpr
             ; e2 <- pExpr
             ; return $ mkTunk $ Assert e1 e2 }

pTuple :: Parser Expr
-- (assert e1 e2)
pTuple = do { pReserved "tuple"
            ; es <- many pExpr
            ; return $ mkTunk $ Tuple es }

pLam :: Parser Expr
-- (lam i e)
pLam = do { pReserved "lam"
          ; i <- pParam
          ; e <- pExpr
          ; return $ mkTunk $ Lam i e }

pBind :: Parser (TVar, Expr)
-- var rhs
pBind = do { v <- pIdentifier
           ; e <- pExpr
          ; return (TVar TypeUnknown $ Simple v, e) }

pLet :: Parser Expr
-- (let (x r) b)
pLet = do { pReserved "let"
          ; pairs <- parens $ do { b <- pBind
                                 ; return [b] }
                          <|> many (parens pBind)
          ; e <- pExpr
          ; return $ foldr (\(v,r) e -> mkTunk $ Let v r e) e pairs }

pDef :: Parser Def
-- (def f (x1 x2 x3) rhs)
pDef = parens $ do { pReserved "def"
                   ; f <- pIdentifier
                   ; xs <- parens (many pParam)
                   ; rhs <- pExpr
                   ; return (Def (mkFunk f) xs rhs) }

pDefs :: Parser [Def]
pDefs = spaces >> many pDef


---------------------- Tests ------------------

toStr :: Pretty a => Parser a -> String -> String
toStr p s = case runParser p s of
                   Left err -> error ("Failed: " ++ show err)
                   Right r  -> show (PP.render (ppr r))

test p src expected = it src $ (toStr p src) `shouldBe` (show expected)

test_Parser =
  hspec $ do
    describe "Parser" $ do
      test pExpr "(f 1)" "f( 1 )" ;
      test pExpr "(if (f 1 2 3) (let (v (+ 2 3)) (* v 7)) 0.7)" $
                    "if f( 1, 2, 3 )\nthen let { (v : TypeUnknown) = 2 + 3 }\n     v * 7\nelse 0.7"
