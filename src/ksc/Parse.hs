module Parse  where

{- The language we parse
~~~~~~~~~~~~~~~~~~~~~~~~
Here's the BNF for our language:

<decl> ::= <def> | <rule>

<rule> ::= ( rule <string> <params> <expr> <expr> )

<def> ::= ( def <var> <params> <expr> )

<expr> ::= <atom> | ( <kexpr> )

<params> ::= <param> | ( <param1> ... <param>n )

<param>  ::= ( <var> ":" <type> )

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
import Prim

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


---------------------
testParse :: Pretty a => Parser a -> String -> IO ()
testParse  p s = case runParser p s of
                   Left err -> putStrLn ("Failed: " ++ show err)
                   Right r  -> putStrLn (PP.render (ppr r))

runParser :: Parser a -> String -> Either ParseError a
runParser p s = parse p "" s

runParserOrPanic :: Parser a -> String -> a
runParserOrPanic p s = case runParser p s of
                        Left err -> error $ show err
                        Right r -> r

parseF :: String -> IO [Decl]
parseF file = do
        cts <- readFile file
        case runParser pDecls cts of
                    Left err    -> error ("Failed parse: " ++ show err)
                    Right decls -> return decls

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
  , Tok.reservedNames   = [ "def", "rule", "let", "if", "assert", "call", "tuple", ":" ]
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

pString :: Parser String
pString = Tok.stringLiteral lexer

pIdentifier :: Parser String
pIdentifier = Tok.identifier lexer

pVar :: Parser Var
pVar = Simple <$> pIdentifier

pParam :: Parser TVar
pParam = do { v <- pVar
            ; pReserved ":"
            ; ty <- pType
            ; return (TVar ty v) }

pParams :: Parser [TVar]
pParams = parens $ do { b <- pParam
                      ; return [b] }
               <|> many (parens pParam)

pKonst :: Parser (ExprX Fun Var)
pKonst =   try (( Konst . KFloat) <$> pDouble)
       <|> ((Konst . KInteger) <$> pInteger)

pExpr :: Parser (ExprX Fun Var)
pExpr = pKonst
   <|> (Var . Simple) <$> pIdentifier
   <|> parens pKExpr

pKExpr :: Parser (ExprX Fun Var)
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

pCall :: Parser (ExprX Fun Var)
-- (f e)
pCall = do { f <- pIdentifier
           ; es <- many pExpr
           ; case es of
               []  -> return (Var (Simple f))
               [e] -> return (Call (mkFun f) e)
               _   -> return (Call (mkFun f) (Tuple es))
        }
      
pIfThenElse :: Parser (ExprX Fun Var)
-- (if e1 e2 e3)
pIfThenElse = do { pReserved "if"
                 ; cond <- pExpr
                 ; tr   <- pExpr
                 ; fl   <- pExpr
                 ; return $ If cond tr fl }

pAssert :: Parser (ExprX Fun Var)
-- (assert e1 e2)
pAssert = do { pReserved "assert"
             ; e1 <- pExpr
             ; e2 <- pExpr
             ; return $ Assert e1 e2 }

pTuple :: Parser (ExprX Fun Var)
-- (assert e1 e2)
pTuple = do { pReserved "tuple"
            ; es <- many pExpr
            ; return $ Tuple es }

pLam :: Parser (ExprX Fun Var)
-- (lam i e)
pLam = do { pReserved "lam"
          ; TVar ty v <- parens pParam
          ; e <- pExpr
          ; return $ Lam (TVar ty v) e }

pBind :: Parser (Var, ExprX Fun Var)
-- var rhs
pBind = do { v <- pIdentifier
           ; e <- pExpr
          ; return (Simple v, e) }

pLet :: Parser (ExprX Fun Var)
-- (let (x r) b)
pLet = do { pReserved "let"
          ; pairs <- parens $ do { b <- pBind
                                 ; return [b] }
                          <|> many (parens pBind)
          ; e <- pExpr
          ; return $ foldr (\(v,r) e -> Let v r e) e pairs }

pDef :: Parser (Def)
-- (def f (x1 x2 x3) rhs)
pDef = do { pReserved "def"
          ; f <- pIdentifier
          ; xs <- pParams
          ; rhs <- pExpr
          ; return (DefX (Fun (UserFun f)) xs rhs) }

pRule :: Parser Rule
pRule = do { pReserved "rule"
           ; name <- pString
           ; qvars <- pParams
           ; lhs <- pExpr
           ; rhs <- pExpr
           ; return (Rule { ru_name = name, ru_qvars = qvars
                          , ru_lhs = lhs, ru_rhs = rhs }) }

pDecl :: Parser Decl
pDecl = parens ( (DefDecl  <$> pDef)
                 <|>  (RuleDecl <$> pRule ))

pDecls :: Parser [Decl]
pDecls = spaces >> many pDecl


---------------------- Tests ------------------

toStr :: Pretty a => Parser a -> String -> String
toStr p s = case runParser p s of
                   Left err -> error ("Failed: " ++ show err)
                   Right r  -> show (PP.render (ppr r))

{-
test_Parser =
  hspec $ do
    describe "Parser" $ do
      test pExpr "(f 1)" "f( 1 )" ;
      test pExpr "(if (f 1 2 3) (let (v (+ 2 3)) (* v 7)) 0.7)" $
                    "if f( 1, 2, 3 )\nthen let { v = 2 + 3 }\n     v * 7\nelse 0.7"
      test pDef "(def f ((x : Integer)) (lam (y : Float) (+ x y)))" $
                "def f((x : Integer)) = (lam (y : Float)  x + y)" ;
  where test p src expected = it src $ (toStr p src) `shouldBe` (show expected)
-}