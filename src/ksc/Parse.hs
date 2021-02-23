-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances,
             PatternSynonyms,
	     ScopedTypeVariables #-}

module Parse  where

{- The language we parse
~~~~~~~~~~~~~~~~~~~~~~~~
Here's the BNF for our language:

<decl> ::= <def> | <rule> | <edef> | <gdef>

<rule> ::= ( rule <string> <params> <expr> <expr> )

<def> ::= ( def <sname> <type> <params> <expr> )
-- (def f Float ( (x : Float) (y : Vec Float) ) (...) )

<edef> ::= ( edef <sname> <type> <types> )

<gdef> ::= ( gdef <derivation> <sname> )

<params> ::= <param> | ( <param_1> ... <param_n> )
<param>  ::= ( <var> ":" <type> )

-- <type> is atomic; <ktype> is compound
<type>   ::= "Integer" | "Float" | "String" | "Bool" | ( <ktype> )
<ktype>  ::= "Tensor" <int> <type>
           | "Vec" <type> -- shorthand for Tensor 1 Type
           | "Tuple" <type_1> .. <type_n>
           | <type>
<types>  ::= ( <type_1> ... <type_n> )

{-
x : Tensor 1 Float
x : Tensor 1 (Tensor 2 Float)
-}

-- <expr> is atomic; <kexpr> is compound
<expr> ::= <konst> | <var> | ( <kexpr> )
<kexpr> ::= let <bind>                <expr>
        |   let (<bind>1 ... <bind>n) <epxr>      n >= 0
        |   assert <expr> <expr>
        |   lam <var> <expr>
        |   if <expr> <expr> <expr>
        |   tuple <expr>1 ... <expr>n      n >= 0
        |   $dummy <type>
        |   <sname> <exrp>1 ... <expr>n      calls, n >= 1
        |   <expr>

<binds> ::= (<pat> <expr>)

<pat> ::= <var>
      |   (<var>1 ... <var>n)

<sname> ::= <var>
        | "[" <derivation> <sname> "]"
        | "[" <var> <type> "]"

<derivation> ::= "rev" | "fwd" | "shape" | "cost" | "D" | "Dt" | "suffwdpass" | "sufrevpass"

An example
  (def f7 ((x : Vec Float) (y : Vec Float))
       (assert (== (size(x)) (size(y)))
         (sum (build (size x)
                     (lam i (mul (index i x)
                               (index i y)
   ))))))


We have comments, thus
   ; Comment to end of line
   #| block comment |#

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


import Lang hiding (parens, brackets)
import Prim

import Text.Parsec( (<|>), try, many, parse, eof, manyTill, ParseError, unexpected )
import Text.Parsec.Char
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

import Data.Functor ( ($>) )
import Control.Monad
import Text.Read ( readMaybe )

--import Test.Hspec


---------------------
testParse :: Pretty a => Parser a -> String -> IO ()
testParse  p s = case runParser p s of
                   Left err -> putStrLn ("Failed: " ++ show err)
                   Right r  -> putStrLn (render (ppr r))

runParser :: Parser a -> String -> Either ParseError a
runParser p = parse p ""

runParserOrPanic :: Parser a -> String -> a
runParserOrPanic p s = case runParser p s of
                        Left err -> error $ show err
                        Right r -> r

parseF :: String -> IO [Decl]
parseF file = do
        cts <- readFile file
        return (parseS cts)

parseS :: String -> [Decl]
parseS cts =
        case parseE cts of
                    Left err    -> error err
                    Right decls -> decls

parseE :: String -> Either String [Decl]
parseE cts = case runParser pDecls cts of
                    Left err    -> Left ("Failed parse: " ++ show err)
                    Right decls -> Right decls

------- Parser -------
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "#|"
  , Tok.commentEnd      = "|#"
  , Tok.commentLine     = ";"
  , Tok.nestedComments  = True
--  , Tok.identLetter     = alphaNum <|> oneOf "_'"
--  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
--  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.identStart      = letter <|> oneOf "_':!$%&*+.,/<=>?@\\^|-~"
  , Tok.identLetter     = alphaNum <|> oneOf "_':!$%&*+.,/<=>?@\\^|-~"
  , Tok.opStart         = mzero
  , Tok.opLetter        = mzero
  , Tok.reservedNames   = [ "def", "edef", "rule"
                          , "let", "if", "assert", "call", "tuple", ":", "$dummy"
                          , "Integer", "Float", "Vec", "Lam", "String", "true", "false"
                          ]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

pReserved :: String -> Parser ()
pReserved = Tok.reserved lexer

pInt :: Parser Int
pInt = fromInteger <$> pInteger

pInteger :: Parser Integer
pInteger = Tok.integer lexer

-- Along the same lines as the definition of integer from
-- http://hackage.haskell.org/package/parsec-3.0.0/docs/src/Text-Parsec-Token.html#makeTokenParser
pDouble :: Parser Double
pDouble = do
  s <- Tok.lexeme lexer pSign
  f <- Tok.float lexer
  return (s f)

pSign :: Num a => Parser (a -> a)
pSign = (Text.Parsec.Char.char '-' >> return negate)
        <|> (Text.Parsec.Char.char '+' >> return id)
        <|> return id

pString :: Parser String
pString = Tok.stringLiteral lexer

pIdentifier :: Parser String
pIdentifier = Tok.identifier lexer

pBool :: Parser Bool
pBool = (True <$ pReserved "true")
        <|> (False <$ pReserved "false")

pVar :: Parser Var
pVar = Simple <$> pIdentifier

pParam :: Parser TVarX
pParam = do { v <- pVar
            ; pReserved ":"
            ; ty <- pKType
            ; return (TVar ty v) }

pParams :: Parser [TVarX]
pParams = parens $ do { b <- pParam
                      ; return [b] }
               <|> many (parens pParam)

pKonst :: Parser (ExprX Parsed)
pKonst =   try (( Konst . KFloat) <$> pDouble)
       <|> ((Konst . KInteger) <$> pInteger)
       <|> ((Konst . KString) <$> pString)
       <|> ((Konst . KBool) <$> pBool)

pExpr :: Parser (ExprX Parsed)
pExpr = pKonst
   <|> (Var . Simple) <$> pIdentifier
   <|> parens pKExpr

pKExpr :: Parser (ExprX Parsed)
-- All these start with a keyword
pKExpr =   pIfThenElse
       <|> pLet
       <|> pLam
       <|> pAssert
       <|> pCall
       <|> pTuple
       <|> pDummy

pType :: Parser TypeX
pType = (pReserved "Integer" >> return TypeInteger)
    <|> (pReserved "Float"   >> return TypeFloat)
    <|> (pReserved "String"  >> return TypeString)
    <|> (pReserved "Bool"    >> return TypeBool)
    <|> parens pKType

pTypes :: Parser [TypeX]
pTypes = parens (many pType)

pKType :: Parser TypeX
pKType =   (do { pReserved "Vec"; ty <- pType; return (TypeTensor 1 ty) })
       <|> (do { pReserved "Tensor"; d <- pInt; ty <- pType; return (TypeTensor d ty)})
       <|> (do { pReserved "Tuple"; tys <- many pType; return (TypeTuple tys) })
       <|> (do { pReserved "LM"; s <- pType; t <- pType ; return (TypeLM s t) })
       <|> (do { pReserved "Lam"; s <- pType; t <- pType ; return (TypeLam s t) })
       <|> pType

pCall :: Parser (ExprX Parsed)
-- Calls (f e), (f e1 e2), etc
pCall = do { f <- pFun
           ; es <- many pExpr
           -- See Note [Function arity]
           ; return (Call f (mkTuple es))
        }

pIfThenElse :: Parser (ExprX Parsed)
-- (if e1 e2 e3)
pIfThenElse = do { pReserved "if"
                 ; cond <- pExpr
                 ; tr   <- pExpr
                 ; fl   <- pExpr
                 ; return $ If cond tr fl }

pAssert :: Parser (ExprX Parsed)
-- (assert e1 e2)
pAssert = do { pReserved "assert"
             ; e1 <- pExpr
             ; e2 <- pExpr
             ; return $ Assert e1 e2 }

pTuple :: Parser (ExprX Parsed)
-- (tuple e1 ... en)
pTuple = do { pReserved "tuple"
            ; es <- many pExpr
            ; return $ Tuple es }

pDummy :: Parser (ExprX Parsed)
-- ($dummy type)
pDummy = do { pReserved "$dummy"
            ; ty <- pType
            ; return $ Dummy ty }

pLam :: Parser (ExprX Parsed)
-- (lam i e)
pLam = do { pReserved "lam"
          ; bndr <- parens pParam
          ; e <- pExpr
          ; return $ Lam bndr e }

pPat :: Parser (PatG Var)
-- var or (var1 ... varn)
pPat =     (VarPat <$> pVar)
       <|> (TupPat <$> parens pTuplePat)

pTuplePat :: Parser [Var]
-- var1 ... varn
pTuplePat = do { es <- many pIdentifier
               ; return $ map Simple es }

pBind :: Parser (PatG Var, ExprX Parsed)
-- var rhs or (var1 ... varn) rhs
pBind = do { pat <- pPat
           ; e <- pExpr
           ; return (pat, e) }

pLet :: Parser (ExprX Parsed)
-- (let (x r) b)
pLet = do { pReserved "let"
          ; pairs <- parens $ (try $ do { b <- pBind
                                        ; return [b] })
                          <|> many (parens pBind)
          ; e <- pExpr
          ; return $ foldr (\(v,r) e -> Let v r e) e pairs }

pIsUserFun :: InPhase p => Fun p -> Parser (UserFun p)
pIsUserFun fun = case maybeUserFun fun of
  Nothing -> unexpected ("Unexpected non-UserFun in Def: " ++ render (ppr fun))
  Just userFun -> pure userFun

pPrimFun :: Parser (BaseFun p)
pPrimFun = try $ do { f <- pIdentifier
                    ; when (not (isPrimFun f))
                           (unexpected (f ++ " is not a PrimFun"))
                    ; pure (PrimFun f)
                    }

pSelFun :: Parser (BaseFun p)
pSelFun = do { rest <- try $ do { f <- pIdentifier
                                ; case break (== '$') f of
                                    ("get", '$':rest) -> pure rest
                                    _ -> unexpected "Did not start with get$"
                                }
             ; let mselfun = do { (istring, '$':nstring) <- pure (break (== '$') rest)
                                ; i <- readMaybe istring
                                ; n <- readMaybe nstring
                                ; pure (SelFun i n)
                                }
             ; case mselfun of
                 Nothing     -> unexpected "Ill-formed get"
                 Just selfun -> pure selfun
             }

pBaseUserFunWithType :: (Type -> BaseUserFunT p) -> Parser (BaseUserFun p)
pBaseUserFunWithType add =
     brackets (do { f  <- pIdentifier
                  ; ty <- pType
                  ; pure (BaseUserFunId f (add ty))
                  })

pBaseUserFunWithoutType :: Parser (BaseUserFun Parsed)
pBaseUserFunWithoutType =
         do { f <- pIdentifier
            ; pure (BaseUserFunId f Nothing)
            }

pBaseUserFun :: Parser (BaseFun Parsed)
pBaseUserFun = BaseUserFun <$>
     (pBaseUserFunWithType Just
     <|> pBaseUserFunWithoutType)

pBaseFun :: Parser (BaseFun Parsed)
pBaseFun = pSelFun
       <|> pPrimFun
       <|> pBaseUserFun

pFunG :: forall p. Parser (BaseFun p) -> Parser (Fun p)
pFunG pBase = try (brackets $
            ((pBaseDerivation "D" GradFun BasicAD)
         <|> (pBaseDerivation "Dt" GradFun TupleAD)
         <|> (pBaseDerivation "fwd" DrvFun (AD BasicAD Fwd))
         <|> (pBaseDerivation "fwdt" DrvFun (AD TupleAD Fwd))
         <|> (pBaseDerivation "rev"  DrvFun (AD BasicAD Rev))
         <|> (pBaseDerivation "revt" DrvFun (AD TupleAD Rev))
         <|> (pReserved "CL"    >> CLFun    <$> pBase)
         <|> (pReserved "shape" >> ShapeFun <$> pFunG pBase)))
   <|> Fun <$> pBase
  where pBaseDerivation :: String
                        -> (BaseFun p -> b -> b1)
                        -> b
                        -> Parser b1
        pBaseDerivation s f p =
          pReserved s >> flip f p <$> pBase

pFunTyped :: Parser (Fun Typed)
pFunTyped = pFunG (BaseUserFun <$> pBaseUserFunWithType id)

pFun :: Parser (Fun Parsed)
pFun = pFunG pBaseFun

pDef :: Parser Def
-- (def f Type ((x1 : Type) (x2 : Type) (x3 : Type)) rhs)
pDef = do { pReserved "def"
          ; f <- pFun
          ; ty <- pType
          ; xs <- pParams
          ; rhs <- pExpr
          ; mk_fun_f <- pIsUserFun f
          -- See Note [Function arity]
          ; let pat = case xs of
                  [x] -> VarPat x
                  xs  -> TupPat xs
          ; return (Def { def_fun    = mk_fun_f
                        , def_pat    = pat
                        , def_rhs    = UserRhs rhs
                        , def_res_ty = ty }) }

pRule :: Parser Rule
pRule = do { pReserved "rule"
           ; name <- pString
           ; qvars <- pParams
           ; lhs <- pExpr
           ; rhs <- pExpr
           ; return (Rule { ru_name = name, ru_qvars = qvars
                          , ru_lhs = lhs, ru_rhs = rhs }) }

pEdef :: Parser (DefX Parsed)
pEdef = do { pReserved "edef"
           ; f          <- pFun
           ; returnType <- pType
           ; argTypes   <- pTypes
           ; mk_fun_name <- pIsUserFun f
           ; return (Def { def_fun = mk_fun_name
                         , def_res_ty = returnType
                         -- See note [Function arity]
                         , def_pat = VarPat (mkTVar (mkTupleTy argTypes) "edefArgVar")
                         , def_rhs = EDefRhs }) }

pDerivation :: Parser Derivation
pDerivation =
      (pReserved "fwd" $> DerivationDrvFun (AD BasicAD Fwd))
  <|> (pReserved "rev" $> DerivationDrvFun (AD BasicAD Rev))
  <|> (pReserved "CL"  $> DerivationCLFun)
  <|> (pReserved "shape" $> DerivationShapeFun)

pGDef :: Parser GDefX
pGDef = do { pReserved "gdef"
           ; d <- pDerivation
           ; f <- pFunTyped
           ; mk_fun_f <- pIsUserFun f
           ; pure (GDef d mk_fun_f)
           }


pDecl :: Parser Decl
pDecl = parens $
             (DefDecl  <$> pDef)
        <|>  (DefDecl  <$> pEdef)
        <|>  (RuleDecl <$> pRule)
        <|>  (GDefDecl <$> pGDef)

pDecls :: Parser [Decl]
-- NB: `Tok.whiteSpace lexer` matches whitespace *or* comments
--
--         https://hackage.haskell.org/package/parsec-3.1.11/docs/src/Text.Parsec.Token.html#local-1627451418
--
-- `spaces` is not good enough because that will fail to parse a
-- comment and therefore cause a file that starts with a comment to
-- fail to parse.
pDecls = Tok.whiteSpace lexer >> manyTill pDecl eof
