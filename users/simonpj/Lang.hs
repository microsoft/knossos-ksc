{-# LANGUAGE FlexibleInstances #-}

module Lang where

-- import Text.PrettyPrint (Doc, (<>), (<+>))
import Text.PrettyPrint as PP

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Functor.Identity
import Data.Maybe
import Data.Functor

import Control.Monad.Trans
import System.Console.Haskeline

import Data.Map

------ Data types ---------


type FunId = String  -- For now


data Fun = Fun     String       -- The function              f(x)
         | GradFun String Bool  -- Full Jacobian Df(x)
                                --   True <=> transposed  Rf(x)
         | DrvFun  String Bool  -- Derivative derivative f'(x,dx)
                                --   True <=> reverse mode f`(x,dr)
         | LMFun      String  -- Linear map
         deriving( Eq, Show )

data Var
  = Simple  String       -- x
  | Delta   String       -- The 'dx' or 'dr' argument to fwd
                         -- or backward versions of f
  | Grad    String Bool  -- \nabla x
                         --   True <=> transposed \bowtie x
  | Drv     String Bool  -- Let-bound variations (\nabla v `apply` dx)
  deriving( Show, Eq, Ord )

data Konst = KZero  -- Of any type
           | KInteger Integer
           | KFloat   Float
           deriving( Eq, Show )

data Def = Def Fun [Var] Expr  -- f x = e


type TExpr ty = Expr
  -- The phantom parameter gives the type of
  -- the expresssion, for documentation purposes

data Expr
  = Konst Konst
  | Var Var
  | Call Fun Expr      -- f e
  | Tuple [Expr]       -- (e1, ..., en)
  | Let Var Expr Expr  -- let x = e1 in e2  (non-recursive)
  | If Expr Expr Expr
  deriving (Eq, Show)

data Value = VKonst Konst

mkInfixCall :: Fun -> Expr -> Expr -> Expr
mkInfixCall f a b = Call f (Tuple [a, b])


------ Pretty printer ------

parensIf ::  Bool -> Doc -> Doc
parensIf True  = PP.parens
parensIf False = id

class Pretty p where
  ppr :: p -> Doc

instance Pretty Var where
  ppr (Simple s)     = PP.text s
  ppr (Delta s)      = PP.text ('d' : s)
  ppr (Grad s False) = PP.text ('D' : s)
  ppr (Grad s True)  = PP.text ('R' : s)
  ppr (Drv s False)  = PP.text ('v' : s)
  ppr (Drv s True)   = PP.text ('r' : s)

instance Pretty Fun where
  ppr (Fun s)           = PP.text s
  ppr (GradFun s False) = PP.text ('D' : s)
  ppr (GradFun s True)  = PP.text ('R' : s)
  ppr (DrvFun s False)  = PP.text s <> PP.char '\''
  ppr (DrvFun s True)   = PP.text s <> PP.char '`'
  ppr (LMFun s)         = PP.text s

instance Pretty Konst where
  ppr (KInteger i) = PP.integer i
  ppr (KFloat f)   = PP.float f
  ppr KZero        = text "KZero"

instance Pretty Expr where
  ppr (Var v)       = ppr v
  ppr (Konst k)     = ppr k
  ppr (Call f e@(Tuple _)) = ppr f <> ppr e
  ppr (Call f e)           = ppr f <> parensSp (ppr e)
  ppr (Tuple es)    = parens (pprWithCommas es)
  ppr (Let v e1 e2) = PP.vcat [ PP.text "let" <+>
                               (bracesSp $ PP.sep [ ppr v
                                                  , PP.nest 2 (PP.text "=" <+> ppr e1) ])
                             , ppr e2 ]
--  ppr p (If a b c)
--      = sep [ PP.text "if"   <+> ppr p a
--            , PP.text "then" <+> ppr p b
--            , PP.text "else" <+> ppr p c ]

instance Pretty Def where
  ppr (Def f vs rhs)
    = PP.sep [ PP.text "fun" <+> ppr f
                 <> parens (pprWithCommas vs)
             , PP.nest 2 (PP.text "=" <+> ppr rhs) ]


display :: Pretty p => p -> IO ()
display p = putStrLn (PP.render (ppr p))

bracesSp :: Doc -> Doc
bracesSp d = PP.char '{' <+> d <+> PP.char '}'

parensSp :: Doc -> Doc
parensSp d = PP.char '(' <+> d <+> PP.char ')'

pprWithCommas :: Pretty p => [p] -> Doc
pprWithCommas ps = PP.sep (add_commas ps)
  where
     add_commas []     = []
     add_commas [p]    = [ppr p]
     add_commas (p:ps) = ppr p <> PP.comma : add_commas ps


{-
------- Parser -------
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = ["let", "in"]
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

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

infixOp :: String -> (a -> a -> a) -> Ex.Operator String () Identity a
infixOp s f = Ex.Prefix (reservedOp s >> return f)

-- If/then/else
iteExpr :: Parser Expr
iteExpr = do { reserved "if"
            ; cond <- expr
            ; reservedOp "then"
            ; tr <- expr
            ; reserved "else"
            ; fl <- expr
            ; return (If cond tr fl) }

letExpr :: Parser Expr
letExpr = do { reserved "let"
             ; b <- parseBind
             ; reserved "in"
             ; e <- expr
             ; return (Let b e) }

parseBind :: Parser Bind
parseBind = do { fun <- identifier
               ; arg <- identifier
               ; reservedOp "="
               ; rhs <- expr
               ; return (Bind fun arg rhs) }

-- Constants
true, false, zero :: Parser Expr
true  = reserved "true"  >> return Tr
false = reserved "false" >> return Fl
zero  = reservedOp "0"   >> return Zero

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