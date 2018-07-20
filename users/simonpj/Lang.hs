module Lang where

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

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

data Name x = Simple x   -- x
            | Grad x     -- \nabla x
            deriving( Show, Eq )

type FunId = String  -- For now


type Fun = Name FunId
type Var = Name String
type Const = Integer

data Bind = Bind Fun Var Expr  -- f x = e

data Expr
  = Let Var Expr Expr  -- let x = e1 in e2  (non-recursive)
  | Const Const
  | Var Var
  | Call Fun Expr
  | If Expr Expr Expr
  deriving (Eq, Show)

data Value = VConst Const

mkInfixCall :: Fun -> Expr -> Expr -> Expr
mkInfixCall f a b = Call f [a,b]

{-
data FunValue = FunValue Env Var Expr

------ Pretty printer ------

parensIf ::  Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Expr where
  ppr _ (Var v)     = PP.text v
  ppr _ (Const k)   = PP.int k
  ppr _ (Call f es) = PP.text f <> parens (pprWithCommas es)
  ppr p (Let b e) = sep [ PP.text "let"
                        , nest 2 (ppr b)
                        , PP.text "in" <+> ppr p e ]
  ppr p (If a b c)
      = sep [ PP.text "if"   <+> ppr p a
            , PP.text "then" <+> ppr p b
            , PP.text "else" <+> ppr p c ]

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0

pprWithCommas :: Pretty p => [p] -> p
pprWithCommas ps = sep (add_commas ps)
  where
     add_commas []     = []
     add_commas [p]    = [p]
     add_commas (p:ps) = p <> PP.comma : add_commas ps


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
aexpr =     liftM Const integer
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