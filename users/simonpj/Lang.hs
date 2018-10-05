{-# LANGUAGE FlexibleInstances #-}

module Lang where

import Prelude hiding( (<>) )

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

infixr 0 `seqExpr`

------ Data types ---------


data FunId = SelFun     -- Selector function: fst, snd etc
               Int      -- Index; 1-indexed, so (SelFun 1 2) is fst
               Int      -- Arity
           | SFun String  -- For now
           deriving( Eq, Ord, Show )

data ADMode = Fwd | Rev
            deriving( Eq, Ord, Show )

flipMode :: ADMode -> ADMode
flipMode Fwd = Rev
flipMode Rev = Fwd

data Fun = Fun     FunId         -- The function              f(x)
         | GradFun FunId ADMode  -- Full Jacobian Df(x)
                                 --   Rev <=> transposed  Rf(x)
         | DrvFun  FunId ADMode  -- Derivative derivative f'(x,dx)
                                 --   Rev <=> reverse mode f`(x,dr)
         | LMFun   String        -- Linear map
         deriving( Eq, Ord, Show )

data Var
  = Simple  String         -- x
  | Delta   String         -- The 'dx' or 'dr' argument to fwd
                           -- or backward versions of f
  | Grad    String ADMode  -- \nabla x
                           --   True <=> transposed \bowtie x
  deriving( Show, Eq, Ord )

data Konst = KZero  -- Of any type
           | KInteger Integer
           | KFloat   Float
           | KBool    Bool
           deriving( Eq, Ord, Show )

isKZero :: Expr -> Bool
isKZero (Konst KZero) = True
isKZero _             = False

data DefX b = Def Fun [Var] (ExprX b)  -- f x = e

type Def = DefX Var


type TExpr ty = Expr
  -- The phantom parameter gives the type of
  -- the expresssion, for documentation purposes

data ExprX b
  = Konst Konst
  | Var Var
  | Call Fun (ExprX b)         -- f e
  | Tuple [ExprX b]            -- (e1, ..., en)
  | Lam b (ExprX b)
  | App (ExprX b) (ExprX b)
  | Let b (ExprX b) (ExprX b)  -- let x = e1 in e2  (non-recursive)
  | If (ExprX b) (ExprX b) (ExprX b)
  deriving (Eq, Ord, Show)

type Expr = ExprX Var

data Value = VKonst Konst

mkInfixCall :: Fun -> Expr -> Expr -> Expr
mkInfixCall f a b = Call f (Tuple [a, b])

mkLets :: [(Var,Expr)] -> Expr -> Expr
mkLets [] e = e
mkLets ((v,r):bs) e = Let v r (mkLets bs e)

mkTuple :: [Expr] -> Expr
mkTuple [e] = e   -- One-tuples are always flattened
mkTuple es  = Tuple es

kInt :: Integer -> Expr
kInt i = Konst (KInteger i)

seqExpr :: Expr -> a -> a
seqExpr (Var v) x = v `seq` x
seqExpr (Call fun e) x = fun `seq` e `seqExpr` x
seqExpr (Konst k) x = k `seq` x
seqExpr (Let v r b) x = v `seq` r `seqExpr` b `seqExpr` x
seqExpr (Tuple es) x = Prelude.foldr seqExpr x es

--------------
notFreeIn :: Var -> Expr -> Bool
notFreeIn v e = go e
 where
   go (Var v2) = v /= v2
   go (Konst _) = True
   go (Tuple es) = all go es
   go (If b t e) = go b && go t && go e
   go (App f a)  = go f && go a
   go (Let v2 r b) = go r && (v == v2 || go b)
   go (Lam v2 e)   = v == v2 || go e

------ Pretty printer ------

class Pretty p where
  ppr :: p -> Doc

instance Pretty Var where
  ppr (Simple s)   = PP.text s
  ppr (Delta s)    = PP.text ('d' : s)
  ppr (Grad s Fwd) = PP.text ('D' : s)
  ppr (Grad s Rev) = PP.text ('R' : s)

instance Pretty FunId where
  ppr (SFun s)     = PP.text s
  ppr (SelFun i n) = PP.text "sel_" PP.<> PP.int i PP.<> PP.char '_' PP.<> PP.int n

instance Pretty Fun where
  ppr (Fun s)           = ppr s
  ppr (GradFun s Fwd) = PP.char 'D' PP.<> ppr s
  ppr (GradFun s Rev) = PP.char 'R' PP.<> ppr s
  ppr (DrvFun s Fwd)  = ppr s PP.<> PP.char '\''
  ppr (DrvFun s Rev)  = ppr s PP.<> PP.char '`'
  ppr (LMFun s)        = PP.text s

instance Pretty Konst where
  ppr (KInteger i) = PP.integer i
  ppr (KFloat f)   = PP.float f
  ppr KZero        = text "KZero"

type Prec = Int
 -- 0 => no need for parens
 -- high => parenthesise everything

precZero = 0  -- Base
precOne  = 1  -- +
precTwo  = 2  -- *

instance Pretty Expr where
  ppr expr = pprExpr 0 expr

pprExpr :: Prec -> Expr -> Doc
pprExpr _  (Var v)   = ppr v
pprExpr _ (Konst k)  = ppr k
pprExpr p (Call f e) = pprCall p f e
pprExpr _ (Tuple es) = parens (pprWithCommas es)
pprExpr p (Lam v e)  = parensIf p precZero $
                       PP.char '\\' <> ppr v <> PP.char '.' <+> ppr e
pprExpr p (Let v e1 e2)
  = parensIf p precZero $
    PP.vcat [ PP.text "let" PP.<+>
                (bracesSp $ PP.sep [ ppr v
                                   , PP.nest 2 (PP.text "=" PP.<+> ppr e1) ])
           , ppr e2 ]
pprExpr p (If e1 e2 e3)
  = parensIf p precZero $
    PP.sep [ PP.text "if" PP.<+> ppr e1
           , PP.text "then" PP.<+> ppr e2
           , PP.text "else" PP.<+> ppr e3 ]

pprCall :: Prec -> Fun -> Expr -> Doc
pprCall prec f (Tuple [e1,e2])
  | Just prec' <- isInfix f
  = parensIf prec prec' $
    sep [ pprExpr prec' e1, ppr f <+> pprExpr prec' e2 ]

pprCall _ f e@(Tuple {}) = ppr f PP.<> ppr e
pprCall _ f e            = ppr f PP.<> parensSp (ppr e)

isInfix :: Fun -> Maybe Prec
isInfix (Fun (SFun s))
  | s == "+" = Just precOne
  | s == "-" = Just precOne
  | s == "*" = Just precTwo
  | s == "/" = Just precTwo
isInfix _ = Nothing

parensIf :: Prec -> Prec -> Doc -> Doc
parensIf ctxt inner doc
  | ctxt == precZero = doc
  | ctxt >= inner    = parens doc
  | otherwise        = doc

--  ppr p (If a b c)
--      = sep [ PP.text "if"   PP.<+> ppr p a
--            , PP.text "then" PP.<+> ppr p b
--            , PP.text "else" PP.<+> ppr p c ]

instance Pretty Def where
  ppr (Def f vs rhs)
    = PP.sep [ PP.text "fun" PP.<+> ppr f
                 PP.<> parens (pprWithCommas vs)
             , PP.nest 2 (PP.text "=" PP.<+> ppr rhs) ]


display :: Pretty p => p -> IO ()
display p = putStrLn (PP.render (ppr p))

bracesSp :: Doc -> Doc
bracesSp d = PP.char '{' PP.<+> d PP.<+> PP.char '}'

parensSp :: Doc -> Doc
parensSp d = PP.char '(' PP.<+> d PP.<+> PP.char ')'

pprWithCommas :: Pretty p => [p] -> Doc
pprWithCommas ps = PP.sep (add_commas ps)
  where
     add_commas []     = []
     add_commas [p]    = [ppr p]
     add_commas (p:ps) = ppr p PP.<> PP.comma : add_commas ps

instance Pretty a => Pretty [a] where
  ppr xs = PP.char '[' <> pprWithCommas xs <> PP.char ']'

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