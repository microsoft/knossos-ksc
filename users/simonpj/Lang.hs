{-# LANGUAGE FlexibleInstances #-}

module Lang where

import Prelude hiding( (<>) )

import Text.PrettyPrint as PP
import KMonad

import Data.Functor.Identity
import Data.Maybe
import Data.Functor

import System.Console.Haskeline

import Data.Map as M
import Debug.Trace( trace )
import Test.Hspec

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
           | KFloat   Double
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
  | Assert (ExprX b) (ExprX b)
  deriving (Show)

type Expr = ExprX Var

-- awf: is this needed?
-- data Value = VKonst Konst

mkInfixCall :: Fun -> Expr -> Expr -> Expr
mkInfixCall f a b = Call f (Tuple [a, b])

mkSCall1 :: String -> Expr -> Expr
mkSCall1 fname a = Call (Fun (SFun fname)) a

mkSCall2 :: String -> Expr -> Expr -> Expr
mkSCall2 fname a b = Call (Fun (SFun fname)) (Tuple [a, b])

mkSCall3 :: String -> Expr -> Expr -> Expr -> Expr
mkSCall3 fname a b c = Call (Fun (SFun fname)) (Tuple [a, b, c])

mkLets :: [(Var,Expr)] -> Expr -> Expr
mkLets [] e = e
mkLets ((v,r):bs) e = Let v r (mkLets bs e)

mkTuple :: [Expr] -> Expr
mkTuple [e] = e   -- One-tuples are always flattened
mkTuple es  = Tuple es

kInt :: Integer -> Expr
kInt i = Konst (KInteger i)

kFloat :: Double -> Expr
kFloat f = Konst (KFloat f)



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
   go (Call (Fun f) e) = go e
   go (App f a)  = go f && go a
   go (Let v2 r b) = go r && (v == v2 || go b)
   go (Lam v2 e)   = v == v2 || go e
   go (Assert e1 e2) = go e1 && go e2

-----------------
newVarNotIn :: Expr -> Var
newVarNotIn e = go e 1 -- FIXME start with hash of e to reduce retries
  where go e n =
          let v = Simple ("_t" ++ show n) in
            if v `notFreeIn` e then
              v
            else
              trace ("newVarNotIn: Var " ++ show v ++ "was bound in E, retry") (
              go e (n + 1))

test_FreeIn () =
  hspec $ do
    let e = Call (Fun (SFun "f")) (Var (Simple "i"))
    let e2 = Call (Fun (SFun "f")) (Var (Simple "_t1"))
    describe "notFreeIn" $ do
      it ("i notFreeIn " ++ show (ppr (e::Expr))) $
        (Simple "i" `notFreeIn` e) `shouldBe` False
      it ("x not notFreeIn " ++ show (ppr (e::Expr))) $
        (Simple "x" `notFreeIn` e) `shouldBe` True
    describe "newVarNotIn" $ do
      it "not in, so new var is _t1..." $
        newVarNotIn e `shouldBe` (Simple "_t1")
      it "in, so new var is _t2..." $
        newVarNotIn e2 `shouldBe` (Simple "_t2")

------ Equality modulo alpha --------

instance Eq Expr where
  e1 == e2 = case e1 `cmpExpr` e2 of
               EQ -> True
               _  -> False

instance Ord Expr where
  compare = cmpExpr

thenCmp :: Ordering -> Ordering -> Ordering
EQ `thenCmp` o = o
o  `thenCmp` _ = o

cmpExpr :: Expr -> Expr -> Ordering
cmpExpr e1 e2
 = go e1 M.empty e2
 where
   go :: Expr -> Map Var Var -> Expr -> Ordering
   go (Konst k1) subst e2
     = case e2 of
         Konst k2 -> k1 `compare` k2
         _        -> LT

   go (Var v1) subst e2
     = case e2 of
         Konst {} -> GT
         Var v2   -> v1 `compare` M.findWithDefault v2 v2 subst
         _        -> LT

   go (Call f1 e1) subst e2
     = case e2 of
         Konst {} -> GT
         Var {} -> GT
         Call f2 e2 -> (f1 `compare` f2) `thenCmp`
                       (go e1 subst e2)
         _ -> LT

   go (Tuple es1) subst e2
     = case e2 of
         Konst {} -> GT
         Var {}  -> GT
         Call {} -> GT
         Tuple es2 -> gos es1 subst es2
         _        -> LT

   go (Lam b1 e1) subst e2
      = case e2 of
         Konst {} -> GT
         Var {}   -> GT
         Call {}  -> GT
         Tuple es -> GT
         Lam b2 e2 -> go e1 (M.insert b2 b1 subst) e2
         _         -> LT

   go (App e1a e1b) subst e2
     = case e2 of
         Konst {} -> GT
         Var {}   -> GT
         Call {}  -> GT
         Tuple {} -> GT
         Lam {}   -> GT
         App e2a e2b -> go e1a subst e2a `thenCmp` go e1b subst e2b
         _           -> LT

   go (Let b1 r1 e1) subst e2
     = case e2 of
         If {}     -> LT
         Assert {} -> LT
         Let b2 r2 e2 -> go r1 subst r2 `thenCmp`
                         go e1 (M.insert b2 b1 subst) e2
         _ -> GT

   go (If e1a e1b e1c) subst e2
      = case e2 of
          Assert {} -> LT
          If e2a e2b e2c -> go e1a subst e2a `thenCmp`
                            go e1b subst e2b `thenCmp`
                            go e1c subst e2c
          _ -> GT

   go (Assert e1a e1b) subst e2
      = case e2 of
          Assert e2a e2b -> go e1a subst e2a `thenCmp` go e1b subst e2b
          _              -> GT

   gos [] subst [] = EQ
   gos [] subst (_:_) = LT
   gos (_:_) subst [] = GT
   gos (e1:es1) subst (e2:es2) = go e1 subst e2 `thenCmp` gos es1 subst es2

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
  ppr (KFloat f)   = PP.double f
  ppr KZero        = text "KZero"

type Prec = Int
 -- 0 => no need for parens
 -- high => parenthesise everything

precZero  = 0  -- Base
precOne   = 1  -- ==
precTwo   = 2  -- +
precThree = 3  -- *

instance Pretty Expr where
  ppr expr = pprExpr 0 expr

pprParendExpr :: Expr -> Doc
pprParendExpr = pprExpr precTwo

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
pprExpr p (Assert e1 e2)
  = parensIf p precZero $
    PP.sep [ PP.text "assert" PP.<+> pprParendExpr e1
           , ppr e2 ]

pprExpr _ (App e1 e2)
  = parens (text "App" <+> sep [pprParendExpr e1, pprParendExpr e2])
    -- We aren't expecting Apps, so I'm making them very visible

pprCall :: Prec -> Fun -> Expr -> Doc
pprCall prec f (Tuple [e1,e2])
  | Just prec' <- isInfix f
  = parensIf prec prec' $
    sep [ pprExpr prec' e1, ppr f <+> pprExpr prec' e2 ]

pprCall _ f e = ppr f PP.<> parensSp pp_args
  where
    pp_args = case e of
                Tuple es -> pprWithCommas es
                _        -> ppr e

isInfix :: Fun -> Maybe Prec
isInfix (Fun (SFun s))
  | s == "==" = Just precOne
  | s == "+"  = Just precTwo
  | s == "-"  = Just precTwo
  | s == "*"  = Just precThree
  | s == "/"  = Just precThree
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
    = PP.sep [ PP.text "def" PP.<+> ppr f
                 PP.<> parens (pprWithCommas vs)
             , PP.nest 2 (PP.text "=" PP.<+> ppr rhs) ]


display :: Pretty p => p -> KM ()
display p = liftIO $ putStrLn (PP.render (ppr p))

displayN :: Pretty p => [p] -> KM ()
displayN ps = liftIO $ putStrLn (PP.render (go ps))
  where
    go []     = PP.empty
    go [p]    = ppr p
    go (p:ps) = vcat [ ppr p, text "", go ps ]

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

pprTrace :: String -> Doc -> a -> a
pprTrace str doc v
  = trace (PP.render (PP.sep [PP.text str, PP.nest 2 doc])) v
