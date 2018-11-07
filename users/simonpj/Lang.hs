{-# LANGUAGE FlexibleInstances #-}

module Lang where

import Prelude hiding( (<>) )

import Text.PrettyPrint as PP
import KMonad

import Data.Functor.Identity
import Data.Maybe
import Data.Functor

import System.Console.Haskeline

import qualified Data.Map as M
import Debug.Trace( trace )
import Test.Hspec

------ Debugging utilities ---------
assertEqualThen msg t1 t2 e =
  if t1 == t2 then e else error ("Asserts unequal ["++msg++"] " ++ show t1 ++ " == " ++ show t2)


------ Data types ---------
data Type = TypeZero               -- Polyamorous zero
          | TypeBool 
          | TypeInteger 
          | TypeFloat
          | TypeTuple [Type] 
          | TypeVec Type
          | TypeLambda Type Type   -- Domain -> Range 
          | TypeLM Type Type -- Linear map
          | TypeUnknown
          deriving (Show, Eq, Ord)

data ADMode = Fwd | Rev
            deriving( Eq, Ord, Show )

flipMode :: ADMode -> ADMode
flipMode Fwd = Rev
flipMode Rev = Fwd

data FunId = SelFun     -- Selector function: fst, snd etc
               Int      -- Index; 1-indexed, so (SelFun 1 2) is fst
               Int      -- Arity
           | SFun String  -- For now
           deriving( Eq, Ord, Show )

data Fun = Fun     FunId         -- The function              f(x)
         | GradFun FunId ADMode  -- Full Jacobian Df(x)
                                 --   Rev <=> transposed  Rf(x)
         | DrvFun  FunId ADMode  -- Derivative derivative f'(x,dx)
                                 --   Rev <=> reverse mode f`(x,dr)
         | LMFun   String        -- Linear map
         deriving( Eq, Ord, Show )

data TFun = TFun Type Fun
            deriving( Eq, Ord, Show )

data Var
  = Simple   String         -- x
  | Delta    String         -- The 'dx' or 'dr' argument to fwd
                            -- or backward versions of f
  | Grad     String ADMode  -- \nabla x
                            --   True <=> transposed \bowtie x
  deriving( Show, Eq, Ord )

data TVar = TVar Type Var
            deriving( Show, Eq, Ord )

data Konst = KZero  -- Of any type
           | KInteger Integer
           | KFloat   Double
           | KBool    Bool
           deriving( Eq, Ord, Show )

isKZero :: ExprX -> Bool
isKZero (Konst KZero) = True
isKZero _             = False

data Def = Def TFun [TVar] Expr  -- f x = e

type TExpr ty = Expr
  -- The phantom parameter gives the type of
  -- the expresssion, for documentation purposes

data ExprX
  = Konst Konst
  | Var Var
  | Call TFun Expr         -- f e
  | Tuple [Expr]            -- (e1, ..., en)
  | Lam TVar Expr
  | App Expr Expr
  | Let TVar Expr Expr    -- let x = e1 in e2  (non-recursive)
  | If Expr Expr Expr 
  | Assert Expr Expr
  deriving( Show )

data Expr = Expr Type ExprX 
  deriving(Eq, Ord, Show)

typeOf :: Expr -> Type
typeOf (Expr ty _) = ty

bodyOf :: Expr -> ExprX
bodyOf (Expr _ e) = e

-- mkInfixCall :: Fun -> Expr -> Expr -> Expr
-- mkInfixCall f a b = Call f (Tuple [a, b])

mkTFun :: Type -> String -> TFun
mkTFun ty fname = TFun ty (Fun (SFun fname))

mkTuple :: [Expr] -> Expr
mkTuple es  = Expr (TypeTuple $ map typeOf es) $ Tuple es

mkCall ty fname [e] = Expr ty $ Call (mkTFun ty fname) e
mkCall ty fname es = Expr ty $ Call (mkTFun ty fname) (mkTuple es)

mkLet :: TVar -> Expr -> Expr -> Expr
mkLet v@(TVar ty _) e1@(Expr ty1 _) e2@(Expr ty2 _) = assertEqualThen "mkLet" ty ty1 $ Expr ty2 $ Let v e1 e2

mkLets :: [(TVar,Expr)] -> Expr -> Expr
mkLets [] e = e
mkLets ((v,e1):bs) e2 = mkLet v e1 (mkLets bs e2)

mkSCall1 :: Type -> String -> Expr -> Expr
mkSCall1 ty fname a = mkCall ty fname [a]

mkSCall2 :: Type -> String -> Expr -> Expr -> Expr
mkSCall2 ty fname a b = mkCall ty fname [a, b]

mkSCall3 :: Type -> String -> Expr -> Expr -> Expr -> Expr
mkSCall3 ty fname a b c = mkCall ty fname [a, b, c]

kInt :: Integer -> Expr
kInt i = Expr TypeInteger $ Konst (KInteger i)

kFloat :: Double -> Expr
kFloat f = Expr TypeFloat $ Konst (KFloat f)

{-
infixr 0 `seqExpr`

seqExprX :: ExprX -> a -> a
seqExprX (Var v) x = v `seq` x
seqExprX (Call fun e) x = fun `seq` e `seqExprX` x
seqExprX (Konst k) x = k `seq` x
seqExprX (Let v r b) x = v `seq` r `seqExprX` b `seqExprX` x
seqExprX (Tuple es) x = Prelude.foldr seqExprX x es

seqExpr:: Expr -> a -> a
seqExpr (Expr ty e) = ty `seq` e
-}

------ Equality modulo alpha --------

instance Eq ExprX where
  e1 == e2 = case e1 `cmpExpr` e2 of
               EQ -> True
               _  -> False

instance Ord ExprX where
  compare = cmpExpr

thenCmp :: Ordering -> Ordering -> Ordering
EQ `thenCmp` o = o
o  `thenCmp` _ = o

cmpExpr :: ExprX -> ExprX -> Ordering
cmpExpr e1 e2
 = go e1 M.empty e2
 where
   go :: ExprX -> M.Map Var Var -> ExprX -> Ordering
   go (Konst k1) subst e2
     = case e2 of
         Konst k2 -> k1 `compare` k2
         _        -> LT

   go (Var v1) subst e2
     = case e2 of
         Konst {} -> GT
         Var v2   -> v1 `compare` M.findWithDefault v2 v2 subst
         _        -> LT

   go (Call f1 (Expr _ e1)) subst e2
     = case e2 of
         Konst {} -> GT
         Var {} -> GT
         Call f2 (Expr _ e2) -> (f1 `compare` f2) `thenCmp` (go e1 subst e2)
         _ -> LT

   go (Tuple es1) subst e2
     = case e2 of
         Konst {} -> GT
         Var {}  -> GT
         Call {} -> GT
         Tuple es2 -> gos es1 subst es2
         _        -> LT

   go (Lam (TVar _ b1) (Expr _ e1)) subst e2
      = case e2 of
         Konst {} -> GT
         Var {}   -> GT
         Call {}  -> GT
         Tuple es -> GT
         Lam (TVar _ b2) (Expr _ e2) -> go e1 (M.insert b2 b1 subst) e2
         _         -> LT

   go (App (Expr _ e1a) (Expr _ e1b)) subst e2
     = case e2 of
         Konst {} -> GT
         Var {}   -> GT
         Call {}  -> GT
         Tuple {} -> GT
         Lam {}   -> GT
         App (Expr _ e2a) (Expr _ e2b) -> go e1a subst e2a `thenCmp` go e1b subst e2b
         _           -> LT

   go (Let (TVar _ b1) (Expr _ r1) (Expr _ e1)) subst e2
     = case e2 of
         If {}     -> LT
         Assert {} -> LT
         Let (TVar _ b2) (Expr _ r2) (Expr _ e2) -> 
                go r1 subst r2 `thenCmp` go e1 (M.insert b2 b1 subst) e2
         _ -> GT

   go (If (Expr _ e1c) (Expr _ e1t) (Expr _ e1f)) subst e2
      = case e2 of
          Assert {} -> LT
          If (Expr _ e2c) (Expr _ e2t) (Expr _ e2f) -> go e1c subst e2c `thenCmp`
                                                       go e1t subst e2t `thenCmp`
                                                       go e1f subst e2f
          _ -> GT

   go (Assert (Expr _ e1a) (Expr _ e1b)) subst e2
      = case e2 of
          Assert (Expr _ e2a) (Expr _ e2b) -> go e1a subst e2a `thenCmp` go e1b subst e2b
          _              -> GT

   gos [] subst [] = EQ
   gos [] subst (_:_) = LT
   gos (_:_) subst [] = GT
   gos (Expr _ e1:es1) subst (Expr _ e2:es2) = go e1 subst e2 `thenCmp` gos es1 subst es2


--------------
notFreeIn :: Var -> Expr -> Bool
notFreeIn v e = go e
 where
   go::Expr -> Bool
   go (Expr _ e) = goX e
   goX::ExprX -> Bool
   goX (Var v2) = v /= v2
   goX (Konst _) = True
   goX (Tuple es) = all go es
   goX (If b t e) = go b && go t && go e
   goX (Call _ e) = go e
   goX (App f a)  = go f && go a
   goX (Let (TVar _ v2) r b) = go r && (v == v2 || go b)
   goX (Lam (TVar _ v2) e)   = v == v2 || go e
   goX (Assert e1 e2) = go e1 && go e2

-----------------
newVarNotIn :: Expr -> Var
newVarNotIn e = go e 1 -- FIXME start with hash of e to reduce retries
  where go e n =
          let v = Simple ("_t" ++ show n) in
            if v `notFreeIn` e then
              v
            else
              trace ("newVarNotIn: Var " ++ show v ++ " was bound in E, retry") (
              go e (n + 1))

test_FreeIn =
  hspec $ do
    let ty = TypeFloat
    let var s = Expr ty (Var (Simple s))
    let e = mkSCall1 ty "f" (var "i")
    let e2 = mkSCall2 ty "f" (var "_t1") (kInt 5)
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

------ Pretty printer ------
class Pretty p where
  ppr :: p -> Doc

instance Pretty Var where
  ppr (Simple s)   = PP.text s
  ppr (Delta s)    = PP.text ('d' : s)
  ppr (Grad s Fwd) = PP.text ('D' : s)
  ppr (Grad s Rev) = PP.text ('R' : s)

instance Pretty TVar where
  ppr (TVar ty v)  = parens (ppr v PP.<> PP.text " : " PP.<> PP.text (show ty))

instance Pretty FunId where
  ppr (SFun s)     = PP.text s
  ppr (SelFun i n) = PP.text "sel_" PP.<> PP.int i PP.<> PP.char '_' PP.<> PP.int n

instance Pretty Fun where
  ppr (Fun s)           = ppr s
  ppr (GradFun s Fwd) = PP.char 'D' PP.<> ppr s
  ppr (GradFun s Rev) = PP.char 'R' PP.<> ppr s
  ppr (DrvFun s Fwd)  = ppr s PP.<> PP.char '\''
  ppr (DrvFun s Rev)  = ppr s PP.<> PP.char '`'
  ppr (LMFun s)   = PP.text s

instance Pretty TFun where
  ppr (TFun ty f) = ppr f

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

instance Pretty ExprX where
  ppr expr = pprExprX 0 expr

instance Pretty Expr where
  ppr (Expr ty expr) = ppr expr

pprParendExpr :: Expr -> Doc
pprParendExpr = pprExpr precTwo

pprExpr :: Prec -> Expr -> Doc
pprExpr p (Expr _ e) = pprExprX p e

pprExprX :: Prec -> ExprX -> Doc
pprExprX _  (Var v)   = ppr v
pprExprX _ (Konst k)  = ppr k
pprExprX p (Call f e) = pprCall p f e
pprExprX _ (Tuple es) = parens (pprWithCommas es)
pprExprX p (Lam v e)  = parensIf p precZero $
                       PP.char '\\' <> ppr v <> PP.char '.' <+> ppr e
pprExprX p (Let v e1 e2)
  = parensIf p precZero $
    PP.vcat [ PP.text "let" PP.<+>
                (bracesSp $ PP.sep [ ppr v
                                   , PP.nest 2 (PP.text "=" PP.<+> ppr e1) ])
           , ppr e2 ]
pprExprX p (If e1 e2 e3)
  = parensIf p precZero $
    PP.sep [ PP.text "if" PP.<+> ppr e1
           , PP.text "then" PP.<+> ppr e2
           , PP.text "else" PP.<+> ppr e3 ]
pprExprX p (Assert e1 e2)
  = parensIf p precZero $
    PP.sep [ PP.text "assert" PP.<+> pprParendExpr e1
           , ppr e2 ]

pprExprX _ (App e1 e2)
  = parens (text "App" <+> sep [pprParendExpr e1, pprParendExpr e2])
    -- We aren't expecting Apps, so I'm making them very visible

pprCall :: Prec -> TFun -> Expr -> Doc
pprCall prec (TFun _ f) (Expr _ (Tuple [e1,e2]))
  | Just prec' <- isInfix f
  = parensIf prec prec' $
    sep [ pprExpr prec' e1, ppr f <+> pprExpr prec' e2 ]

pprCall _ f e = PP.cat [ppr f, nest 2 (parensSp pp_args)]
  where
    pp_args = case e of
                (Expr _ (Tuple es)) -> pprWithCommas es
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

pprPanic :: String -> Doc -> a
pprPanic str doc
  = error (PP.render (PP.sep [PP.text str, PP.nest 2 doc]))

test_Pretty =
  hspec $ do
    let ty = TypeFloat
    let var s = Expr ty (Var (Simple s))
    let e = mkSCall1 ty "g" (var "i")
    let e2 = mkSCall3 ty "f" e (var "_t1") (kInt 5)
    describe "Pretty" $ do
      it "ppr" $
        (show $ ppr e) `shouldBe` "g( i )"
      it "ppr" $
        (show $ ppr e2) `shouldBe` "f( g( i ), _t1, 5 )"
