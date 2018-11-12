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
assertEqual msg t1 t2 =
  if t1 /= t2 then error ("Asserts unequal ["++msg++"] " ++ show t1 ++ " == " ++ show t2) else ()

assertEqualThen :: HasCallStack => Eq a => Show a => String -> a -> a -> b -> b
assertEqualThen msg t1 t2 e =
  if t1 == t2 then e else error ("Asserts unequal ["++msg++"] \n T1 = " ++ show t1 ++ "\n T2 = " ++ show t2 ++ "\n")

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
           deriving( Eq, Ord )

instance Show FunId where
  show (SFun s) = s
  show (SelFun i n) = "selfun<" ++ show i ++ "," ++ show n ++ ">"

data Fun = Fun     FunId         -- The function              f(x)
         | GradFun FunId ADMode  -- Full Jacobian Df(x)
                                 --   Rev <=> transposed  Rf(x)
         | DrvFun  FunId ADMode  -- Derivative derivative f'(x,dx)
                                 --   Rev <=> reverse mode f`(x,dr)
         | LMFun   String        -- Linear map
         deriving( Eq, Ord )

instance Show Fun where
  show (Fun s) = show s
  show (GradFun s Fwd) = "D$" ++ show s
  show (GradFun s Rev) = "R$" ++ show s
  show (DrvFun  s Fwd) = "fwd$" ++ show s
  show (DrvFun  s Rev) = "rev$" ++ show s
  show (LMFun s) = "LM$" ++ s

data TFun = TFun Type Fun
  deriving (Eq, Ord)

instance Show TFun where
  show (TFun ty f) = show f ++ "<" ++ show ty ++ ">"

data Var
  = Simple   String         -- x
  | Delta    String         -- The 'dx' or 'dr' argument to fwd
                            -- or backward versions of f
  | Grad     String ADMode  -- \nabla x
                            --   True <=> transposed \bowtie x
  deriving( Eq, Ord )

instance Show Var where
  show v = case v of
    Simple s -> "s$" ++ s
    Delta  d -> "d$" ++ d
    Grad g m ->
      "g"
      ++ (case m of
            Fwd -> "f"
            Rev -> "r")
      ++ "$"
      ++ g


data TVar b = TVar Type b
  deriving( Show, Eq, Ord )

data Konst = KZero  -- Of any type
           | KInteger Integer
           | KFloat   Double
           | KBool    Bool
           deriving( Eq, Ord, Show )

isKZero :: ExprX f b -> Bool
isKZero (Konst KZero) = True
isKZero _             = False

data DefX f b = DefX f [TVar Var] (ExprX f b)  -- f x = e
type Def = DefX Fun Var
type TDef = DefX TFun (TVar Var)

data ExprX f b
  = Konst Konst
  | Var b
  | Call f (ExprX f b)      -- f e
  | Tuple [ExprX f b]            -- (e1, ..., en)
  | Lam b Type (ExprX f b)
  | App (ExprX f b) (ExprX f b)
  | Let b (ExprX f b) (ExprX f b)    -- let x = e1 in e2  (non-recursive)
  | If (ExprX f b) (ExprX f b) (ExprX f b)  -- FIXME make cond ExprX?
  | Assert (ExprX f b) (ExprX f b)
  deriving( Show )

type Expr = ExprX Fun Var
type TExpr = ExprX TFun (TVar Var)

class TypeableFun f where
  typeofFun :: f -> Type -> Type

instance TypeableFun Fun where
  typeofFun v arg = TypeUnknown

instance TypeableFun TFun where
  typeofFun (TFun ty f) argtype = ty 

class Typeable b where
  typeof :: b -> Type

instance Typeable Var where
  typeof v = TypeUnknown

instance Typeable (TVar b) where
  typeof (TVar ty _) = ty

instance (Typeable b, TypeableFun f) => 
         Typeable (ExprX f b) where
  typeof (Konst k) = typeofKonst k
  typeof (Var b) = typeof b
  typeof (Call f e) = typeofFun f (typeof e)
  typeof (App f e) = TypeUnknown
  typeof (Tuple es) = TypeTuple $ map typeof es
  typeof (Lam v tyv e) = TypeLambda tyv $ typeof e
  typeof (Let b e1 e2) = typeof e2
  typeof (If c t f) = typeof t
  typeof (Assert c e) = typeof e

typeofKonst :: Konst -> Type
typeofKonst KZero = TypeZero
typeofKonst (KInteger _) = TypeInteger
typeofKonst (KFloat _) = TypeFloat
typeofKonst (KBool _) = TypeBool

typeofLMDst (TypeLM _ dst) = dst
typeofLMSrc (TypeLM src _) = src

typeofDst :: Typeable e => e -> Type
typeofDst = typeofLMDst . typeof
typeofSrc :: Typeable e => e -> Type
typeofSrc = typeofLMSrc . typeof

-- mkInfixCall :: Fun -> Expr -> Expr -> Expr
-- mkInfixCall f a b = Call f (Tuple [a, b])

mkFun :: String -> Fun
mkFun fname = Fun (SFun fname)

mkTFun :: Type -> String -> TFun
mkTFun ty fname = TFun ty $ mkFun fname

mkCall :: String->[Expr]->Expr
mkCall fname [e] = Call (mkFun fname) e
mkCall fname es = Call (mkFun fname) (mkTuple es)

mkTCall :: Type -> Fun -> [TExpr] -> TExpr
mkTCall ty f [e] = Call (TFun ty f) e
mkTCall ty f es = Call (TFun ty f) (mkTuple es)

mkTuple :: [ExprX f b] -> ExprX f b
mkTuple [e] = e
mkTuple es = Tuple es

mkSCall1 :: String -> Expr -> Expr
mkSCall1 fname a = mkCall fname [a]

mkSCall2 :: String -> Expr -> Expr -> Expr
mkSCall2 fname a b = mkCall fname [a, b]

mkSCall3 :: String -> Expr -> Expr -> Expr -> Expr
mkSCall3 fname a b c = mkCall fname [a, b, c]

mkTCall1 :: Type -> Fun -> TExpr -> TExpr
mkTCall1 ty f a = mkTCall ty f [a]

mkTCall2 :: Type -> Fun -> TExpr -> TExpr -> TExpr
mkTCall2 ty f a b = mkTCall ty f [a, b]

mkTCall3 :: Type -> Fun -> TExpr -> TExpr -> TExpr -> TExpr
mkTCall3 ty f a b c = mkTCall ty f [a, b, c]

mkLets :: [(TVar Var,TExpr)] -> TExpr -> TExpr
mkLets [] e = e
mkLets ((TVar ty v,rhs):bs) e = Let (TVar ty v) rhs (mkLets bs e)

kInt :: Integer -> Expr
kInt i = Konst (KInteger i)

kTInt :: Integer -> TExpr
kTInt i = Konst (KInteger i)

kFloat :: Double -> Expr
kFloat f = Konst (KFloat f)

kTFloat :: Double -> TExpr
kTFloat f = Konst (KFloat f)

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

instance (Ord f, Ord b) => Eq (ExprX f b) where
  e1 == e2 = case e1 `cmpExpr` e2 of
               EQ -> True
               _  -> False

instance (Ord f, Ord b) => Ord (ExprX f b) where
  compare = cmpExpr

thenCmp :: Ordering -> Ordering -> Ordering
EQ `thenCmp` o = o
o  `thenCmp` _ = o

cmpExpr :: (Ord f, Ord b) => ExprX f b -> ExprX f b -> Ordering
cmpExpr e1 e2
 = go e1 M.empty e2
 where
   go :: (Ord f, Ord b) => ExprX f b -> M.Map b b -> ExprX f b -> Ordering
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
         Call f2 e2 -> (f1 `compare` f2) `thenCmp` (go e1 subst e2)
         _ -> LT

   go (Tuple es1) subst e2
     = case e2 of
         Konst {} -> GT
         Var {}  -> GT
         Call {} -> GT
         Tuple es2 -> gos es1 subst es2
         _        -> LT

   go (Lam b1 _ e1) subst e2
      = case e2 of
         Konst {} -> GT
         Var {}   -> GT
         Call {}  -> GT
         Tuple es -> GT
         Lam b2 _ e2 -> go e1 (M.insert b2 b1 subst) e2
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
         Let b2 r2 e2 -> 
                go r1 subst r2 `thenCmp` go e1 (M.insert b2 b1 subst) e2
         _ -> GT

   go (If e1c e1t e1f) subst e2
      = case e2 of
          Assert {} -> LT
          If e2c e2t e2f -> go e1c subst e2c `thenCmp`
                            go e1t subst e2t `thenCmp`
                            go e1f subst e2f
          _ -> GT

   go (Assert e1a e1b) subst e2
      = case e2 of
          Assert e2a e2b -> go e1a subst e2a `thenCmp` go e1b subst e2b
          _              -> GT

   gos :: (Ord f, Ord b) => [ExprX f b] -> M.Map b b -> [ExprX f b] -> Ordering
   gos [] subst [] = EQ
   gos [] subst (_:_) = LT
   gos (_:_) subst [] = GT
   gos (e1:es1) subst (e2:es2) = go e1 subst e2 `thenCmp` gos es1 subst es2


--------------
notFreeIn :: Eq b => b -> ExprX f b -> Bool
notFreeIn v e = go v e
 where
   go:: Eq b => b -> ExprX f b -> Bool
   go v (Var v2) = v /= v2
   go v (Konst _) = True
   go v (Tuple es) = all (go v) es
   go v (If b t e) = go v b && go v t && go v e
   go v (Call _ e) = go v e
   go v (App f a)  = go v f && go v a
   go v (Let v2 r b) = go v r && (v == v2 || go v b)
   go v (Lam v2 _ e)   = v == v2 || go v e
   go v (Assert e1 e2) = go v e1 && go v e2

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

newVarNotInT :: Type -> TExpr -> TVar Var
newVarNotInT ty e = go ty e 1 -- FIXME start with hash of e to reduce retries
  where go ty e n =
          let v = TVar ty $ Simple ("_t" ++ show n) in
            if v `notFreeIn` e then
              v
            else
              trace ("newVarNotIn: Var " ++ show v ++ " was bound in E, retry") (
              go ty e (n + 1))

test_FreeIn =
  hspec $ do
    let var s = Var (Simple s)
    let e = mkSCall1 "f" (var "i")
    let e2 = mkSCall2 "f" (var "_t1") (kInt 5)
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
  ppr v   = PP.text $ show v

instance Pretty b => Pretty (TVar b) where
  ppr (TVar ty v)  = parens (ppr v PP.<> PP.text " : " PP.<> ppr ty)

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
  ppr (TFun ty f) = parens (ppr f PP.<> PP.text " : " PP.<> ppr ty)

instance Pretty Konst where
  ppr (KInteger i) = PP.integer i
  ppr (KFloat f)   = PP.double f
  ppr KZero        = text "KZero"

instance Pretty Type where
  ppr (TypeVec ty) = PP.text "(Vec " PP.<> ppr ty PP.<> PP.text ")" 
  ppr (TypeTuple tys) = PP.text "(Tuple (" PP.<> pprWithCommas tys PP.<> PP.text "))" 
  ppr (TypeLambda from to) = PP.text "(Lambda " PP.<> ppr from PP.<> PP.text " -> " PP.<> ppr to PP.<> PP.text ")" 
  ppr (TypeLM from to) = PP.text "(LM " PP.<> ppr from PP.<> PP.text " -o " PP.<> ppr to PP.<> PP.text ")" 
  ppr TypeZero = PP.text "zero_t"
  ppr TypeFloat = PP.text "Float"
  ppr TypeInteger = PP.text "Integer"
  ppr TypeBool = PP.text "Bool"
  ppr TypeUnknown = PP.text "UNKNOWN"

type Prec = Int
 -- 0 => no need for parens
 -- high => parenthesise everything

precZero  = 0  -- Base
precOne   = 1  -- ==
precTwo   = 2  -- +
precThree = 3  -- *

instance (HasInfix f, Pretty f, Pretty b) => Pretty (ExprX f b) where
  ppr expr = pprExpr 0 expr

pprParendExpr :: (HasInfix f, Pretty f, Pretty b) =>
                ExprX f b -> Doc
pprParendExpr = pprExpr precTwo

pprExpr :: (HasInfix f, Pretty f, Pretty b) => Prec -> ExprX f b -> Doc
pprExpr _  (Var v)   = ppr v
pprExpr _ (Konst k)  = ppr k
pprExpr p (Call f e) = pprCall p f e
pprExpr _ (Tuple es) = parens (pprWithCommas es)
pprExpr p (Lam v ty e)  = parensIf p precZero $
                      PP.text "(lam (" <> ppr v <> PP.text " : " <> ppr ty <> text ") " <+> ppr e <> PP.char ')'
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

pprCall :: (Pretty f, Pretty b, HasInfix f) => Prec -> f -> ExprX f b -> Doc
pprCall prec f (Tuple [e1,e2])
  | Just prec' <- isInfix f
  = parensIf prec prec' $
    sep [ pprExpr prec' e1, ppr f <+> pprExpr prec' e2 ]

pprCall _ f e = PP.cat [ppr f, nest 2 (parensSp pp_args)]
  where
    pp_args = case e of
                (Tuple es) -> pprWithCommas es
                _        -> ppr e

class HasInfix f where
  isInfix :: f -> Maybe Prec

instance HasInfix TFun where
  isInfix (TFun _ f) = isInfix f

instance HasInfix Fun where
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

instance (Pretty f, Pretty b, HasInfix f) => Pretty (DefX f b) where
  ppr (DefX f vs rhs)
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
    let test e s = it s $ (show $ ppr e) `shouldBe` s

    let var s = Var (Simple s)
    let e = mkSCall1 "g" (var "i")
    let e2 = mkSCall3 "f" e (var "_t1") (kInt 5)

    describe "Pretty" $ do
      test e "g( i )"
      test e2 "f( g( i ), _t1, 5 )"
