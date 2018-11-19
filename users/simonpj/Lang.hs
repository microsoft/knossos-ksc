{-# LANGUAGE FlexibleInstances,LambdaCase #-}

module Lang where

import Prelude hiding( (<>) )

import Data.List ( intercalate )
import qualified Data.Map                      as Map
import Text.PrettyPrint as PP
import KMonad

import Data.Functor.Identity
import Data.Maybe
import Data.Functor

import System.Console.Haskeline

import qualified Data.Map as M
import Debug.Trace( trace )
import Test.Hspec

foldll :: (a -> x -> (a,x')) -> a -> [x] -> (a,[x'])
foldll f a0 xs = foldl (\(a,xs) x -> let (a',x') = f a x in (a', xs ++ [x'])) (a0,[]) xs


------ Debugging utilities ---------
class Match a where
  isMatch :: a -> a -> Bool

instance Match Int where
  isMatch a b = a == b

instance Match TypeLM where
  isMatch (LM s t) lm = -- dbtrace("[isMatch LM]") $ 
                        s == typeofSrc lm && t == typeofDst lm
  isMatch lm1 lm2 = lm1 == lm2

instance Match Type where
  isMatch (TypeLM t1) (TypeLM t2) = isMatch t1 t2
  isMatch t1 t2 = t1 == t2

assertEqual msg t1 t2 =
  assertEqualThen msg t1 t2 ()

assertEqualThen :: HasCallStack => Match a => Show a => String -> a -> a -> b -> b
assertEqualThen msg t1 t2 e =
  if isMatch t1 t2 then e else trace ("Asserts unequal ["++msg++"] \n T1 = " ++ show t1 ++ "\n T2 = " ++ show t2 ++ "\n") $ e

assertAllEqualThen :: HasCallStack => Eq a => Show a => String -> [a] -> b -> b
assertAllEqualThen msg es e =
  if allEq es then e else
     flip trace e $ ("Assert failed: ["++msg++"] not all equal  \n " ++ show es ++ "\n")
  where
    allEq [] = True
    allEq (a:as) = allEqa a as

    allEqa a0 [] = True
    allEqa a0 [a] = a0 == a
    allEqa a0 (a:as) = a0 == a && allEqa a0 as

assertAllEqualRet :: HasCallStack => Eq a => Show a => String -> [a] -> a
assertAllEqualRet msg (e:es) = assertAllEqualThen msg (e:es) e

typeofCheckLM :: HasCallStack => TypeLM -> TExpr -> TypeLM
typeofCheckLM tylm e = 
  let ty = typeof e in
  case ty of
  TypeLM tylm1 ->
     assertEqualThen "CheckLM" (typeofSrc tylm) (typeofSrc tylm1) $ 
     assertEqualThen "CheckLM" (typeofDst tylm) (typeofDst tylm1) $ 
     tylm
  _ -> error $ "Bad LM type"


------ Data types ---------
data Type = TypeZero               -- Polyamorous zero
          | TypeBool 
          | TypeInteger 
          | TypeFloat
          | TypeTuple [Type] 
          | TypeVec Type
          | TypeLambda Type Type   -- Domain -> Range 
          | TypeLM TypeLM  -- Linear map
          | TypeUnknown
          deriving (Show, Eq, Ord)

data TypeLM
          = LM Type Type             -- Generic linear map
          | LMZero Type Type         -- Zeros may be rectangular 
          | LMOne Type               -- Ones must be square
          | LMScale Type             -- Scalar will always be of TypeFloat, Type is the type being scaled
          | LMSelFun Type Type
          | LMTranspose TypeLM
          | LMCompose TypeLM TypeLM
          | LMAdd TypeLM TypeLM
          | LMVCat [TypeLM]
          | LMHCat [TypeLM]
          | LMVariant [TypeLM]        -- One of the arg types, to be selected by another boolean
          | LMBuild TypeLM
          | LMBuildT TypeLM
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
  = Dummy
  | Simple   String         -- x
  | Delta    String         -- The 'dx' or 'dr' argument to fwd
                            -- or backward versions of f
  | Grad     String ADMode  -- \nabla x
                           --   True <=> transposed \bowtie x
  deriving( Eq, Ord )

instance Show Var where
  show v = case v of
    Dummy -> "/*dummy*/"
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

class CmkVar b where
  mkVar :: Type -> String -> b

instance CmkVar Var where
  mkVar ty s = Simple s

instance CmkVar (TVar Var) where
  mkVar ty s = TVar ty $ Simple s

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
       -- ToDo: Simon says: what is this Type?

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
  typeof (If c t f) = makeUnionType (typeof t) (typeof f)
  typeof (Assert c e) = typeof e


makeUnionType :: Type -> Type -> Type
makeUnionType (TypeLM lm1) (TypeLM lm2) = TypeLM $ if lm1 == lm2 then lm1 else LMVariant [lm1, lm2]
makeUnionType t1 t2 = assertEqualThen "makeUnionType" t1 t2 $ 
                      t1

typeofKonst :: Konst -> Type
typeofKonst KZero = TypeZero
typeofKonst (KInteger _) = TypeInteger
typeofKonst (KFloat _) = TypeFloat
typeofKonst (KBool _) = TypeBool

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

mkLet :: HasCallStack => TVar Var -> TExpr -> TExpr -> TExpr
mkLet (TVar ty v) rhs body
  = assertEqualThen ("mkLet " ++ show v ++ " = " ++ (show $ ppr rhs))
                    ty (typeof rhs) $
    Let (TVar ty v) rhs body

mkLets :: HasCallStack => [(TVar Var,TExpr)] -> TExpr -> TExpr
mkLets [] e = e
mkLets ((v,rhs):bs) e = mkLet v rhs (mkLets bs e)

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


------ Substitution --------

substE :: Ord b => M.Map b (ExprX f b) -> ExprX f b -> ExprX f b
substE subst (Konst k)      = Konst k
substE subst (Var v)        = case M.lookup v subst of
                               Just e  -> e
                               Nothing -> Var v
substE subst (Call f e)     = Call f (substE subst e)
substE subst (If b t e)     = If (substE subst b) (substE subst t) (substE subst e)
substE subst (Tuple es)     = Tuple (map (substE subst) es)
substE subst (App e1 e2)    = App (substE subst e1) (substE subst e2)
substE subst (Assert e1 e2) = Assert (substE subst e1) (substE subst e2)
substE subst (Lam v ty e)   = Lam v ty (substE (v `M.delete` subst) e)
substE subst (Let v r b)    = Let v (substE subst r) $
                              substE (v `M.delete` subst) b

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

newVarNotIn :: (CmkVar b, Eq b, Show b) => Type -> ExprX f b -> b
newVarNotIn ty e = go ty e 1 -- FIXME start with hash of e to reduce retries
  where go ty e n =
          let v = mkVar ty ("_t" ++ show n) in
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
        newVarNotIn TypeFloat e `shouldBe` (Simple "_t1")
      it "in, so new var is _t2..." $
        newVarNotIn TypeFloat e2 `shouldBe` (Simple "_t2")

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
  ppr (TypeLM ty) = PP.text "(LM " PP.<> ppr ty PP.<> PP.text ")" 
  ppr TypeZero = PP.text "zero_t"
  ppr TypeFloat = PP.text "Float"
  ppr TypeInteger = PP.text "Integer"
  ppr TypeBool = PP.text "Bool"
  ppr TypeUnknown = PP.text "UNKNOWN"

instance Pretty TypeLM where
  ppr lm = PP.text (nameOf lm) <> text " " <> ppr (typeofSrc lm) <> text " -o " <> ppr (typeofDst lm)

------------------------- TypeLM methods ------------------------

getLM :: HasCallStack => Type -> TypeLM
getLM (TypeLM ty) = ty
getLM t = error $ "Wanted TypeLM, got " ++ (show $ ppr t)


nameOfType :: TypeLM -> String
nameOfType (LM s t) = "LM**"
nameOfType (LMZero s t) = "LMZero"
nameOfType (LMOne t) = "LMOne"
nameOfType (LMScale t) = "LMScale"
nameOfType (LMSelFun s t) = "LMSelFun"
nameOfType (LMTranspose lm) = "LMTranspose<" ++ nameOf lm ++ ">"
nameOfType (LMCompose bc ab) = "LMCompose<" ++ nameOf bc ++ "," ++ nameOf ab ++ ">"
nameOfType (LMAdd a b) = "LMAdd<" ++ nameOf a ++ "," ++ nameOf b ++ ">"
nameOfType (LMVCat lms) = "LMVCat<" ++ intercalate "," (map nameOf lms) ++ ">"
nameOfType (LMHCat lms) = "LMHCat<" ++ intercalate "," (map nameOf lms) ++ ">"
nameOfType (LMBuild lm) = "LMBuild<" ++ nameOf lm ++ ">"
nameOfType (LMBuildT lm) = "LMBuildT<" ++ nameOf lm ++ ">"
nameOfType (LMVariant lms) = "LMVariant<" ++ intercalate "," (map nameOf lms) ++ ">"

nameOf :: TypeLM -> String
nameOf (LM s t) = "lmBase"
nameOf (LMZero s t) = "lmZero"
nameOf (LMOne t) = "lmOne"
nameOf (LMScale t) = "lmScale"
nameOf (LMSelFun s t) = "lmSelFun"
nameOf (LMTranspose lm) = "lmTranspose"
nameOf (LMCompose bc ab) = "lmCompose"
nameOf (LMAdd a b) = "lmAdd"
nameOf (LMVCat lms) = "lmVCat"
nameOf (LMHCat lms) = "lmHCat"
nameOf (LMBuild lm) = "lmBuild"
nameOf (LMBuildT lm) = "lmBuildT"
nameOf (LMVariant lms) = "lmVariant"

typeofSrc :: TypeLM -> Type
typeofSrc (LM s t) = s
typeofSrc (LMZero s t) = s
typeofSrc (LMOne t) = t
typeofSrc (LMScale t) = t
typeofSrc (LMSelFun s t) = s
typeofSrc (LMTranspose lm) = typeofDst lm
typeofSrc (LMCompose bc ab) = typeofSrc ab
typeofSrc (LMAdd a b) = typeofSrc a
typeofSrc (LMVCat (lm:lms)) = typeofSrc lm
typeofSrc (LMHCat lms) = TypeTuple $ map typeofSrc lms
typeofSrc (LMBuild lm) = typeofSrc lm
typeofSrc (LMBuildT lm) = TypeVec (typeofSrc lm)
typeofSrc (LMVariant lms) = assertAllEqualRet "lmvariant" (map typeofSrc lms) 

typeofDst :: TypeLM -> Type
typeofDst (LM s t) = t
typeofDst (LMZero s t) = t
typeofDst (LMOne t) = t
typeofDst (LMScale t) = t
typeofDst (LMSelFun s t) = t
typeofDst (LMTranspose lm) = typeofSrc lm
typeofDst (LMCompose bc ab) = typeofDst bc
typeofDst (LMAdd a b) = typeofDst a
typeofDst (LMVCat lms) = TypeTuple $ map typeofDst lms
typeofDst (LMHCat (l:lms)) = typeofDst l
typeofDst (LMBuild lm) = TypeVec (typeofDst lm)
typeofDst (LMBuildT lm) = typeofDst lm
typeofDst (LMVariant lms) = assertAllEqualRet "lmvariant/dst" (map typeofDst lms) 

transpose :: TypeLM -> TypeLM
transpose = \case
    LM s t          -> LM t s
    LMZero s t      -> LMZero t s
    LMOne t         -> LMOne t
    LMScale t       -> LMScale t
    LMSelFun s t    -> LM t s
    LMTranspose lm  -> lm
    LMCompose bc ab -> LMCompose (transpose ab) (transpose bc)
    LMAdd a b       -> LMAdd (transpose a) (transpose b)
    LMVCat lms      -> LMHCat (map transpose lms)
    LMHCat lms      -> LMVCat (map transpose lms)
    LMBuild lm      -> LMBuildT lm
    LMBuildT lm     -> LMBuild lm
    LMVariant lms   -> LMVariant (map transpose lms) 
   
  
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

------------------ ST (SymTab) ------------------

type ST = Map.Map Var Type

sttrace :: String -> a -> a
sttrace _ e = e -- trace msg e

stCreate :: ST
stCreate = Map.empty

stInsert :: Var -> Type -> ST -> ST
stInsert v ty env = sttrace
  ("Inserting " ++ show v ++ " = " ++ show ty ++ " in " ++ show env ++ "\n")
  (Map.insert v ty env)

stInsertFun :: Fun -> Type -> ST -> ST
stInsertFun f = stInsert (Simple $ show f)

stLookup :: HasCallStack => String -> Var -> ST -> Type
stLookup msg v env = case Map.lookup v env of
  Just a  -> a
  Nothing -> error
    ("Couldn't find " ++ show v ++ " in " ++ msg ++ ", env = " ++ show env)
    TypeUnknown

stLookupFun :: HasCallStack => String -> Fun -> ST -> Type
stLookupFun msg f = stLookup msg (Simple $ show f)

------------------------------------------------------------------------------


-- A single place for "domain knowledge" about polymorphic functions -- to be pruned when we get primdefs
typeofFunTy :: HasCallStack => ST -> Fun -> Type -> Type
typeofFunTy env f (TypeTuple tys) = typeofFunTys env f tys
typeofFunTy env f ty              = typeofFunTys env f [ty]

typeofFunTys :: HasCallStack => ST -> Fun -> [Type] -> Type
typeofFunTys env tf tys = 
  case (tf, tys) of
  (LMFun "lmApply",  [TypeLM tylm, s]) -> assertEqualThen "lmApply" (typeofSrc tylm) s $ typeofDst tylm
  (LMFun f, tys) -> TypeLM $ typeofLMFun f tys
  (Fun (SFun "pr")       , _                            ) -> TypeInteger
  (GradFun (SFun "pr") _ , _                            ) -> TypeInteger
  (Fun (SFun "build")    , [_, TypeLambda TypeInteger t]) -> TypeVec t
  (Fun (SFun "index")    , [_, TypeVec t]               ) -> t
  (GradFun (SFun "index") _, [_, TypeVec t]               ) -> TypeLM $ LMHCat [LMZero TypeInteger t, LMBuildT (LMScale t)]
  (Fun (SFun "size" )    , [TypeVec _]                  ) -> TypeInteger
  (GradFun (SFun "size" )   _ , [TypeVec t]                  ) -> TypeLM $ LMZero (TypeVec t) TypeInteger
  (Fun (SFun "sum"  )    , [TypeVec t]                  ) -> t
  (GradFun (SFun "sum") _, [TypeVec t]                  ) -> TypeLM $ LMBuildT (LMOne t)

  (Fun (SFun "to_float") , [TypeInteger]                ) -> TypeFloat
  (GradFun (SFun "to_float"  ) _   , [TypeInteger]                  ) -> TypeLM $ LMZero TypeInteger TypeFloat
  (Fun (SFun "neg"  )    , [t]                          ) -> t
  (Fun (SFun "exp"  )    , [TypeFloat]                  ) -> TypeFloat
  (GradFun (SFun "exp"  ) _   , [TypeFloat]                  ) -> TypeLM $ LMScale TypeFloat
  (Fun (SFun "log"  )    , [TypeFloat]                  ) -> TypeFloat
  (GradFun (SFun "log"  ) _   , [TypeFloat]                  ) -> TypeLM $ LMScale TypeFloat
  (Fun (SFun "*"    )    , [t1, TypeFloat]              ) -> t1
  (Fun (SFun "+"    )    , [t1, t2]                     ) -> t1
  (Fun (SFun "/"    )    , [t1, t2]                     ) -> t1
  (Fun (SFun "*"    )    , [t1, t2]                     ) -> t1
  (Fun (SFun "-"    )    , [t1, t2]                     ) -> t1

  (GradFun (SFun "*") Fwd, [t1, t2]                     ) -> TypeLM $ LMHCat [LMScale t1, LMScale t2]
  (GradFun (SFun "/") Fwd, [t1, t2]                     ) -> TypeLM $ LMHCat [LMScale t1, LMScale t2]
  (GradFun (SFun "-") Fwd, [t1, t2]                     ) -> TypeLM $ LMHCat [LMScale t1, LMScale t2]
  (GradFun (SFun "+") Fwd, [t1, t2]                     ) -> TypeLM $ LMHCat [LMScale t1, LMScale t2]

  -- (GradFun (SFun "*") Fwd, [TypeInteger, t2]                     ) -> TypeLM $ LMZero (TypeTuple [TypeInteger, t2]) TypeInteger
  -- (GradFun (SFun "/") Fwd, [TypeInteger, t2]                     ) -> TypeLM $ LMZero (TypeTuple [TypeInteger, t2]) TypeInteger
  -- (GradFun (SFun "-") Fwd, [TypeInteger, t2]                     ) -> TypeLM $ LMZero (TypeTuple [TypeInteger, t2]) TypeInteger
  -- (GradFun (SFun "+") Fwd, [TypeInteger, t2]                     ) -> TypeLM $ LMZero (TypeTuple [TypeInteger, t2]) TypeInteger

  (Fun (SFun "=="   )    , _                            ) -> TypeBool
  (Fun (SFun "!="   )    , _                            ) -> TypeBool
  (Fun (SFun "<"    )    , _                            ) -> TypeBool
  (Fun (SFun ">"    )    , _                            ) -> TypeBool

  (Fun (SFun "delta")    , [TypeInteger, TypeInteger, t]) -> t

  (Fun (SelFun i _  )    , [TypeTuple tys]              ) -> tys !! (i - 1)
  (Fun (SelFun{})      , [TypeVec t]) -> t
  --fixme(GradFun (SelFun i _) _, [TypeTuple tys]) ->    TypeLM (TypeTuple tys) (tys !! (i - 1))
  --(GradFun (SelFun{}) _, [TypeVec t]) -> TypeLM (TypeVec t) t
  --(GradFun (SelFun{}) _, _          ) -> TypeUnknown

  (f        , _          ) -> case Map.lookup (Simple $ show f) env of
                                  Just a  -> a
                                  Nothing -> error $ "LOOKUP: " ++ emsg

  -- (GradFun (SFun f) _, [tfrom]) -> let ty = stLookup "GradFun" (Simple f) env in TypeLM tfrom ty
  -- (GradFun (SFun f) _, t : tys) ->
  --   let tfrom = TypeTuple (t : tys)
  --   in  let ty = stLookup "GradFun2" (Simple f) env in TypeLM tfrom ty

  where emsg = "Failed to find type for Function\n"
                ++ show tf
                ++ " @ "
                ++ show tys
                ++ ".    Env:\n"
                ++ show env

typeofLMFun :: HasCallStack => String -> [Type] -> TypeLM
typeofLMFun f ty = case --trace ("typeofLMFun " ++ show f ++ " @ " ++ show ty) 
                        (f, ty) of
  ("lmZero" , [s, t])   -> LMZero s t
  ("lmOne" ,  [t])      -> LMOne t
  ("lmScale", [t, TypeFloat]) -> LMScale t
  ("lmBuild", [TypeInteger, TypeLambda TypeInteger (TypeLM ty)]) -> LMBuild ty
  ("lmBuildT", [TypeInteger, TypeLambda TypeInteger (TypeLM ty)]) -> LMBuildT ty
  ("lmVCat",  lms) -> LMVCat $ map getLM lms 
  ("lmHCat",  lms) -> LMHCat $ map getLM lms 
  ("lmCompose", [TypeLM lm1, TypeLM lm2]) -> LMCompose lm1 lm2
  ("lmAdd",  [TypeLM lm1, TypeLM lm2]) -> LMAdd lm1 lm2
  _ ->
    error -- flip trace (LM TypeUnknown TypeUnknown)
      $  "Failed to type LMfun ("
      ++ show f
      ++ ", "
      ++ show ty
      ++ ")"
