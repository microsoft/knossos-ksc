module AD where

import Lang
import Prim
import Text.PrettyPrint as PP


gradF :: Fun -> Fun
gradF (Fun f) = GradFun f Fwd
gradF f       = error ("gradF: bad function: " ++ show f)

gradTF :: TFun -> TFun
gradTF (TFun ty f) = TFun (TypeLM ty ty) $ gradF f

gradV :: Var -> Var
gradV (Simple x) = Grad x Fwd
gradV v          = error ("gradV: bad variable: " ++ PP.render (ppr v))

gradTV :: TVar -> TVar
gradTV (TVar ty v) = TVar (TypeLM ty ty) (gradV v)

gradE :: Expr -> Expr
gradE (Expr ty ex) = gradEX ty ex

gradEX :: Type -> ExprX -> Expr
gradEX TypeInteger _   = lmZero TypeInteger TypeInteger
gradEX ty (Konst k)    = lmZero ty ty
gradEX ty (Var v)         = Expr (TypeLM ty ty) $ Var (gradV v)
gradEX ty (Assert e1 e2)  = mkAssert e1 (gradE e2)

-- grad[ build (\i.e ]
--  = B (\i. let Di = 0 in grad[e])
-- We need the Di binding in case 'i' is mentioned in
-- grad[e], e.g. build (\i. power(x, i))
gradEX tyv (Call (TFun _ (Fun (SFun "build"))) (Expr _ (Tuple [n, Expr (TypeLambda TypeInteger ty) (Lam i b)])))
  = lmBuild n $ mkLam i (mkLet (gradTV i) (lmZero TypeInteger TypeInteger) (gradE b))

gradEX ty (Call (TFun tyf fun) arg@(Expr tyarg _))
  = assertEqualThen "gradEXFun" ty tyf $
    mkCallF (TypeLM tyarg ty) (gradF fun) [arg]
    `lmCompose`
    gradE arg

gradEX ty (Let v e1 e2) = mkLet v e1                 $
                          mkLet (gradTV v) (gradE e1)   $
                          gradE e2

gradEX ty (Tuple es) = lmVCats (map gradE es)
gradEX ty (If b t e) = mkIf b (gradE t) (gradE e)
gradEX ty e@(Lam {}) = pprPanic "gradE: can't deal with lambda yet" (ppr e)

gradDefs :: [Def] -> [Def]
gradDefs = map gradDef

gradDef :: Def -> Def
gradDef (Def fun params rhs)
  = Def (gradTF fun) params $
    mkLets [ (gradTV param, gradParam param i n)
           | (param, i) <- params `zip` [1..] ] $
    gradE rhs
  where
    n = length params
    gradParam (TVar TypeInteger _) _ _= lmZero TypeInteger TypeInteger
    gradParam _ i n = gradSelFun (TypeTuple $ map (\(TVar ty _) -> ty) params) i n 

gradSelFun :: Type -> Int -> Int -> Expr
-- (gradSelFun i n) selects the i'th component of a n-tuple
-- Special case for 1-tuples
-- Result expr has type (t1, ..., tn) -o ti
gradSelFun (TypeTuple [t]) i 1 = lmOne t
gradSelFun tyt@(TypeTuple ts) i n =  mkCallF (TypeLM (ts!!(i-1)) (ts!!(i-1))) (GradFun (SelFun i n) Fwd) []


---------------------------------

-- Apply-to-dx
--
-- Local bindings of type (S -o T)
-- are transformed to ones of type T

applyD :: Def -> Def
applyD (Def (TFun (TypeLM s t) (GradFun f Rev)) vars rhs)
  = Def (TFun s (DrvFun f Rev)) (vars ++ [TVar t dr]) $
    lmApply rhs $ Expr t (Var dr)
    where dr = Delta "r"

applyD (Def (TFun (TypeLM s t) (GradFun f Fwd)) vars rhs)
  = Def (TFun t (DrvFun f Fwd)) (vars ++ dvars) $
    lmApply rhs (mkTuple $ map (\(TVar ty v) -> Expr ty $ Var v) dvars)
  where
    dvars = map to_delta vars

    to_delta (TVar ty (Simple x)) = TVar ty (Delta x)

---------------------------------
-- Transpose

transposeD :: Def -> Def
transposeD (Def (TFun (TypeLM s t) (GradFun f d)) args rhs)
  = Def (TFun (TypeLM t s) (GradFun f (flipMode d))) args $
    lmTranspose rhs
