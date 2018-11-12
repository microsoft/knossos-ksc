module AD where

import Lang
import Prim
import Text.PrettyPrint as PP

gradDefs :: [TDef] -> [TDef]
gradDefs = map gradDef

gradDef :: TDef -> TDef
gradDef (DefX (TFun ty f) params rhs) =
  let gradf = TFun tylm $ gradF f in
  assertEqualThen ("gradDef " ++ show f) ty (typeof rhs) $
  DefX gradf params $
    mkLets [ gradParam param i n | (param, i) <- params `zip` [1..] ] $
    gradE tys rhs

  where
    n = length params
    tys = tysOfParams params
    tylm = TypeLM tys ty

    tysOfParams [] = TypeUnknown
    tysOfParams [TVar ty _] = ty
    tysOfParams ps = TypeTuple (map (\(TVar ty v) -> ty) ps)

    gradParam (TVar TypeInteger v) _ _ = 
      (TVar (TypeLM tys TypeInteger) (gradV v), lmZero tys TypeInteger)
    gradParam (TVar tyv v) i n =
      (TVar (TypeLM tys tyv) (gradV v), gradSelFun tys i n params) 

gradSelFun :: Type -> Int -> Int -> [TVar Var] -> TExpr
-- (gradSelFun i n) selects the i'th component of a n-tuple
-- Special case for 1-tuples
-- Result expr has type (t1, ..., tn) -o ti
gradSelFun tyt@(TypeTuple ts) i n params = 
   mkTCall (TypeLM tyt (ts!!(i-1))) (GradFun (SelFun i n) Fwd) (map Var params)
gradSelFun t i 1 params = lmOne t


gradF :: Fun -> Fun
gradF (Fun f) = GradFun f Fwd
gradF f       = error ("gradF: bad function: " ++ show f)

gradV :: Var -> Var
gradV (Simple x) = Grad x Fwd
gradV v          = error ("gradV: bad variable: " ++ PP.render (ppr v))

gradTV :: Type -> TVar Var -> TVar Var
gradTV s (TVar ty v) = TVar (TypeLM s ty) (gradV v)

-- s -> Expr :: t -> Expr LM s -o t
gradE :: Type -> TExpr -> TExpr
gradE s (Konst k)         = lmZero s (typeofKonst k)
gradE s (Var v) = Var $ gradTV s v
gradE s (Assert e1 e2)    = Assert e1 (gradE s e2)

-- grad[ build (\i.e ]
--  = B (\i. let Di = 0 in grad[e])
-- We need the Di binding in case 'i' is mentioned in
-- grad[e], e.g. build (\i. power(x, i))
gradE s (Call (TFun _ (Fun (SFun "build"))) (Tuple [n, (Lam i TypeInteger b)]))
  = lmBuild n $ Lam i TypeInteger (Let (gradTV s i) (lmZero  s TypeInteger) (gradE s b))

gradE s (Call (TFun tyf fun) arg) =
  let tyarg = typeof arg in
  mkTCall (TypeLM tyarg tyf) (gradF fun) [arg]
  `lmCompose`
  gradE s arg

gradE s (Let v e1 e2) = Let v e1                        $
                        Let (gradTV s v) (gradE s e1)   $
                        gradE s e2

gradE s (Tuple es) = lmVCats (map (gradE s) es)
gradE s (If b t e) = If b (gradE s t) (gradE s e)
gradE s e@(Lam {}) = pprPanic "gradE: can't deal with lambda yet" (ppr e)


---------------------------------

-- Apply-to-dx
--
-- Local bindings of type (S -o T)
-- are transformed to ones of type T

applyD :: TDef -> TDef
applyD (DefX (TFun (TypeLM s t) (GradFun f Rev)) vars rhs)
  = DefX (TFun s (DrvFun f Rev)) (vars ++ [dr]) $
    lmApply rhs $ Var dr
    where dr = TVar t $ Delta "r"

applyD (DefX (TFun (TypeLM s t) (GradFun f Fwd)) vars rhs)
  = DefX (TFun t (DrvFun f Fwd)) (vars ++ dvars) $
    lmApply rhs (mkTuple $ map Var dvars)
  where
    dvars = map to_delta vars

    to_delta (TVar ty (Simple x)) = TVar ty (Delta x)

---------------------------------
-- Transpose

transposeD :: TDef -> TDef
transposeD (DefX (TFun (TypeLM s t) (GradFun f d)) args rhs)
  = DefX (TFun (TypeLM t s) (GradFun f (flipMode d))) args $
    lmTranspose rhs
