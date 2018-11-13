module AD where

import GHC.Stack
import Lang
import Prim
import Text.PrettyPrint as PP

--------------- Generate names for gradded indentifiers

gradF :: Fun -> Fun
gradF (Fun f) = GradFun f Fwd
gradF f       = error ("gradF: bad function: " ++ show f)

gradV :: Var -> Var
gradV (Simple x) = Grad x Fwd
gradV v          = error ("gradV: bad variable: " ++ PP.render (ppr v))

gradSelFun :: Type -> Int -> Int -> [TVar Var] -> TExpr
-- (gradSelFun i n) selects the i'th component of a n-tuple
-- Special case for 1-tuples
-- Result expr has type (t1, ..., tn) -o ti
-- gradSelFun tyt@(TypeTuple ts) i n params = 
--    mkTCall (TypeLM (LMSelFun tyt (ts!!(i-1)))) (GradFun (SelFun i n) Fwd) (map Var params)
gradSelFun tyt@(TypeTuple ts) i n params = 
   let es = (map Var params) in
    lmHCat [ if i == j then lmOne t else lmZero (ts!!(j-1)) t  |   j <- [1..n] ]
   where t = ts!!(i-1)
gradSelFun t i 1 params = lmOne t

-------------------------------------------------

gradDefs :: [TDef] -> [TDef]
gradDefs defs =
  reverse $ snd $ foldl add_def_to_env (stCreate,[]) defs
  where
    add_def_to_env :: (ST, [TDef]) -> TDef -> (ST,[TDef])
    add_def_to_env (env, gdefs) def = 
      let gdef@(DefX (TFun ty g) params rhs) = gradDef env def in
          (stInsertFun g ty env, gdef:gdefs) 

gradDef :: ST -> TDef -> TDef
gradDef env (DefX (TFun ty f) params rhs) =
  assertEqualThen ("gradDef " ++ show f) ty (typeof rhs) $
  let tys = tysOfParams params
      lets = [ gradParam tys param i n | (param, i) <- params `zip` [1..] ]
      env' = foldl add_let_to_env env lets 
      grhs = gradE env' tys rhs
      tylm = typeofCheckLM (LM tys ty) grhs
  in
    DefX (TFun (typeof grhs) (gradF f)) params (mkLets lets grhs)

  where
    n = length params

    add_let_to_env :: ST -> (TVar Var, TExpr) -> ST
    add_let_to_env env (TVar ty v, e) = stInsert v ty env

    tysOfParams [] = TypeUnknown
    tysOfParams [TVar ty _] = ty
    tysOfParams ps = TypeTuple (map (\(TVar ty v) -> ty) ps)

    gradParam :: Type -> TVar Var -> Int -> Int -> (TVar Var, TExpr)
    gradParam tys (TVar TypeInteger v) _ _ = 
      (TVar (typeof g) (gradV v), g) where g = lmZero tys TypeInteger

    gradParam tys (TVar tyv v) i n =
      (TVar (typeof g) (gradV v), g) where g = gradSelFun tys i n params 


-- s -> Expr :: t -> Expr LM s -o t
gradE :: ST -> Type -> TExpr -> TExpr
gradE env s (Konst k)         = lmZero s (typeofKonst k)
gradE env s (Var (TVar ty v)) = Var (TVar (stLookup "var" gv env) gv) where gv = gradV v
gradE env s (Assert e1 e2)    = Assert e1 (gradE env s e2)

-- grad[ build (\i.e ]
--  = B (\i. let Di = 0 in grad[e])
-- We need the Di binding in case 'i' is mentioned in
-- grad[e], e.g. build (\i. power(x, i))
gradE env s (Call (TFun _ (Fun (SFun "build"))) (Tuple [n, (Lam ti@(TVar TypeInteger i) TypeInteger b)]))
  = lmBuild n $ 
    Lam ti TypeInteger $
        (mkLet (TVar (typeof gi) (gradV i)) gi (gradE env s b)) 
    where gi = lmZero s TypeInteger

gradE env s (Call (TFun tyf fun) arg) =
  let gfun = gradF fun
      tyarg = typeof arg 
      ty = typeofFunTy env gfun tyarg in
  mkTCall ty gfun [arg]
  `lmCompose`
  gradE env s arg

gradE env s (Let (TVar ty v) e1 e2) =
  let ge1 = gradE env s e1 -- v is already well typed in the env
      ty1 = typeof ge1
      env' = stInsert (gradV v) ty1 env
      ge2 = gradE env' s e2 in
    Let (TVar ty v) e1                        $                        
    Let (TVar ty1 (gradV v)) ge1   $
    ge2

gradE env s (Tuple es) = lmVCat (map (gradE env s) es)
gradE env s (If b t e) = If b (gradE env s t) (gradE env s e)
gradE env s e@(Lam {}) = pprPanic "gradE: can't deal with lambda yet" (ppr e)


---------------------------------

-- Apply-to-dx
--
-- Local bindings of type (S -o T)
-- are transformed to ones of type T

applyD :: TDef -> TDef
applyD (DefX (TFun (TypeLM tylm) (GradFun f Rev)) vars rhs)
  = DefX (TFun (typeofSrc tylm) (DrvFun f Rev)) (vars ++ [dr]) $
    lmApply rhs $ Var dr
    where dr = TVar (typeofDst tylm) $ Delta "r"

applyD (DefX (TFun (TypeLM tylm) (GradFun f Fwd)) vars rhs)
  = DefX (TFun (typeofDst tylm) (DrvFun f Fwd)) (vars ++ dvars) $
    lmApply rhs (mkTuple $ map Var dvars)
  where
    dvars = map to_delta vars

    to_delta (TVar ty (Simple x)) = TVar ty (Delta x)

---------------------------------
-- Transpose

transposeD :: TDef -> TDef
transposeD (DefX (TFun (TypeLM tylm) (GradFun f d)) args rhs)
  = DefX (TFun (TypeLM (LMTranspose tylm)) (GradFun f (flipMode d))) args $
    lmTranspose rhs
