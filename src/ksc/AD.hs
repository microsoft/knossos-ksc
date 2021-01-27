-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.

{-# LANGUAGE DataKinds #-}

module AD where

import Lang
import LangUtils
import Prim
import qualified OptLet
import GHC.Stack

import Data.Maybe (mapMaybe, fromMaybe)

-- for unit test
--import Test.Hspec

--------------- Generate names for gradded indentifiers

gradF :: HasCallStack => ADPlan -> Fun Typed -> Fun Typed
gradF adm (Fun f) = GradFun f adm
gradF _   f       = error ("gradF: bad function: " ++ show f)

gradV :: ADPlan -> Var -> Var
gradV adp (Simple x) = Grad x adp
gradV _ v            = error ("gradV: bad variable: " ++ render (ppr v))

gradTFun :: HasCallStack => ADPlan -> TFun Typed -> Type -> TFun Typed
gradTFun adp (TFun res_ty f) arg_tys
  = TFun (mkGradType adp arg_tys res_ty)
         (gradF adp f)

mkGradTVar :: HasCallStack => ADPlan -> Type -> Var -> Type -> TVar
mkGradTVar adp s var ty
  = TVar (mkGradType adp s ty) var

gradTVar :: ADPlan -> Shape -> TVar -> TVar
gradTVar adp s (TVar ty v) = mkGradTVar adp (typeof s) (gradV adp v) ty

-------------------------------------------------

gradDefs :: HasCallStack => ADPlan -> [TDef] -> [TDef]
gradDefs adp = mapMaybe (gradDef adp)

-- We noTupPatifyDef before gradDef.  See Note [Replacing TupPat with
-- nested Let].
gradDef :: HasCallStack => ADPlan -> TDef -> Maybe TDef
gradDef adp = gradDefInner adp . noTupPatifyDef

gradDefInner :: HasCallStack => ADPlan -> TDef -> Maybe TDef
gradDefInner adp
        (Def { def_fun = f@(Fun{}), def_pat = VarPat params
             , def_rhs = UserRhs rhs, def_res_ty = res_ty })
  = Just $
    Def { def_fun    = gradF adp f
        , def_pat    = VarPat params
        , def_res_ty = mkGradType adp s_ty res_ty
        , def_rhs    = UserRhs (mkLets lets (gradE adp s rhs')) }
  where
    s :: TExpr
    s = Var params
    s_ty = typeof s

    -- See Note: [Shadowing after grad]
    rhs' = OptLet.ensureDon'tReuseParams [params] rhs

    lets = [ (gradTVar adp s params,
              mkGradTuple adp (Var params) (lmOne (typeof params)))
           ]

gradDefInner _ (Def { def_pat = TupPat {} })
  -- TupPat should not appear.  See Note [Replacing TupPat with nested
  -- Let]
  = error $ unlines [ "gradDefInner: TupPat encountered\n"
                    , "This should not occur." ]

gradDefInner _ _ = Nothing


-- s -> (Expr :: t) -> (Expr :: s -o t)
gradE :: HasCallStack => ADPlan -> Shape -> TExpr -> TExpr
gradE adp s e@(Konst _)    = mkGradTuple adp e (lmZero s e)
gradE adp s (Var tv)       = Var (gradTVar adp s tv)
gradE adp s (Dummy ty)     = Dummy (mkGradType adp (typeof s) ty)
gradE adp s (Assert e1 e2) = Assert e1 (gradE adp s e2)
gradE adp s (Tuple es)     = lmVCat_AD adp (map (gradE adp s) es)
gradE adp s (If b t e)     = If b (gradE adp s t) (gradE adp s e)
gradE _   _ e@(Lam {})     = pprPanic "gradE: can't deal with lambda yet" (ppr e)
gradE adp s (Let (VarPat v) e1 e2) = gradLet adp s v e1 e2
gradE _   _ e@(Let (TupPat _) _ _) =
  -- TupPat should not appear.  See Note [Replacing TupPat with nested
  -- Let]
  pprPanic "gradE: TupPat encountered. This should not occur." (ppr e)
gradE _   _ (App{})        = error "gradE of App not yet supported"

-- Currently ignoring $inline when gradding.  Perhaps we should
-- perform the inlining before gradding.
gradE adp s (Call f arg)
  | f `isThePrimFun` "$inline"
  = gradE adp s arg

-- grad[ build (\i.e ]
--  = B (\i. let Di = 0 in grad[e])
-- We need the Di binding in case 'i' is mentioned in
-- grad[e], e.g. build (\i. power(x, i))
gradE adp s (Call f (Tuple [n, Lam ti body]))
  | f `isThePrimFun` "build"
  = gradBuild adp s n ti body

-- TODO: I'm not very happy about this rule, which effectively
-- undoes sum (build e) --> sumbuild e
gradE adp s (Call f (Tuple [n, body]))
  | f `isThePrimFun` "sumbuild"
  = gradE adp s (pSum (pBuild n body))

gradE adp s (Call f (Tuple [Lam ti body, acc, v]))
  | f `isThePrimFun` "fold"
  = gradFold adp s ti body acc v

gradE adp s (Call f args) = gradCall adp s f args

---------------
gradBuild :: ADPlan -> Shape -> TExpr -> TVar -> TExpr -> TExpr
gradBuild BasicAD s n ti body
  = lmVCatV $
    pBuild n $ Lam ti $
    mkLet (gradTVar BasicAD s ti) (lmZero s (Var ti)) $
    gradE BasicAD s body

gradBuild TupleAD s n ti body
  = mkLet p (pUnzip (pBuild n (Lam ti grad_body))) $
    Tuple [ pFst (Var p)
          , lmVCatV (pSnd (Var p)) ]
  where
     t_ty = typeof body
     p = TVar res_ty resVar
     err = error $ "Unexpected size type in gradBuild: " ++ show (typeof n)
     d = fromMaybe err (tensorDimensionFromIndexType_maybe (typeof n))
     res_ty = TypeTuple [ TypeTensor d t_ty
                        , TypeTensor d (TypeLM (typeof s) t_ty) ]
     grad_body = mkLet (gradTVar TupleAD s ti)
                       (Tuple [Var ti, lmZero s (Var ti)]) $
                 gradE TupleAD s body
---------------
gradFold :: ADPlan -> Shape -> TVar -> TExpr -> TExpr -> TExpr -> TExpr
gradFold BasicAD s ti body acc v =
  lmFold (mkTangentZero s) (Lam ti body) (Lam ti bodyAdjusted) acc v
  `lmCompose`
  args
  where body' = mkLet (gradTVar BasicAD s' ti)
                      (lmHCat [lmZero s (Var ti), lmOne (typeof ti)])
                $ gradE BasicAD s' body
        s' = Tuple [s, Var ti]

        -- The gradded free variables occurring in `body'` are linear
        -- maps whose domain is `s'` (because they were created with
        -- a call of `gradE Basic AD s'`). However, they were bound
        -- outside body in such a way that their domain is `s`.  Thus
        -- we reassign them here.  All we need to do is project from
        -- `ty` = `(s, typeof ti)` to `s`, which is a simple `HCat` of
        -- an `lmOne` (on `s`) and an `lmZero` (on `ti`).
        --
        -- NB the gradded free variables occurring in `body'` are the
        -- gradded versions of all free variables occurring in
        -- `\ti.body`.
        bodyAdjusted = foldr adjustGrad body' (freeVarsOf (Lam ti body))
          where
            adjustGrad v = mkLet (grad s' v) (adjust (grad s v))
            grad = gradTVar BasicAD
            adjust v = Var v `lmCompose` lmHCat [lmOne (typeof s), lmZero (Var ti) s]

        args = lmVCat
               [ lmOne (typeof s)
               , lmVCat (map (gradE BasicAD s) [acc, v]) ]

-- Just a dummy for tuple mode.  We don't calculate it properly yet.
gradFold TupleAD s _ti _body acc _v =
  lmDummyFold (TypeTuple [t_acc, TypeLM (typeof s) t_acc])
  where t_acc = typeof acc

---------------
gradCall :: ADPlan -> Shape -> TFun Typed -> TExpr -> TExpr
gradCall BasicAD s f args
  = lmCompose (Call gf args) (gradE BasicAD s args)
  where
    gf = gradTFun BasicAD f (typeof args)

gradCall TupleAD s f args
  = mkLets (case grad_arg_let of { Just p -> [p]; Nothing -> [] }) $
    mkLet res_tv (Call gf pFst_grad_arg) $
    Tuple [ pFst (Var res_tv), lmCompose (pSnd (Var res_tv)) (pSnd grad_arg) ]
  where
    gf     = gradTFun TupleAD f (typeof args)
    res_ty = typeof f
    res_tv = mkGradTVar TupleAD arg_tys resVar res_ty
    arg_tys = typeof args

    (grad_arg, pFst_grad_arg, grad_arg_let) = grad_arg_binds

     -- Nothing <=> the arg is atomic, so no need to let-bind
    grad_arg_binds :: (TExpr, TExpr, Maybe (TVar,TExpr))
    grad_arg_binds
      -- If arg is trivial then we want to apply it directly to gf.
      -- This is particularly important in cases where the type of the
      -- body depends on arg.  Having arg appear in the form 'fst
      -- (arg, <darg>)' displeases the type checker when it's the
      -- first argument of build, for example.
      | isTrivial arg
      = (Var arg_var, arg, Just (arg_var, grad_arg))
      | isTrivial grad_arg
      = (grad_arg, pFst grad_arg, Nothing)
      | otherwise
      = (Var arg_var, pFst (Var arg_var), Just (arg_var, grad_arg))
      where
        arg_var  = mkGradTVar TupleAD (typeof s) (mkArgVar 1) (typeof arg)
        grad_arg = gradE TupleAD s arg
        arg      = args

----------------------
gradLet :: HasCallStack => ADPlan -> Shape -> TVar -> TExpr -> TExpr -> TExpr
gradLet BasicAD s v e1 e2
  = mkLet (gradTVar BasicAD s v) (gradE BasicAD s e1) $
      -- See Note [Shadowing after grad]
    mkLet v e1                                        $
    gradE BasicAD s e2

gradLet TupleAD s v e1 e2
  -- We include a binding for the old v because pre-gradded
  -- subexpressions may contain it and it's easier to just let them
  -- continue to refer to it rather than substitute 'fst(dv)' for it
  -- everywhere.
  = mkLet dv (gradE TupleAD s e1) $
    mkLet v (pFst (Var dv)) $
    gradE TupleAD s e2
  where dv = gradTVar TupleAD s v

{- Note [Shadowing after grad]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We transform   let x = <rhs>
               in <body>
to
               let g$x = <gradded rhs>
               let x   = <rhs>
               in body

Note that the g$x defn comes first.  Why?  Because <gradded rhs>
might mention x, either directly (remember this is a non-rec let)
or via a call to lmZero if 'x' is a parameter of the function in
whose RHS this is.

If <gradded rhs> mentions x, it should be the x from the outer
scope, the locally bound x!  See test/ksc/test0, test_inline2

Unfortunately this trick is not sufficient to cope with lambdas in
builds.  If we have

    (def f ... (a : T)
        ... (build 10 (lam (a : Integer) <body>)) ...)

then in the transformed build there will still be a

    (lam (a : Integer) <gradded body>)

But when gradding the body we pass in the Shape of the function
result, to use in calls to lmZero. Alas, this Shape contains (a:T)
from the parameters to f.

To avoid this we must ensure that the parameters to f do not clash
with any bound variable in the body. We do so by giving all binders
unique names before gradding.

-}

lmVCat_AD :: ADPlan -> [TExpr] -> TExpr
lmVCat_AD BasicAD ms = lmVCat ms
lmVCat_AD TupleAD ms = Tuple [ Tuple  (map pFst ms)
                             , lmVCat (map pSnd ms) ]



---------------------------------

-- Apply-to-dx
--
-- Local bindings of type (S -o T)
-- are transformed to ones of type T

applyD :: ADDir -> TDef -> TDef

applyD dir def@(Def { def_pat = TupPat {} })
  = applyD dir (noTupPatifyDef def)

-- Forward
--   D$f  :: S1 S2       -> ((S1,S2) -o T)
--  Dt$f  :: S1 S2       -> (T, (S1,S2) -o T)
-- fwd$f  :: (S1, S2) (dS1, dS2) -> dT
-- fwdt$f :: (S1, S2) (dS1, dS2) -> (T, dT)
applyD Fwd (Def { def_fun = GradFun f adp, def_res_ty = res_ty
                , def_pat = VarPat x, def_rhs = UserRhs rhs })
  = Def { def_fun    = DrvFun f (AD adp Fwd)
        , def_pat   = VarPat x_dx
        , def_rhs    = UserRhs $ extract2args $ perhapsFstToo $ lmApply lm $ Var dx
        , def_res_ty = t }
  where
    x_dx = newVarNotIn (TypeTuple [typeof x, typeof dx])
                       (Tuple (map Var [x,dx]))

    dx = to_delta x

    extract2args = mkLets [ (x,  pFst (Var x_dx))
                          , (dx, pSnd (Var x_dx)) ]

    to_delta (TVar ty (Simple x)) = TVar (tangentType ty) (Delta x)
    to_delta (TVar _  v         )
      = error ("Unexpected non-Simple variable: " ++ show v)
    (perhapsFstToo, lm, t)  -- lm :: s -o t
        = case (adp, res_ty) of
            (BasicAD, TypeLM _ t)       -> (id, rhs, tangentType t)
            (TupleAD, TypeTuple [t, _]) -> (\lmrhs -> Tuple [pFst rhs, lmrhs],
                                            pSnd rhs,
                                            TypeTuple [t, tangentType t])
            (adp    , t               )
              -> error ("Unexpected combination of AD plan and result type:"
                       ++ show adp ++ " " ++ show t)

--   D$f :: S1 S2    -> ((S1,S2) -o T)
-- rev$f :: (S1, S2) dT -> (dS1,dS2)
applyD Rev (Def { def_fun = GradFun f adp, def_res_ty = res_ty
                , def_pat = VarPat x, def_rhs = UserRhs rhs })
  = Def { def_fun    = DrvFun f (AD adp Rev)
        , def_pat    = VarPat x_dr
        , def_rhs    = UserRhs $ extract2args $ lmApplyR (Var dr) lm
        , def_res_ty = tangentType (mkTupleTy [typeof x]) }
  where
    x_dr = newVarNotIn (TypeTuple [typeof x, typeof dr])
                       (Tuple (map Var [x,dr]))

    extract2args = mkLets [ (x,  pFst (Var x_dr))
                          , (dr, pSnd (Var x_dr)) ]

    dr = TVar (tangentType t) $ Delta "r"
    (lm, t)  -- lm :: s -o t
        = case (adp, res_ty) of
            (BasicAD, TypeLM _ t)       -> (rhs,      t)
            (TupleAD, TypeTuple [t, _]) -> (pSnd rhs, t)
            (adp    , t               )
              -> error ("Unexpected combination of AD plan and result type:"
                       ++ show adp ++ " " ++ show t)

-------------------------------

applyD _ def = def

applyDefs :: ADDir -> [TDef] -> [TDef]
applyDefs dir = map (applyD dir)
