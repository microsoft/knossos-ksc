-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.

{-# LANGUAGE DataKinds #-}

module Ksc.AD where

import Ksc.Lang
import Ksc.LangUtils
import Ksc.Prim
import qualified Ksc.OptLet
import GHC.Stack

import Data.Maybe (mapMaybe)
import Text.PrettyPrint (render)

{- Note [Automatic differentiation documentation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ksc has a variety of different automatic differentiation algorithms.
Information about how to obtain the PDF in which our AD algorithms are
documented can be found at

https://github.com/microsoft/knossos-ksc/blob/master/README-ksc.md#getting-the-pdf

-}

--------------- Generate names for gradded indentifiers

gradF :: HasCallStack => Fun Typed -> Fun Typed
gradF (Fun JustFun f) = Fun GradFun f
gradF f       = error ("gradF: bad function: " ++ show f)

gradV :: Var -> Var
gradV (Simple x) = Grad x
gradV v            = error ("gradV: bad variable: " ++ render (ppr v))

gradTFun :: HasCallStack => TFun Typed -> Type -> TFun Typed
gradTFun (TFun res_ty f) arg_tys
  = TFun (mkGradType arg_tys res_ty)
         (gradF f)

mkGradTVar :: HasCallStack => Type -> Var -> Type -> TVar
mkGradTVar s var ty
  = TVar (mkGradType s ty) var

gradTVar :: Shape -> TVar -> TVar
gradTVar s (TVar ty v) = mkGradTVar (typeof s) (gradV v) ty

-------------------------------------------------

gradDefs :: HasCallStack => [TDef] -> [TDef]
gradDefs = mapMaybe gradDef

-- We noTupPatifyDef before gradDef.  See Note [Replacing TupPat with
-- nested Let].
gradDef :: HasCallStack => TDef -> Maybe TDef
gradDef = gradDefInner . noTupPatifyDef

gradDefInner :: HasCallStack => TDef -> Maybe TDef
gradDefInner
        (Def { def_fun = Fun JustFun f, def_pat = VarPat params
             , def_rhs = UserRhs rhs, def_res_ty = res_ty })
  = Just $
    Def { def_fun    = Fun GradFun f
        , def_pat    = VarPat params
        , def_res_ty = mkGradType s_ty res_ty
        , def_rhs    = UserRhs (mkLets lets (gradE s rhs')) }
  where
    s :: TExpr
    s = Var params
    s_ty = typeof s

    -- See Note: [Shadowing after grad]
    rhs' = Ksc.OptLet.ensureDon'tReuseParams [params] rhs

    lets = [ (gradTVar s params,
              mkGradTuple (Var params) (lmOne (typeof params)))
           ]

gradDefInner (Def { def_pat = TupPat {} })
  -- TupPat should not appear.  See Note [Replacing TupPat with nested
  -- Let]
  = error $ unlines [ "gradDefInner: TupPat encountered\n"
                    , "This should not occur." ]

gradDefInner _ = Nothing


-- See Note [Automatic differentiation documentation]
--
-- s -> (Expr :: t) -> (Expr :: s -o t)
gradE :: HasCallStack => Shape -> TExpr -> TExpr
gradE s e@(Konst _)    = mkGradTuple e (lmZero s e)
gradE s (Var tv)       = Var (gradTVar s tv)
gradE s (Dummy ty)     = Dummy (mkGradType (typeof s) ty)
gradE s (Assert e1 e2) = Assert e1 (gradE s e2)
gradE s (Tuple es)     = lmVCat_AD s (map (gradE s) es)
gradE s (If b t e)     = If b (gradE s t) (gradE s e)
gradE _ e@(Lam {})     = pprPanic "gradE: can't deal with lambda yet" (ppr e)
gradE s (Let (VarPat v) e1 e2) = gradLet s v e1 e2
gradE _ e@(Let (TupPat _) _ _) =
  -- TupPat should not appear.  See Note [Replacing TupPat with nested
  -- Let]
  pprPanic "gradE: TupPat encountered. This should not occur." (ppr e)
gradE _ (App{})        = error "gradE of App not yet supported"

-- Currently ignoring $inline when gradding.  Perhaps we should
-- perform the inlining before gradding.
gradE s (Call f arg)
  | f `isThePrimFun` P_inline
  = gradE s arg

-- grad[ build (\i.e ]
--  = B (\i. let Di = 0 in grad[e])
-- We need the Di binding in case 'i' is mentioned in
-- grad[e], e.g. build (\i. power(x, i))
gradE s (Call f (Tuple [n, Lam ti body]))
  | f `isThePrimFun` P_build
  = gradBuild s n ti body

-- TODO: I'm not very happy about this rule, which effectively
-- undoes sum (build e) --> sumbuild e
gradE s (Call f (Tuple [n, body]))
  | f `isThePrimFun` P_sumbuild
  = gradE s (pSum (pBuild n body))

gradE s (Call f (Tuple [Lam ti body, acc, v]))
  | f `isThePrimFun` P_fold
  = gradFold s ti body acc v

gradE s (Call f args) = gradCall s f args

---------------
gradBuild :: Shape -> TExpr -> TVar -> TExpr -> TExpr
gradBuild s n ti body
  = lmVCatV $
    pBuild n $ Lam ti $
    mkLet (gradTVar s ti) (lmZero s (Var ti)) $
    gradE s body
---------------
gradFold :: Shape -> TVar -> TExpr -> TExpr -> TExpr -> TExpr
gradFold s ti body acc v =
  lmFold (mkTangentZero s) (Lam ti body) (Lam ti bodyAdjusted) acc v
  `lmCompose`
  args
  where body' = mkLet (gradTVar s' ti)
                      (lmHCat [lmZero s (Var ti), lmOne (typeof ti)])
                $ gradE s' body
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
            grad = gradTVar
            adjust v = Var v `lmCompose` lmHCat [lmOne (typeof s), lmZero (Var ti) s]

        args = lmVCat s
               [ lmOne (typeof s)
               , lmVCat s (map (gradE s) [acc, v]) ]
---------------
gradCall :: Shape -> TFun Typed -> TExpr -> TExpr
gradCall s f args
  = lmCompose (Call gf args) (gradE s args)
  where
    gf = gradTFun f (typeof args)
----------------------
gradLet :: HasCallStack => Shape -> TVar -> TExpr -> TExpr -> TExpr
gradLet s v e1 e2
  = mkLet (gradTVar s v) (gradE s e1) $
      -- See Note [Shadowing after grad]
    mkLet v e1                                        $
    gradE s e2

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

lmVCat_AD :: Shape -> [TExpr] -> TExpr
lmVCat_AD s ms = lmVCat s ms



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
-- fwd$f  :: (S1, S2) (dS1, dS2) -> dT
applyD Fwd (Def { def_fun = Fun GradFun f, def_res_ty = res_ty
                , def_pat = VarPat x, def_rhs = UserRhs rhs })
  = Def { def_fun    = Fun (DrvFun Fwd) f
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
        = case res_ty of
            TypeLM _ t        -> (id, rhs, tangentType t)
            t -> error ("Unexpected combination result type:"
                       ++ " " ++ show t)

--   D$f :: S1 S2    -> ((S1,S2) -o T)
-- rev$f :: (S1, S2) dT -> (dS1,dS2)
applyD Rev (Def { def_fun = Fun GradFun f, def_res_ty = res_ty
                , def_pat = VarPat x, def_rhs = UserRhs rhs })
  = Def { def_fun    = Fun (DrvFun Rev) f
        , def_pat    = VarPat x_dr
        , def_rhs    = UserRhs $ extract2args $ lmApplyR (Var dr) lm
        , def_res_ty = tangentType (typeof x) }
  where
    x_dr = newVarNotIn (TypeTuple [typeof x, typeof dr])
                       (Tuple (map Var [x,dr]))

    extract2args = mkLets [ (x,  pFst (Var x_dr))
                          , (dr, pSnd (Var x_dr)) ]

    dr = TVar (tangentType t) $ Delta "r"
    (lm, t)  -- lm :: s -o t
        = case res_ty of
            TypeLM _ t       -> (rhs,      t)
            t -> error ("Unexpected combination of AD plan and result type:"
                       ++ " " ++ show t)

-------------------------------

applyD _ def = def

applyDef :: ADDir -> TDef -> TDef
applyDef = applyD

applyDefs :: ADDir -> [TDef] -> [TDef]
applyDefs dir = map (applyDef dir)
