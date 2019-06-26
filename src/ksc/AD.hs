-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AD where

import Lang
import LangUtils
import Prim
import GHC.Stack

-- for unit test
--import Test.Hspec

--------------- Generate names for gradded indentifiers

gradF :: HasCallStack => ADPlan -> Fun -> Fun
gradF adm (Fun f) = GradFun f adm
gradF _   f       = error ("gradF: bad function: " ++ show f)

gradV :: ADPlan -> Var -> Var
gradV adp (Simple x) = Grad x adp
gradV _ v            = error ("gradV: bad variable: " ++ render (ppr v))

gradTFun :: HasCallStack => ADPlan -> TFun -> [Type] -> TFun
gradTFun adp (TFun res_ty f) arg_tys
  = TFun (mkGradType adp (mkTupleTy arg_tys) res_ty)
         (gradF adp f)

mkGradTVar :: HasCallStack => ADPlan -> Type -> Var -> Type -> TVar
mkGradTVar adp s var ty
  = TVar (mkGradType adp s ty) var

gradTVar :: ADPlan -> Type -> TVar -> TVar
gradTVar adp s (TVar ty v) = mkGradTVar adp s (gradV adp v) ty

lmSelFun :: [TVar] -> TVar -> Int -> TExpr
-- (gradSelFun i n) selects the i'th component of a n-tuple
-- Result expr has type (t1, ..., tn) -o ti
--  or its transpose depending on direction
lmSelFun params pi i
  = lmHCat [ lm pj j | (pj,j) <- params `zip` [1..] ]
  where
    pi_ty = typeof pi
    lm pj j | i == j    = lmOne pi_ty
            | otherwise = lmZero (typeof pj) pi_ty

-------------------------------------------------

isUserFunDef :: TDef -> Bool
isUserFunDef (Def { def_fun = Fun _
                  , def_rhs = UserRhs {} })
               = True
isUserFunDef _ = False

filterGradFuns :: [TDef] -> [TDef]
filterGradFuns = filter isUserFunDef

gradDefs :: HasCallStack => ADPlan -> [TDef] -> [TDef]
gradDefs adp = map (gradDef adp) . filterGradFuns

gradDef :: HasCallStack => ADPlan -> TDef -> TDef
gradDef adp
        (Def { def_fun = f, def_args = params
             , def_rhs = UserRhs rhs, def_res_ty = res_ty })
  = Def { def_fun    = gradF adp f
        , def_args   = params
        , def_res_ty = mkGradType adp s res_ty
        , def_rhs    = UserRhs (mkLets lets (gradE adp s rhs)) }
  where
    param1 :: TExpr
    param1 = mkTuple (map Var params)
    s = typeof param1

    lets = plets ++ szlets

    plets = [ (gradTVar adp s p, mkGradTuple adp (Var p) lm)
            | (p,i) <- params `zip` [1..]
            , let lm = lmSelFun params p i ]

    szlets = [ (gradTVar adp s p, mkGradTuple adp (Var p)
                                     (lmZero s TypeSize))
             | p <- paramsSizeBinders params]

gradDef _ def = pprPanic "gradDef" (ppr def)
   -- The filterGradFuns should make this impossible


-- s -> (Expr :: t) -> (Expr :: s -o t)
gradE :: HasCallStack => ADPlan -> Type -> TExpr -> TExpr
gradE adp s e@(Konst _)    = mkGradTuple adp e lm_zero
  where
    lm_zero = lmZero s (typeof e)

gradE adp s (Var tv)       = Var (gradTVar adp s tv)
gradE adp s (Assert e1 e2) = Assert e1 (gradE adp s e2)
gradE adp s (Tuple es)     = lmVCat_AD adp (map (gradE adp s) es)
gradE adp s (If b t e)     = If b (gradE adp s t) (gradE adp s e)
gradE _   _ e@(Lam {})     = pprPanic "gradE: can't deal with lambda yet" (ppr e)
gradE adp s (Let v e1 e2)  = gradLet adp s v e1 e2

-- Currently ignoring $inline when gradding.  Perhaps we should
-- perform the inlining before gradding.
gradE adp s (Call f [arg])
  | f `isThePrimFun` "$inline"
  = gradE adp s arg

-- grad[ build (\i.e ]
--  = B (\i. let Di = 0 in grad[e])
-- We need the Di binding in case 'i' is mentioned in
-- grad[e], e.g. build (\i. power(x, i))
gradE adp s (Call f [n, Lam ti body])
  | f `isThePrimFun` "build"
  = gradBuild adp s n ti body

-- I'm not very happy about this rule, which effectively
-- undoes sum (build e) --> sumbuild e
gradE adp s (Call f [n, body])
  | f `isThePrimFun` "sumbuild"
  = gradE adp s (pSum (pBuild n body))

gradE adp s (Call f [n, Lam ti body])
  | f `isThePrimFun` "build"
  = gradBuild adp s n ti body

gradE adp s (Call f args) = gradCall adp s f args

---------------
gradBuild :: ADPlan -> Type -> TExpr -> TVar -> TExpr -> TExpr
gradBuild BasicAD s n ti body
  = lmVCatV $
    pBuild n $ Lam ti $
    mkLet (gradTVar BasicAD s ti) (lmZero s (typeof ti)) $
    gradE BasicAD s body

gradBuild TupleAD s n ti body
  = mkLet p (pUnzip (pBuild n (Lam ti grad_body))) $
    Tuple [ pFst (Var p)
          , lmVCatV (pSnd (Var p)) ]
  where
     t_ty = typeof body
     p = TVar res_ty resVar
     res_ty = TypeTuple [ TypeVec n t_ty, TypeVec n (TypeLM s t_ty) ]
     grad_body = mkLet (gradTVar TupleAD s ti)
                       (Tuple [Var ti, lmZero s (typeof ti)]) $
                 gradE TupleAD s body
---------------
gradCall :: ADPlan -> Type -> TFun -> [TExpr] -> TExpr
gradCall BasicAD s f args
  = lmCompose (Call gf args) (lmVCat (map (gradE BasicAD s) args))
  where
    gf = gradTFun BasicAD f (map typeof args)

gradCall TupleAD s f args
  = mkLets [p | (_, _, Just p) <- grad_arg_binds] $
    mkLet res_tv
          (Call gf [ pFstga | (_,pFstga,_) <- grad_arg_binds ]) $
    Tuple [ pFst (Var res_tv)
          , lmCompose (pSnd (Var res_tv))
                      (lmVCat [ pSnd ga | (ga, _, _) <- grad_arg_binds ])
          ]
  where
    gf     = gradTFun TupleAD f (map typeof args)
    res_ty = typeof f
    res_tv = mkGradTVar TupleAD (mkTupleTy arg_tys) resVar res_ty
    arg_tys = map typeof args

    grad_arg_binds :: [(TExpr, TExpr, Maybe (TVar,TExpr))]
     -- Nothing <=> the arg is atomic, so no need to let-bind
    grad_arg_binds = zipWith mk_grad_arg_bind args [1..]

    mk_grad_arg_bind arg i
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
        arg_var  = mkGradTVar TupleAD s (mkArgVar i) (typeof arg)
        grad_arg = gradE TupleAD s arg

----------------------
gradLet :: HasCallStack => ADPlan -> Type -> TVar -> TExpr -> TExpr -> TExpr
gradLet BasicAD s v e1 e2
  = mkLet v e1                                        $
    mkLet (gradTVar BasicAD s v) (gradE BasicAD s e1) $
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

-- Forward
--   D$f  :: S1 S2       -> ((S1,S2) -o T)
--  Dt$f  :: S1 S2       -> (T, (S1,S2) -o T)
-- fwd$f  :: S1 S2 dS1 dS2 -> dT
-- fwdt$f :: S1 S2 dS1 dS2 -> (T, dT)
applyD Fwd (Def { def_fun = GradFun f adp, def_res_ty = res_ty
                , def_args = vars, def_rhs = UserRhs rhs })
  = Def { def_fun    = DrvFun f (AD adp Fwd)
        , def_args   = vars ++ dvars
        , def_rhs    = UserRhs $ perhapsFstToo $ lmApply lm $ mkTuple $ map Var dvars
        , def_res_ty = t }
  where
    dvars = map to_delta vars
    to_delta (TVar ty (Simple x)) = TVar (tangentType ty) (Delta x)
    (perhapsFstToo, lm, t)  -- lm :: s -o t
        = case (adp, res_ty) of
            (BasicAD, TypeLM _ t)       -> (id, rhs, tangentType t)
            (TupleAD, TypeTuple [t, _]) -> ((\lmrhs -> Tuple [pFst rhs, lmrhs]),
                                            pSnd rhs,
                                            TypeTuple [t, tangentType t])

--   D$f :: S1 S2    -> ((S1,S2) -o T)
-- rev$f :: S1 S2 dT -> (dS1,dS2)
applyD Rev (Def { def_fun = GradFun f adp, def_res_ty = res_ty
                , def_args = vars, def_rhs = UserRhs rhs })
  = Def { def_fun    = DrvFun f (AD adp Rev)
        , def_args   = vars ++ [dr]
        , def_rhs    = UserRhs $ lmApplyR (Var dr) lm
        , def_res_ty = tangentType (mkTupleTy (map typeof vars)) }
  where
    dr = TVar (tangentType t) $ Delta "r"
    (lm, t)  -- lm :: s -o t
        = case (adp, res_ty) of
            (BasicAD, TypeLM _ t)       -> (rhs,      t)
            (TupleAD, TypeTuple [t, _]) -> (pSnd rhs, t)

-------------------------------

applyD _ def = def

applyDefs :: ADDir -> [TDef] -> [TDef]
applyDefs dir = map (applyD dir)


----------------------------------
--- Unit test

{-

-- TODO make this work
test_AD =
  hspec $ do
    let e1 = runParserOrPanic pDef "(def f ((x : Float)) (* x 2.0))"
    let (env,ae1) = annotDef e1
    let de1_expected = runParserOrPanic pDef "(def D$f ((x : Float)) (lmScale Float 2.0))"
    let (env1,ade1_expected) = annotDef de1_expected
    let ade1 = gradDef emptyST ae1
    describe "AD" $ do
      it "AD" $
        ade1 `shouldBe` ade1_expected
-}
