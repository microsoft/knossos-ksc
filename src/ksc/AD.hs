{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AD where

import Lang
import LangUtils
import Prim
import GHC.Stack

-- for unit test
--import Test.Hspec

--------------- Generate names for gradded indentifiers

gradF :: HasCallStack => Fun -> Fun
gradF (Fun f) = GradFun f Fwd
gradF f       = error ("gradF: bad function: " ++ show f)

gradV :: Var -> Var
gradV (Simple x) = Grad x Fwd
gradV v          = error ("gradV: bad variable: " ++ render (ppr v))

gradSelFun :: [TVar] -> TVar -> Int -> TExpr
-- (gradSelFun i n) selects the i'th component of a n-tuple
-- Result expr has type (t1, ..., tn) -o ti
gradSelFun params pi i
  = lmHCat [ if i == j then lmOne $ typeof pi
                       else lmZero (typeof pj) (typeof pi)
           | (pj,j) <- params `zip` [1..] ]

-------------------------------------------------

isUserFunDef :: TDef -> Bool
isUserFunDef (Def { def_fun = Fun _
                  , def_rhs = UserRhs {} })
               = True
isUserFunDef _ = False

filterGradFuns :: [TDef] -> [TDef]
filterGradFuns = filter isUserFunDef

gradDefs :: HasCallStack => [TDef] -> [TDef]
gradDefs = map gradDef . filterGradFuns

gradDef :: HasCallStack => TDef -> TDef
gradDef (Def { def_fun = f, def_args = params
             , def_rhs = UserRhs rhs, def_res_ty = res_ty })
  = Def { def_fun    = gradF f
        , def_args   = params
        , def_res_ty = TypeLM s res_ty
        , def_rhs    = UserRhs (mkLets lets (gradE s rhs)) }
  where
    param1 :: TExpr
    param1 = mkTuple (map Var params)
    s = typeof param1

    plets = case params of
             [p] -> [ (gradTVar s p, lmOne (typeof p)) ]
             _   -> [ (gradTVar s p, gradSelFun params p i)
                    | (p,i) <- params `zip` [1..] ]

    szlets = [ (gradTVar s p, lmZero s TypeSize) | p <- paramsSizeBinders params]

    lets = plets ++ szlets

gradDef def = pprPanic "gradDef" (ppr def)
   -- The fitlterGradFuns should make this impossible


-- s -> (Expr :: t) -> (Expr :: s -o t)
gradE :: HasCallStack => Type -> TExpr -> TExpr
gradE s e@(Konst _)    = lmZero s (typeof e)
gradE s (Var tv)       = Var (gradTVar s tv)
gradE s (Assert e1 e2) = Assert e1 (gradE s e2)
gradE s (Tuple es)     = lmVCat (map (gradE s) es)
gradE s (If b t e)     = If b (gradE s t) (gradE s e)
gradE _ e@(Lam {})     = pprPanic "gradE: can't deal with lambda yet" (ppr e)
gradE s (Let v e1 e2)  = mkLet v e1                       $
                         mkLet (gradTVar s v) (gradE s e1) $
                         gradE s e2

-- grad[ build (\i.e ]
--  = B (\i. let Di = 0 in grad[e])
-- We need the Di binding in case 'i' is mentioned in
-- grad[e], e.g. build (\i. power(x, i))
gradE s (Call f [n, Lam ti body])
  | f `isThePrimFun` "build"
  = lmBuild n $ Lam ti $
    mkLet (gradTVar s ti) (lmZero s (typeof  ti)) $
    gradE s body

-- Currently ignoring $inline when gradding.  Perhaps we should
-- perform the inlining before gradding.
gradE s (Call f [arg])
  | f `isThePrimFun` "$inline"
  = gradE s arg

gradE s (Call f args)
  = Call gf args `lmCompose` lmVCat (map (gradE s) args)
  where
    gf = gradTFun (mkTupleTy (map typeof args)) f

gradTFun :: HasCallStack => Type -> TFun -> TFun
gradTFun arg_ty (TFun res_ty f)
  = TFun (TypeLM arg_ty res_ty) (gradF f)

gradTVar :: Type -> TVar -> TVar
gradTVar s (TVar ty v) = TVar (TypeLM s ty) (gradV v)

---------------------------------

-- Apply-to-dx
--
-- Local bindings of type (S -o T)
-- are transformed to ones of type T

applyD :: TDef -> TDef
--   R$f :: S1 S2   -> (T -o (S1,S2))
-- rev$f :: S1 S2 T -> (S1,S2)
applyD (Def { def_fun = GradFun f Rev, def_res_ty =  TypeLM s t
            , def_args = vars, def_rhs = UserRhs rhs })
  = Def { def_fun = DrvFun f Rev, def_res_ty = tangentType t
        , def_args = vars ++ [dr]
        , def_rhs  = UserRhs $ lmApply rhs $ Var dr }
  where
    dr = TVar (tangentType s) $ Delta "r"

--   D$f :: S1 S2       -> ((S1,S2) -o T)
-- rev$f :: S1 S2 S1 S2 -> T
applyD (Def { def_fun = GradFun f Fwd, def_res_ty = TypeLM _ t
            , def_args = vars, def_rhs = UserRhs rhs })
  = Def { def_fun = DrvFun f Fwd, def_res_ty = tangentType t
        , def_args = vars ++ dvars
        , def_rhs = UserRhs $ lmApply rhs $ mkTuple $ map Var dvars }
  where
    dvars = map to_delta vars
    to_delta (TVar ty (Simple x)) = TVar (tangentType ty) (Delta x)

applyD def = def

applyDefs :: [TDef] -> [TDef]
applyDefs = map applyD

---------------------------------
-- Transpose

transposeD :: TDef -> TDef
transposeD (Def { def_fun = GradFun f d, def_res_ty = TypeLM s t
                , def_args = args, def_rhs = UserRhs rhs })
  = Def { def_fun = GradFun f (flipMode d), def_res_ty = TypeLM t s
         , def_args = args
         , def_rhs = UserRhs $ lmTranspose rhs }

transposeD def = def

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
