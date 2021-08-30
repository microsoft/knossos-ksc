{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Ksc.SUF.AD where

import           Ksc.SUF (L2(L2), L3(L3), L4(L4), L8(L8))

import           Ksc.Lang
import           Ksc.LangUtils (notInScopeTVs, stInsertFun, GblSymTab,
                            freeVarsOf, lookupGblST, InScopeSet, mkInScopeSet,
                            extendInScopeSet)
import           Ksc.Prim

import qualified Data.Set as S
import           Data.Maybe (mapMaybe)
import           Data.Traversable (mapAccumL)

type SUFFwdRevPass =
  GblSymTab
  -- ^ Global symbol table, only used for looking up the
  -- [suffwdpass ...] and [sufrevpass ...] versions of a
  -- function in a Call
  -> InScopeSet
  -- ^ Variables in scope
  -> TExpr
  -- ^ Input expression e
  -> (TExpr,
      -- ^ The SUF forward pass output expression FWD{e}
      Type,
      -- ^ The type of the BOG
      SUFRevPass)
      -- ^ The generator for the SUF reverse pass output expression REV{e}

type SUFRevPass =
  InScopeSet
  -- ^ Variables in scope
  -> TExpr
  -- ^ the incoming gradient
  -> TExpr
  -- ^ The BOG corresponding to the input expression
  -> (InScopeSet,
      -- ^ Variables in scope after the reverse pass output bindings
      [(Pat, TExpr)])
      -- ^ The SUF reverse pass output bindings

-- Generate the SUF/BOG-AD foward and reverse pass from an expression
-- already in Single Use Form (see Ksc.SUF for more details on Single
-- Use Form).  The type invariants are
--
-- * If G |- e :: T, then
--
-- * BOG_e is a type which depends on (the structure of) e
--
-- * G |- FWD{e} :: (T, BOG_e)
--
-- * If dt :: dT, b :: BOG_e, then REV{e} dt b is a set of bindings
--   which, for every free variable x : X in G, binds dx : dX.
--
-- See Note [Automatic differentiation documentation]
sufFwdRevPass :: SUFFwdRevPass
sufFwdRevPass gst subst = \case
  -- FWD{k} = (k, ())
  -- REV{k} dt b = {}
  e@(Konst _) -> noBog e (\avoid' _ _ -> (avoid', []))

  -- FWD{v} = (v, ())
  -- REV{v} dt b = { dv = dt }
  Var v -> (Tuple [Var v, Tuple []],
             TypeTuple [],
             \avoid' dt _ -> (S.insert (tVarVar dv) avoid', [(VarPat dv, dt)]))
    where dv = deltaOfSimple v

  -- FWD{let pat = rhs in body} = let (pat, b_rhs)    = FWD{rhs}
  --                                  (bodyv, b_body) = FWD{body}
  --                              in (bodyv, (b_rhs, b_body))
  --
  -- REV{let pat = rhs in body} dt b = { (b_rhs, b_body) = b }
  --                                ++ REV{body} dt   b_body
  --                                ++ REV{rhs}  dpat b_rhs
  Let p rhs body -> (gradf_let, typeof bog, sufRevPass_)
    where (subst2, L4 bodyv b_rhs p_var b_body) =
            notInScopeTVs subst (L4 (TVar (typeof body) (Simple "body"))
                                    (TVar rhs_bog_ty (Simple "b_rhs"))
                                    (TVar (typeof rhs) (Simple "p_var"))
                                    (TVar body_bog_ty (Simple "b_body")))

          subst3 = foldr extendInScopeSet subst2 p

          (fwdpass_rhs, rhs_bog_ty, revpass_rhs) = sufFwdRevPass gst subst3 rhs
          (fwdpass_body, body_bog_ty, revpass_body) = sufFwdRevPass gst subst3 body

          bog = mkBog [b_rhs, b_body]

          gradf_let = Let (TupPat [p_var, b_rhs]) fwdpass_rhs
                    $ Let p (Var p_var)
                    $ Let (TupPat [bodyv, b_body]) fwdpass_body
                    $ Tuple [Var bodyv, bog]

          sufRevPass_ avoid' dt b = (avoid'3, let_:rest)
            where (avoid'2, L2 b_rhs_ b_body_) =
                    notInScopeTVs avoid' (L2 (TVar rhs_bog_ty (Simple "b_rhs"))
                                             (TVar body_bog_ty (Simple "b_body")))

                  let_ = mkBogBind [b_rhs_, b_body_] b

                  (avoid'3, rests) =
                    mapAccumL (\avoid_ (dt_, b_, sufRevPasse) ->
                      sufRevPasse avoid_ dt_ b_)
                        avoid'2
                        [ (dt, possiblyEmptyBogExpr b_body_, revpass_body)
                        , (dp, possiblyEmptyBogExpr b_rhs_, revpass_rhs) ]

                  rest = concat rests

                  dp = patToExpr (fmap deltaOfSimple p)

  -- { TODO: We currently just ignore $inline and $trace.  We should
  -- decide what we do with them.
  Call f e | f `isThePrimFun` P_inline -> sufFwdRevPass gst subst e
  Call f e | f `isThePrimFun` P_trace -> sufFwdRevPass gst subst e
  -- }

  theCall@(Call f (Tuple [theLambda@(Lam i body), e1])) | f `isThePrimFun` P_map ->
    sufFwdRevPass_map gst subst theCall theLambda i body e1

  theCall@(Call f (Tuple [e1, theLambda@(Lam i e2)])) | f `isThePrimFun` P_build ->
    sufFwdRevPass_build gst subst theCall e1 theLambda i e2


  -- FWD{f e} = let (a, b_e) = FWD{e}
  --                (r, b_f) = [suffwdpass f] a
  --            in (r, (b_e, b_f))
  --
  -- REV{f e} dt b = { (b_e, b_f) = b
  --                 ; da = [sufrevpass f] dt b_f }
  --              ++ REV{e} da b_e
  Call (TFun res_ty f) e -> (gradf_call_f_e, typeof bog, sufRevPass_)
    where (subst2, L4 arg b_e r b_f) =
            notInScopeTVs subst (L4 (TVar typeof_e (Simple "arg"))
                                    (TVar e_bog_ty (Simple "b_e"))
                                    (TVar res_ty (Simple "r"))
                                    (TVar f_bog_ty (Simple "b_f")))

          (fwdpass_e, e_bog_ty, revpass_e) = sufFwdRevPass gst subst2 e

          (fwdpass_f, revpass_f, f_bog_ty) = mkSufFuns gst f typeof_e

          bog = mkBog [b_e, b_f]

          gradf_call_f_e = Let (TupPat [arg, b_e]) fwdpass_e
                         $ Let (TupPat [r, b_f]) (Call fwdpass_f (Var arg))
                         $ Tuple [Var r, bog]

          typeof_e = typeof e

          sufRevPass_ avoid' dt b = (avoid'3, lets ++ rest)
            where (avoid'2, L3 b_e_ b_f_ da_) =
                    notInScopeTVs avoid' (L3 (TVar e_bog_ty (Simple "be"))
                                             (TVar f_bog_ty (Simple "bf"))
                                             (TVar (typeof revpass_f) (Delta "a")))

                  lets = [ mkBogBind [b_e_, b_f_] b
                         , (VarPat da_, Call revpass_f (Tuple [dt, possiblyEmptyBogExpr b_f_])) ]

                  (avoid'3, rest) = revpass_e avoid'2 (Var da_) (possiblyEmptyBogExpr b_e_)

  -- FWD{(e1, e2)} = let (a1, b1) = FWD{e1}
  --                     (a2, b2) = FWD{e2}
  --                 in ((a1, a2), (b1, b2))
  --
  -- REV{(e1, e2)} dt b = { (dt1, dt2) = dt
  --                      , (b1, b2) = b }
  --                   ++ REV{e1} dt1 b1
  --                   ++ REV{e2} dt2 b2
  Tuple ts -> (es, typeof bs, sufRevPass_)
    where (_, binds_tys) =
            mapAccumL (\subst' e ->
              let (subst'', L2 a b_e) =
                    notInScopeTVs subst'
                    (L2 (TVar (typeof e) (Simple "a"))
                         (TVar bog_ty (Simple "b")))
                  (fwdpass_e, bog_ty, revpass_e) = sufFwdRevPass gst subst e
              in (subst'', (a, b_e, Let (TupPat [a, b_e]) fwdpass_e, bog_ty, revpass_e)))
            subst ts

          as = Tuple (map (Var . fst4) binds_tys)
            where fst4 (a, _, _, _, _) = a

          bs = mkBog (map bogVar binds_tys)
            where bogVar (_, b, _, _, _) = b

          lets_list = map thd4 binds_tys
            where thd4 (_, _, a, _, _) = a

          lets = foldr (.) id lets_list

          es = lets (Tuple [as, bs])

          sufRevPass_ avoid' dt b = (avoid'4, lets_ ++ rest)
            where (avoid'3, revpass_e_bs_) =
                    mapAccumL (\is (a, _, _, bog_ty, revpass_e) ->
                                 let (is', L2 b_e_ dt_) = notInScopeTVs is
                                       (L2 (TVar bog_ty (Simple "b"))
                                           (TVar (tangentType (typeof a)) (Delta "t")))
                                 in (is', (dt_, revpass_e, b_e_)))
                              avoid'
                              binds_tys

                  dts = map (\(dt_, _, _) -> dt_) revpass_e_bs_
                  bogLet = mkBogBind (map bogVar revpass_e_bs_) b
                    where bogVar (_, _, b_) = b_

                  lets_ = [ (TupPat dts, dt), bogLet ]

                  (avoid'4, rests) = mapAccumL
                    (\avoid_ (dt_, sufRevPasse, b_) ->
                        let (avoidM, binds) =
                              sufRevPasse avoid_ (Var dt_) (possiblyEmptyBogExpr b_)
                        in (avoidM, binds))
                      avoid'3 revpass_e_bs_

                  rest = concat rests

  -- FWD{if cond then t else f} = let (cond_v, b_cond) = FWD{cond}
  --                              in if cond_v then let (t_v, b_t) = FWD{t} in (t_v, (b_cond, Left b_t))
  --                                           else let (f_v, b_f) = FWD{f} in (t_f, (b_cond, Right b_f))
  --
  -- REV{if cond then t else f} dt b = { (b_cond, cond, b_t, b_f) = b
  --                                   ; dvs = if cond then let REV{t} dt b_t in dvs
  --                                                   else let REV{f} dt b_f in dvs }
  --                                ++ REV{cond} () b_cond
  --
  -- (vs is the list of variables that occur free in each branch of
  -- the if.  N.B. the Single Use Property implies that the free
  -- variable set is the same for each branch.)
  --
  -- N.B. Until we have proper sum types we use products with a dummy
  -- component to simulate sums.  Specifically, we use the following
  -- encodings:
  --
  -- (b_cond, Left b_t)   -> (b_cond, cond, b_t, dummy)
  -- (b_cond, Right b_f)  -> (b_cond, cond, dummy, b_f)
  If econd et ef ->
    let (fwdpass_cond, cond_bog_ty, sufRevPassecond) = sufFwdRevPass gst subst econd

        (subst1, L2 cond b_cond) =
          notInScopeTVs subst (L2 (TVar TypeBool (Simple "cond"))
                                  (TVar cond_bog_ty (Simple "bcond")))

        (substt2, L2 rt b_t) =
          notInScopeTVs subst1 (L2 (TVar (typeof et) (Simple "rt"))
                                   (TVar t_bog_ty (Simple "bt")))

        (substf2, L2 rf b_f) =
          notInScopeTVs subst1 (L2 (TVar (typeof ef) (Simple "rf"))
                                   (TVar f_bog_ty (Simple "bf")))

        (fwdpass_t, t_bog_ty, revpass_t) = sufFwdRevPass gst substt2 et
        (fwdpass_f, f_bog_ty, revpass_f) = sufFwdRevPass gst substf2 ef

        inl :: TExpr -> Type -> TExpr
        inl l tyr = mkBogE [Var b_cond, true, l, Dummy tyr]
          where true  = Konst (KBool True)

        inr :: Type -> TExpr -> TExpr
        inr tyl r = mkBogE [Var b_cond, false, Dummy tyl, r]
          where false = Konst (KBool False)

        if_bog_t = inl (Var b_t) f_bog_ty
        if_bog_f = inr t_bog_ty (Var b_f)

        bog_ty = if typeof if_bog_t `eqType` typeof if_bog_f
                 then typeof if_bog_t
                 else error "Mismatched bog types in if"

        sufRevPass_ avoid' dt b =
            let v_bar = map deltaOfSimple (S.toList (freeVarsOf et))
                _v_bar' = map deltaOfSimple (S.toList (freeVarsOf ef))

                !_ = if v_bar /= _v_bar'
                     then error "sufRevPass: If: branches had different free variables"
                     else ()

                v_bar_expr :: TExpr
                v_bar_expr = Tuple (map Var v_bar)

                (avoid'2, L4 b_cond_ cond_ b_t_ b_f_) =
                  notInScopeTVs avoid' (L4 (TVar cond_bog_ty (Simple "bcond"))
                                           (TVar TypeBool (Simple "cond"))
                                           (TVar t_bog_ty (Simple "bt"))
                                           (TVar f_bog_ty (Simple "bf")))

                (_, gradr_et) = revpass_t avoid'2 dt (possiblyEmptyBogExpr b_t_)
                (_, gradr_ef) = revpass_f avoid'2 dt (possiblyEmptyBogExpr b_f_)

                (avoid'3, rest) = sufRevPassecond avoid'2 (Tuple []) (possiblyEmptyBogExpr b_cond_)

            in (avoid'3,
                [ mkBogBind [b_cond_, cond_, b_t_, b_f_] b
                , (TupPat v_bar,
                    If (Var cond_)
                       (mkLets_ gradr_et v_bar_expr)
                       (mkLets_ gradr_ef v_bar_expr)) ]
                ++ rest
                )

    in (Let (TupPat [cond, b_cond]) fwdpass_cond
        $ If (Var cond)
           (Let (TupPat [rt, b_t]) fwdpass_t (Tuple [Var rt, if_bog_t]))
           (Let (TupPat [rf, b_f]) fwdpass_f (Tuple [Var rf, if_bog_f])),
        bog_ty,
        sufRevPass_)

  Dummy{}  -> unimplemented "Dummy"
  Lam{}    -> unimplemented "Lam"
  App{}    -> unimplemented "App"
  Assert{} -> unimplemented "Assert"
  where noBog e sufRevPass_ = (Tuple [e, Tuple []], TypeTuple [], sufRevPass_)
        unimplemented s = error ("sufFwdRevPass: unimplemented: " ++ s)

-- FWD{map (\x. body) e} = let (e_v, b_e) = FWD{e}
--                             (r, b_a) = suffwdpass_map (\x. FWD{body}, e_v)
--                         in (r, (b_e, b_a))
--
-- REV{map (\x. body) e} dt b = { (b_e, b_a) = b
--                              ; (de, dvs) = sufrevpass_map (zero_of_shape_dvs,
--                                                            \(dt1, b1).
--                                                              let REV{body} dt1 b1
--                                                              in (dx, dvs),
--                                                            dt,
--                                                            b) }
--                           ++ REV{e} de b_e
sufFwdRevPass_map :: GblSymTab
                  -> InScopeSet
                  -> TExpr
                  -> TExpr
                  -> TVarX
                  -> TExpr
                  -> TExpr
                  -> (TExpr, Type,
                      S.Set Var
                   -> TExpr
                   -> TExpr
                   -> (S.Set Var, [(PatG TVar, TExpr)]))
sufFwdRevPass_map gst in_scope theCall theLambda x body e =
  (gradf_map, typeof bog, sufRevPass_)
  where (in_scope2, L4 ev be r ba) =
          notInScopeTVs in_scope (L4 (TVar (typeof e) (Simple "ev"))
                                     (TVar gradfe_bog_ty (Simple "be"))
                                     (TVar type_r (Simple "r"))
                                     (TVar type_ba (Simple "ba")))

        (gradfe, gradfe_bog_ty, sufRevPasse) = sufFwdRevPass gst in_scope2 e
        (gradfbody, gradfbody_bog_ty, sufRevPassBody) = sufFwdRevPass gst in_scope2 body

        freeVarsOfMap = S.toList (freeVarsOf theLambda)
        freeVarsOfMapZero = mkTangentZero (Tuple (map Var freeVarsOfMap))

        typeTensor = case typeof e of
          TypeTensor i _ -> TypeTensor i
          unexpectedTy -> pprPanic "Unexpected type for map argument" (ppr unexpectedTy)

        type_ba = typeTensor gradfbody_bog_ty
        type_r = typeof theCall

        bog = Tuple [freeVarsOfMapZero, Var be, Var ba]

        fwdpass_map = mkPrimCall P_suffwdpass_map (Tuple [ Lam x gradfbody
                                                         , Var ev
                                                         ])

        gradf_map = Let (TupPat [ev, be]) gradfe
                  $ Let (TupPat [r, ba]) fwdpass_map
                  $ Tuple [Var r, bog]

        sufRevPass_ in_scope' dt b =
          let v_bar = map deltaOfSimple freeVarsOfMap
              v_bar_expr = Tuple (map Var v_bar)
              dx = deltaOfSimple x

              lamvar_ty = TypeTuple [typeof dt1, typeof b1]
              pat_ty = typeof v_bar_expr

              (in_scope'2, L8 de pat lamvar bfreevars_ be_ ba_ dt1 b1) =
                notInScopeTVs in_scope' (L8 (TVar (tangentType (typeof e)) (Delta "e"))
                                            (TVar pat_ty (Simple "pat"))
                                            (TVar lamvar_ty (Simple "lamvar"))
                                            (TVar (typeof v_bar_expr) (Simple "bfreevars"))
                                            (TVar gradfe_bog_ty (Simple "be"))
                                            (TVar type_ba (Simple "ba"))
                                            (TVar (tangentType (typeof body)) (Simple "dt1"))
                                            (TVar gradfbody_bog_ty (Simple "bbody")))

              (in_scope'3, elets) = sufRevPasse in_scope'2 (Var de) (Var be)

              (in_scope'4, innerLets) = sufRevPassBody in_scope'3 (Var dt1) (Var b1)

              sufrevpass_map = mkPrimCall P_sufrevpass_map (Tuple [ Var bfreevars_
                                                                  , Lam lamvar
                                                                    $ Let (TupPat [dt1, b1]) (Var lamvar)
                                                                    $ mkLets_ innerLets (Tuple [Var dx, v_bar_expr])
                                                                  , dt
                                                                  , Var ba_
                                                                  ])

              lets = [ (TupPat [bfreevars_, be_, ba_], b)
                     , (TupPat [de, pat], sufrevpass_map)
                     , (TupPat v_bar, Var pat)
                     ]

          in (in_scope'4, lets ++ elets)

-- FWD{build e1 (\i. e2)} = let (n, b_n) = FWD{e1}
--                              (r, b_a) = unzip(build n (\i. FWD{e2}))
--                          in (r, (n, b_n, b_a))
--
-- REV{build e1 (\i. e2)} dt b = { (n, b_n, b_a) = b
--                               ; dvs = sumbuild n (\i. let REV{e2} dt[i] b_a[i] in dvs) }
--                            ++ REV{e1} () b_n
--
-- (vs is the list of variables that appears free in the lambda)
sufFwdRevPass_build :: GblSymTab
                    -> InScopeSet
                    -> TExpr
                    -> TExpr
                    -> TExpr
                    -> TVarX
                    -> TExpr
                    -> (TExpr, Type,
                         S.Set Var
                        -> TExpr
                        -> TExpr
                        -> (S.Set Var, [(PatG TVar, TExpr)]))
sufFwdRevPass_build gst subst theCall e1 theLambda i e2 =
    (gradf_build, typeof bog, sufRevPass_)
    where (subst2, L4 n r bn ba) =
            notInScopeTVs subst (L4 (TVar typeIndex (Simple "n"))
                                    (TVar type_r (Simple "r"))
                                    (TVar gradfe1_bog_ty (Simple "bn"))
                                    (TVar type_ba (Simple "ba")))

          (gradfe1, gradfe1_bog_ty, sufRevPasse1) = sufFwdRevPass gst subst2 e1
          (gradfe2, gradfe2_bog_ty, sufRevPasse2) = sufFwdRevPass gst subst2 e2

          typeIndex = typeof i
          (typeTensor, unitOfIndexType) = case typeof theCall of
            TypeTensor d _ ->
              (TypeTensor d, mkTangentZero (zeroIndexForDimension d))
            unexpectedTy -> pprPanic "Unexpected tensor index type" (ppr unexpectedTy)

          type_ba = typeTensor gradfe2_bog_ty
          type_r  = typeof theCall

          bog = Tuple [Var n, Var bn, Var ba]

          gradf_build = Let (TupPat [n, bn]) gradfe1
                      $ Let (TupPat [r, ba]) (pUnzip (pBuild (Var n) (Lam i gradfe2)))
                      $ Tuple [Var r, bog]

          sufRevPass_ avoid' dt b =
            let v_bar = map deltaOfSimple (S.toList (freeVarsOf theLambda))
                v_bar_expr :: TExpr
                v_bar_expr = Tuple (map Var v_bar)

                (avoid'2, L3 n_ ba_ bn_) =
                  -- Maybe we could share construction of these
                  -- variables between the fwd pass and the rev pass.
                  notInScopeTVs avoid' (L3 (TVar typeIndex (Simple "n"))
                                           (TVar type_ba (Simple "ba"))
                                           (TVar gradfe1_bog_ty (Simple "bn")))

                (avoid'3, sizeLets) = sufRevPasse1 avoid'2 unitOfIndexType (Var bn_)

                innerLets :: [(Pat, TExpr)]
                (avoid'4, innerLets) = sufRevPasse2
                                                 avoid'3
                                                 (pIndex (Var i) dt)
                                                 (pIndex (Var i) (Var ba_))

                lets = [ (TupPat [n_, bn_, ba_], b)
                       , (TupPat v_bar,
                          (pSumBuild (Var n_)
                                     (Lam i (mkLets_ innerLets v_bar_expr ))))
                       ]

            in (avoid'4, lets ++ sizeLets)

mkBog :: [TVar] -> TExpr
mkBog = mkBogE . fmap Var

mkBogE :: [TExpr] -> TExpr
mkBogE = mkTuple . mapMaybe possiblyEmptyBogHasType

mkBogBind :: [TVar] -> TExpr -> (PatG TVar, TExpr)
mkBogBind bogVars b = (mkPat @Typed (mapMaybe possiblyEmptyBogVar bogVars), b)

possiblyEmptyBogHasType :: HasType a => a -> Maybe a
possiblyEmptyBogHasType v = if typeof v `eqType` TypeTuple []
                            then Nothing
                            else Just v

possiblyEmptyBogVar :: TVar -> Maybe TVar
possiblyEmptyBogVar = possiblyEmptyBogHasType

possiblyEmptyBogExpr :: TVar -> TExpr
possiblyEmptyBogExpr v = if typeof v `eqType` TypeTuple []
                         then Tuple []
                         else Var v

mkLets_ :: [(Pat, TExpr)] -> TExpr -> TExpr
mkLets_ t e = foldr (uncurry Let) e t

-- f : S -> T
--
-- fwdpass$f : S -> (T, B{f})
--
-- B{f} is the type of the BOG of f
--
-- revpass$f : (dT, B{f}) -> dS
sufFwdRevPassDef :: GblSymTab -> TDef -> (Maybe TDef, Maybe TDef, GblSymTab)
sufFwdRevPassDef gst Def{ def_fun    = Fun JustFun f
                        , def_pat    = s
                        , def_rhs    = UserRhs rhs
                        , def_res_ty = t_ty
                        }
  = let
      (rhs', bog_ty, mkRev) =
        sufFwdRevPass gst (mkInScopeSet (patVars s)) rhs

      fwd = Def { def_fun    = Fun SUFFwdPass f
                , def_pat    = s
                , def_rhs    = UserRhs rhs'
                , def_res_ty = TypeTuple [t_ty, bog_ty]
                }

      rev = Def { def_fun    = Fun SUFRevPass f
                , def_pat    = TupPat [ dt, bog ]
                , def_rhs    = UserRhs rhs''
                , def_res_ty = ds_ty
                }
       where
        bog  = TVar bog_ty (Simple "bog_arg")
        dt = TVar dt_ty (Simple "dt_arg")
        ds_ty = tangentType (patType s)
        dt_ty = tangentType t_ty
        rhs'' = lets (patToExpr (fmap deltaOfSimple s))
          where (_, lets_) = mkRev avoid (Var dt) (Var bog)
                lets e = foldr (uncurry Let) e lets_
                avoid = deltaVarsOfPat s `S.union` S.fromList (map tVarVar [dt, bog])

                deltaVarsOfPat :: Pat -> S.Set Var
                deltaVarsOfPat = S.fromList . map (tVarVar . deltaOfSimple) . patVars

      gst' = (stInsertFun rev . stInsertFun fwd) gst
  in (Just fwd, Just rev, gst')


sufFwdRevPassDef gst _ = (Nothing, Nothing, gst)

sufRevDef :: GblSymTab -> TDef -> Maybe TDef
sufRevDef gst Def{ def_fun    = fun@(Fun JustFun f)
                 , def_pat    = s
                 -- We don't actually use the rhs.  We just look up
                 -- the fwdpass and revpass in the GblSymTab.
                 , def_rhs    = UserRhs _
                 , def_res_ty = t_ty
                 } =
  Just $ Def { def_fun    = Fun SUFRev f
             , def_pat    = TupPat [s_var, dt]
             , def_rhs    = UserRhs rhs'
             , def_res_ty = d s_ty
             }
  where s_ty = patType s
        d = tangentType
        s_var = TVar s_ty (Simple "sarg")
        t  = TVar t_ty (Simple "tres")
        dt = TVar (d t_ty) (Simple "dtarg")
        ds = TVar (d s_ty) (Simple "dsres")
        bog_var = TVar bog_ty (Simple "bog")

        (fwdPass, revPass, bog_ty) =
          mkSufFuns gst (userFunToFun fun) (typeof s)

        rhs' = Let (TupPat [t, bog_var]) (pInline (Call fwdPass (Var s_var)))
             $ Let (VarPat ds) (pInline (Call revPass (Tuple [Var dt, Var bog_var])))
             $ Var ds

sufRevDef _ _ = Nothing

sufRevDefs :: GblSymTab -> [TDef] -> [TDef]
sufRevDefs = mapMaybe . sufRevDef

deltaOfSimple :: TVar -> TVar
deltaOfSimple = \case
  TVar vty (Simple vname) -> TVar (tangentType vty) (Delta vname)
  _ -> error "Non simple variable"

callResultTy :: GblSymTab -> Fun Typed -> Type -> Either SDoc Type
callResultTy env fun arg_ty = case perhapsUserFun fun of
  Right user -> case lookupGblST user env of
    Just f -> pure (def_res_ty f)
    Nothing -> Left (text "Not in scope" <+> ppr user)
  Left prim -> primCallResultTy_maybe prim arg_ty

-- In the input program we see a call f(e), where e : S and f(e) : T.
-- Then
--
--   * suffwdpass$f : S -> (T, B)
--   * suffrevass$f : (dT, B) -> dS
--
-- (for some bog type B). A call of mkSufFuns like
--
--     mkSufFuns env f S T
--
-- returns (suffwdpass$f, sufrevpass$f, B)
mkSufFuns :: GblSymTab -> Fun Typed -> Type
          -> (TFun Typed, TFun Typed, Type)
mkSufFuns env fun arg_ty = (fwdTFun, revTFun, bog_ty)
  where fwdTFun = TFun fwd_res_ty fwdFun
        revTFun = TFun rev_res_ty (Fun SUFRevPass funid)
        fwdFun = Fun SUFFwdPass funid

        fwd_res_ty = case callResultTy env fwdFun arg_ty of
          Right res_ty' -> res_ty'
          Left err -> pprPanic "mkSufFuns" err

        funid = case fun of
          Fun JustFun funid' -> funid'
          _ -> error $
            unlines [ "We don't yet support SUF/BOG-AD of derived "
                    , "functions, but there's no reason we couldn't. "
                    , "If you'd like them, file an issue." ]

        bog_ty = case fwd_res_ty of
          TypeTuple [_, bogty'] -> bogty'
          unexpected -> pprPanic "Unexpected fwd_res_ty" (ppr unexpected)

        rev_res_ty = tangentType arg_ty
