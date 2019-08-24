{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}

module Ksc.LinearAD where

import qualified Annotate                      as A
import           Parse                          ( parseF )
import qualified Prim
import           KMonad                         ( runKM
                                                , banner
                                                , KM
                                                )
import           Lang                           ( displayN
                                                , Decl
                                                )
import qualified Lang                          as L
import qualified LangUtils                     as LU
--import qualified Opt as O
import qualified Rules
import qualified ANF                           as A

import           Data.Maybe                     ( mapMaybe )

demoF :: String -> IO ()
demoF file = do
  defs <- parseF file
  runKM (demoN defs)

demoN :: [Decl] -> KM ()
demoN decls = do
  banner "Original declarations"
  displayN decls

  (env, tc_decls) <- A.annotDecls LU.emptyGblST decls
  let (rules, defs) = L.partitionDecls tc_decls
      _rulebase     = Rules.mkRuleBase rules

  anf_defs <- A.anfDefs defs
  disp "Anf-ised original definition" env anf_defs

  let linear_defs = map lineariseD anf_defs
  disp "Linear original definition" env linear_defs

  let diff_defs = mapMaybe differentiateD linear_defs
  disp "Derivative" env diff_defs

disp :: String -> LU.GblSymTab -> [L.TDef] -> KM ()
disp what env decls = do
  banner what
  displayN decls
  A.lintDefs what env decls

lineariseD :: L.TDef -> L.TDef
lineariseD tdef@(L.Def { L.def_rhs = L.UserRhs rhs }) = L.Def
  { L.def_fun    = L.def_fun tdef
  , L.def_args   = L.def_args tdef
  , L.def_res_ty = L.def_res_ty tdef
  , L.def_rhs    = L.UserRhs (lineariseE rhs)
  }
lineariseD d@(L.Def { L.def_rhs = L.EDefRhs{} }) = d
lineariseD (L.Def { L.def_rhs = L.StubRhs{} }) =
  error "Didn't expect to see a StubRhs at this stage"

lineariseE :: L.TExpr -> L.TExpr
lineariseE = \case
  L.Let v rhs body -> case rhs of
    L.Call f args -> dups' (L.Let v (L.Call f new_args) (lineariseE body))
     where
      dups = flip map args $ \case
        argvv@(L.Var argv) -> if argv `LU.notFreeIn` body
          then (id, L.Var argv)
          else
            let argv' = LU.newVarNotIn (L.typeof argvv) body
            in  (L.Dup (argv, argv') argvv, L.Var argv')
        lam@(L.Lam{}) -> if f `Prim.isThePrimFun` "build"
          then (id, lam)
          else error "Didn't expect to see lam in Anf form"
        arg -> error ("Unexpected in Anf form " ++ L.render (L.ppr arg))
      dups' :: L.TExpr -> L.TExpr
      new_args :: [L.TExpr]
      (dups', new_args) =
        foldr (\(g, a) (gs, as) -> (g . gs, a : as)) (id, []) dups

    a@(L.Konst{}) -> L.Let v a (lineariseE body)
    a@(L.Var{}  ) -> L.Let v a (lineariseE body)
    a@(L.Tuple{}) -> L.Let v a (lineariseE body)
    -- I can't be bothered to deal with assert so I'm just going to
    -- remove it
    L.Assert cond assertbody -> case cond of
      vv@(L.Var{}) ->
        L.Let v (L.Assert vv (lineariseE assertbody)) (lineariseE body)
      o -> error ("lineariseE Let Assert unexpected " ++ show o)
    -- This is probably quite broken.  We need to be sure that both
    -- branches consume the same variables, otherwise we'll have
    -- problems.
    L.If cond true false -> case cond of
      vv@(L.Var{}) ->
        L.Let v (L.If vv (lineariseE true) (lineariseE false)) body
      o -> error ("lineariseE Let If unexpected " ++ show o)
    o -> error ("lineariseE Let unexpected " ++ show o)
  var@(L.Var{}) -> var
  v             -> error ("lineariseE unexpected " ++ show v)

differentiateD :: L.TDef -> Maybe L.TDef
differentiateD tdef@(L.Def { L.def_fun = L.Fun (L.UserFun f), L.def_rhs = L.UserRhs rhs })
  = Just
    (L.Def { L.def_fun    = L.LinearGradFun (L.UserFun f)
           , L.def_args   = args ++ [rev r]
           , L.def_res_ty = res_ty
           , L.def_rhs    = L.UserRhs d_rhs
           }
    )
 where
  (fwd, r, trace, rev') = differentiateE rhs
  d_rhs = (fwd . rev' trace . L.Tuple) (L.Var r : map (L.Var . rev) args)
  args                  = L.def_args tdef
  res_ty                = L.TypeTuple (L.def_res_ty tdef : map L.typeof args)
differentiateD (L.Def { L.def_fun = L.Fun (L.PrimFun f) }) = error
  (  "Wasn't expecting to be asked to differentiate a PrimFun: "
  ++ L.render (L.ppr f)
  )
differentiateD (L.Def { L.def_fun = L.Fun (L.SelFun{}) }) =
  error "Wasn't expecting to be asked to differentiate a SelFun"
differentiateD (L.Def { L.def_rhs = L.StubRhs }) =
  error "Did not expect to see StubRhs"
-- Some things we don't expect to have to differentiate
differentiateD L.Def { L.def_rhs = L.EDefRhs }         = Nothing
differentiateD L.Def { L.def_fun = L.GradFun{} }       = Nothing
differentiateD L.Def { L.def_fun = L.DrvFun{} }        = Nothing
differentiateD L.Def { L.def_fun = L.LinearGradFun{} } = Nothing

differentiateE
  :: L.TExpr
  -> (L.TExpr -> L.TExpr, L.TVar, [L.TVar], [L.TVar] -> L.TExpr -> L.TExpr)
differentiateE = \case
  L.Dup (v1, v2) (L.Var vv) body ->
    ( L.Dup (v1, v2) (L.Var vv) . body'
    , r
    , xs
    , \xs' -> f xs' . L.Let (rev vv) (revVar v1 .+ revVar v2)
    )
    where (body', r, xs, f) = differentiateE body
  L.Let r rhs body -> case rhs of
    (L.Call (L.TFun _ (L.Fun (L.PrimFun op))) [L.Var a1, L.Var a2]) ->
      case op of
        "add" -> g
          ( L.Let r (v a1 .+ v a2)
          , []
          -- We don't need Dup here.  We could just use Let.  We don't
          -- guarantee that the result of differentiating is linear
          , \xs' -> (xs', L.Dup (rev a1, rev a2) (revVar r))
          )
        "sub" -> g
          ( L.Let r (v a1 .- v a2)
          , []
          , \xs' -> (xs', L.Let (rev a1) (revVar r) . L.Let (rev a2) (revVar r))
          )
        "mul" -> g
          ( L.Let r (v a1 .* v a2)
          , [a1, a2]
          , \(a1_ : a2_ : xs') ->
            ( xs'
            , L.Let (rev a1) (revVar r .* v a2_)
              . L.Let (rev a2) (revVar r .* v a1_)
            )
          )
        "div" -> g
          ( L.Let r (v a1 ./ v a2)
          , [a1, a2]
          , \(a1_ : a2_ : xs') ->
            ( xs'
            , L.Let (rev a1) (revVar r ./ v a2_)
              . L.Let
                  (rev a2)
                  (Prim.pNeg ((v a1_ .* revVar r) ./ (v a2_ .* v a2_)))
            )
          )
        "eq" -> temporaryDummy
        s -> error ("differentiateE unexpected PrimFun: " ++ s)
    k@(L.Konst{}) ->
      g
      -- Not strictly linear because we don't eliminate `rev v`, but we
      -- probably don't care at the moment
        (L.Let r k, [], \xs' -> (xs', id))

    L.Var vv ->
      g (L.Let r (v vv), [], \xs' -> (xs', L.Let (rev vv) (revVar r)))
    call@(L.Call f args)
      | f `Prim.isThePrimFun` "build"
      , [L.Var _n, _lambda] <- args
        -> temporaryDummy
      | f `Prim.isThePrimFun` "sum"
      , [L.Var{}] <- args
        -> temporaryDummy
      | f `Prim.isThePrimFun` "eq"
      , [L.Var{}, L.Var{}] <- args
        -> temporaryDummy
      | f `Prim.isThePrimFun` "log"
      , [L.Var a] <- args
        -> g
        ( L.Let r call
        , [a]
        , \(a_:xs') -> (xs', L.Let (rev a) (revVar r ./ v a_))
        )
      | f `Prim.isThePrimFun` "exp"
      , [L.Var a] <- args
        -> g
        ( L.Let r call
        , [a]
        -- Could share this exp
        , \(a_:xs') -> (xs', L.Let (rev a) (revVar r .* Prim.pExp (v a_)))
        )
      | f `Prim.isThePrimFun` "to_float"
      , [L.Var{}] <- args
        -> temporaryDummy
      | f `Prim.isThePrimFun` "$ranhashdoub"
      , [L.Var{}] <- args
        -> temporaryDummy
      | (L.TFun _ (L.Fun (L.UserFun{}))) <- f
        -> temporaryDummy
      | (L.TFun _ (L.Fun (L.SelFun{}))) <- f
        -> temporaryDummy
    L.Assert _cond _assertBody -> temporaryDummy
    L.If _cond _true _false -> temporaryDummy
    _ -> error ("Couldn't differentiate rhs: " ++ show rhs)
   where
    g (myFwd, myTrace, fromTrace) =
      ( myFwd . theirFwd
      , final
      , myTrace ++ theirTrace
      , \fullTrace ->
        let (theirTrace_, myRev) = fromTrace fullTrace
        in  theirRev theirTrace_ . myRev
      )
      where (theirFwd, final, theirTrace, theirRev) = differentiateE body
    temporaryDummy = g (id,
              [],
              \xs' -> (xs', id))


  L.Var vv -> (id, vv, [], \[] -> id)
  s        -> error ("Couldn't differentiate: " ++ show s)
 where
  (.*) = Prim.pMul
  (./) = Prim.pDiv
  (.+) = Prim.pAdd
  (.-) = Prim.pSub
  v    = L.Var

renameTVar :: L.TVar -> (String -> String) -> L.TVar
renameTVar (L.TVar t (L.Simple s)) f = L.TVar t (L.Simple (f s))
renameTVar _                       _ = error "renameTVar"

removeDupsE :: L.TExpr -> L.TExpr
removeDupsE = \case
  L.Dup (v1, v2) v@(L.Var{}) body -> L.Let v1 v (L.Let v2 v (removeDupsE body))
  L.Let v        e           body -> L.Let v e (removeDupsE body)
  -- Since we're supposed be in ANF we only recurse on Dups and Lets
  notDup                          -> notDup

removeDupsD :: L.TDef -> L.TDef
removeDupsD def@(L.Def { L.def_rhs = L.UserRhs rhs }) =
  def { L.def_rhs = L.UserRhs (removeDupsE rhs) }
removeDupsD (L.Def { L.def_rhs = L.StubRhs }) =
  error "Did not expect to see StubRhs"
removeDupsD edef@(L.Def { L.def_rhs = L.EDefRhs }) = edef

rev :: L.TVar -> L.TVar
rev = flip renameTVar (++ "$r")

revVar :: L.TVar -> L.TExpr
revVar = L.Var . rev
