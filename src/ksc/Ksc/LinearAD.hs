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

  let diff_defs = map differentiateD linear_defs
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
lineariseD _ = error "I can't cope with that rhs"

lineariseE :: L.TExpr -> L.TExpr
lineariseE = \case
  L.Let v call@(L.Call f args) body -> dups'
    (L.Let v (L.Call f new_args) (lineariseE body))
   where
    dups = flip map args $ \case
      L.Var argv -> if argv `LU.notFreeIn` body
        then (id, L.Var argv)
        else
          let argv' = LU.newVarNotIn (L.typeof call) body
          in  (L.Dup (argv, argv') (L.Var argv), L.Var argv')
      k@(L.Konst{}) -> (id, k)
      arg           -> error ("Unexpected in Anf form " ++ L.render (L.ppr arg))
    dups' :: L.TExpr -> L.TExpr
    new_args :: [L.TExpr]
    (dups', new_args) =
      foldr (\(g, arg) (gs, args) -> (g . gs, arg : args)) (id, []) dups

  L.Let v k@(L.Konst{}) body -> L.Let v k (lineariseE body)
  var@( L.Var{} )            -> var
  call@(L.Call{})            -> call
  v                          -> error ("lineariseE unexpected " ++ show v)

differentiateD :: L.TDef -> L.TDef
differentiateD tdef@(L.Def { L.def_fun = L.Fun (L.UserFun f), L.def_rhs = L.UserRhs rhs })
  = L.Def
    { L.def_fun    = L.LinearGradFun (L.UserFun f)
    , L.def_args   = args ++ [rev r]
    , L.def_res_ty = res_ty
    , L.def_rhs    = L.UserRhs d_rhs
    }
 where
  (fwd, r, trace, rev') = differentiateE rhs
  d_rhs = (fwd . rev' trace . L.Tuple) (L.Var r : map (L.Var . rev) args)
  args                  = L.def_args tdef
  res_ty                = L.TypeTuple (L.def_res_ty tdef : map L.typeof args)
differentiateD _ = error "differentiateD"

differentiateE
  :: L.TExpr
  -> (L.TExpr -> L.TExpr, L.TVar, [L.TVar], [L.TVar] -> L.TExpr -> L.TExpr)
differentiateE = \case
  L.Dup (v1, v2) (L.Var v) body ->
    ( L.Dup (v1, v2) (L.Var v) . body'
    , r
    , xs
    , \xs' -> f xs' . L.Let (rev v) (revVar v1 .+ revVar v2)
    )
    where (body', r, xs, f) = differentiateE body
  L.Let r rhs body -> case rhs of
    (L.Call (L.TFun _ (L.Fun (L.PrimFun op))) [L.Var a1, L.Var a2]) ->
      case op of
        "add" -> g
          ( L.Let r (v a1 .+ v a2)
          , []
          , \xs' -> (xs', L.Dup (rev a1, rev a2) (revVar r))
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
        s -> error ("differentiateE unexpected " ++ s)
    k@(L.Konst{}) ->
      g
      -- Not strictly linear because we don't eliminate `rev v`, but we
      -- probably don't care at the moment
        (L.Let r k, [], \xs' -> (xs', id))

    rhs -> error ("Couldn't differentiate rhs: " ++ show rhs)
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

  L.Var v -> (id, v, [], \[] -> id)
  s       -> error ("Couldn't differentiate: " ++ show s)
 where
  (.*) :: L.TExpr -> L.TExpr -> L.TExpr
  (.*) = Prim.pMul
  (./) = Prim.pDiv
  (.+) = Prim.pAdd
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
removeDupsD _ = error "removeDupsF"

rev :: L.TVar -> L.TVar
rev = flip renameTVar (++ "$r")

revVar :: L.TVar -> L.TExpr
revVar = L.Var . rev
