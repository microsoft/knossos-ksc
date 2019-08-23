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
lineariseD tdef@(L.Def { L.def_rhs = L.UserRhs rhs })
  = L.Def
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
differentiateD tdef@(L.Def { L.def_fun = L.Fun (L.UserFun f)
                           , L.def_rhs = L.UserRhs rhs }) = L.Def
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
    , \xs' -> f xs' . L.Let v (Prim.pAdd (L.Var v1) (L.Var v2))
    )
    where (body', r, xs, f) = differentiateE body
  L.Let v (L.Call (L.TFun t (L.Fun (L.PrimFun op))) [L.Var a1, L.Var a2]) body
    -> case op of
      "add" ->
        ( L.Let
            v
            (L.Call (L.TFun t (L.Fun (L.PrimFun "add"))) [L.Var a1, L.Var a2])
          . body'
        , r
        , xs
        , \xs' -> f xs' . L.Dup (rev a1, rev a2) (L.Var (rev v))
        )
      "mul" ->
        ( L.Dup (a1', a2') (L.Var a1)
          . L.Dup (renameTVar a2 (++ "$1"), a2) (L.Var a2)
          . L.Let
              v
              (L.Call (L.TFun t (L.Fun (L.PrimFun "mul"))) [L.Var a1, L.Var a2])
          . body'
        , r
        , a1 : a2 : xs
        , \(a1' : a2' : xs') ->
          f xs'
            . L.Dup (rev v1, rev v2) (L.Var (rev v))
            . L.Let (rev a1) (Prim.pMul (L.Var (rev v1)) (L.Var a2'))
            . L.Let (rev a2) (Prim.pMul (L.Var (rev v2)) (L.Var a1'))
        )
      "div"
        -> ( L.Dup (a1', a2') (L.Var a1)
             . L.Dup (renameTVar a2 (++ "$1"), a2) (L.Var a2)
             . L.Let
                 v
                 (L.Call (L.TFun t (L.Fun (L.PrimFun "div")))
                         [L.Var a1, L.Var a2]
                 )
             . body'
           , r
           , a1 : a2 : xs
           , \(a1' : a2' : xs') ->
             f xs'
               . L.Dup (rev v1, rev v2) (L.Var (rev v))
               . L.Let (rev a1) (Prim.pDiv (L.Var (rev v1)) (L.Var a2'))
               . L.Let
                   (rev a2)
                   (Prim.pNeg
                     (Prim.pDiv (L.Var (rev v2))
                                (Prim.pMul (L.Var a1') (L.Var a1'))
                     )
                   )
           )
      s -> error ("differentiateE unexpected " ++ s)
   where
    (body', r, xs, f) = differentiateE body
    -- These renamings are really quite naughty
    a1'               = renameTVar a1 (++ "$1")
    a2'               = renameTVar a2 (++ "$2")
    v1                = renameTVar v (++ "$1")
    v2                = renameTVar v (++ "$2")

  L.Let v k@(L.Konst{}) body ->
    -- Not strictly linear because we don't eliminate `rev v`, but we
    -- probably don't care at the moment
                                (L.Let v k . body', r, xs, \xs' -> f xs')
    where (body', r, xs, f) = differentiateE body

  L.Var v -> (id, v, [], \[] -> id)

  s       -> error ("Couldn't differentiate: " ++ show s)

renameTVar :: L.TVar -> (String -> String) -> L.TVar
renameTVar (L.TVar t (L.Simple s)) f = L.TVar t (L.Simple (f s))
renameTVar _                       _ = error "renameTVar"

rev :: L.TVar -> L.TVar
rev = flip renameTVar (++ "$r")
