{-# LANGUAGE LambdaCase #-}

module Ksc.LinearAD where

import qualified Annotate                      as A
import           Parse                          ( parseF )
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
      arg -> error ("Unexpected in Anf form " ++ L.render (L.ppr arg))
    dups' :: L.TExpr -> L.TExpr
    new_args :: [L.TExpr]
    (dups', new_args) =
      foldr (\(g, arg) (gs, args) -> (g . gs, arg : args)) (id, []) dups

  var@(L.Var{}) -> var
  call@(L.Call{}) -> call
  v -> error ("unexpected " ++ L.render (L.ppr v))
