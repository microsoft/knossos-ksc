-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
module CSE where

import Ksc.Traversal( traverseState )
import Lang
import Prim
import OptLet( Subst, substBndr, lookupSubst, mkEmptySubst, extendSubstMap )
import LangUtils( GblSymTab )
import Rules
import ANF
import Opt
import KMonad
import qualified Data.Map as M

{- Note [CSE for bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Let-bindings have three cases

* SUBSTITUTE: applies when the LHS is a variable and the RHS is a variable

     let x = y in ...(h x)....

  Here we want to extend the /substitution/ with x -> y, so that the
  (h x) in the body might CSE with an enclosing (let v = h y in ...).
  We also drop the binding for x.
  NB: the substitution maps Vars, so we extend the substitution with
      a binding for the original Var 'x'

  How can we have a variable on the RHS? Doesn't the simplifier inline them?
  No: the original RHS might have been (g z) which has CSE'd
  with an enclosing (let y = g z in ...).  This is super-important.
         x1 = C a b
         x2 = C x1 b
         y1 = C a b
         y2 = C y1 b
   Here we CSE y1's rhs to 'x1', and then we must add (y1->x1) to
   the substitution so that we can CSE the binding for y2.

* EXTEND THE REVERSE MAPPING:

     let p = e in ...e...

  where 'e' is not a variable.

  Here we want to extend the /reverse mapping (cs_map)/ so that we
  replace occurrences of the expression 'e' with 'p'.  More precisely
  since 'p' is a pattern, we create an expression 'pe' from 'p', and
  replace 'e' with 'pe'.

* PATTERN BINDINGS: That leaves one case un-handled

     let (x1,x2) = y in ...

  where the LHS is a tuple pattern and the RHS is a variable.
  We can't use SUBSTITUTE because the LHS is a tuple.  We don't
  want to use EXTEND because tha would replace occurences of 'y'
  with (x1,x2) which is not what we want at all.

  So in this final case we just proceed to CSE the body.

Notice that
  - The SUBSTITUTE situation extends the substitution (cs_subst)
  - The EXTEND situation extends the reverse mapping (cs_map)

Notice also that in the SUBSTITUTE case we leave behind a binding
  x = y
even though we /also/ carry a substitution x -> y.  Can we just drop
the binding instead?  Well, not at top level! See SimplUtils
Note [Top level and postInlineUnconditionally]; and in any case CSE
applies only to the /bindings/ of the program, and we leave it to the
simplifier to propate effects to the RULES.  Finally, it doesn't seem
worth the effort to discard the nested bindings because the simplifier
will do it next.


Note [CSE of assert]
~~~~~~~~~~~~~~~~~~~~

When we encounter (assert (eq e1 e2) body) we would like to use the
information that e1 is equal to e2 to simplify body.  Therefore if e1
and e2 are both variables (say v1 and v2) we tweak the reverse map so
to change entries that used to say v1 to say v2 instead.

-}

data CSEnv
  = CS { cs_subst :: Subst  -- Maps InVars to OutExprs
            -- The substitution maps variables to
            -- /trivial/ OutExprs, not arbitrary expressions

       , cs_map   :: M.Map TExpr Pat   -- The reverse mapping
       }

cseDefs :: RuleBase -> GblSymTab -> [TDef]
        -> KM (GblSymTab, [TDef])
-- The returned GblSymTab contains the CSE'd definitions
cseDefs rb gst defs
  = do { anf_defs <- anfDefs defs
--       ; banner "ANF'd"
--       ; displayN anf_defs

       ; let cse_defs = map cseD anf_defs
--       ; banner "CSE'd"
--       ; displayN cse_defs

             -- cseE turns   let x = e in ..let y = e in ...
             --      into    let x = e in ..let y = x in ...
            -- Then optDefs substitutes x for y

       ; optDefs rb gst cse_defs
      }

cseD :: TDef -> TDef
cseD def@(Def { def_arg = bndr, def_rhs = UserRhs rhs })
  = def { def_rhs = UserRhs $ cseE init_env rhs }
  where
    init_env = CS { cs_subst = mkEmptySubst [bndr]
                  , cs_map   = M.empty }

cseD def = def  -- EDefRhs, StubRhs

---------------------------------
cseE :: CSEnv -> TExpr -> TExpr

cseE cse_env@(CS { cs_subst = subst, cs_map = rev_map })
     (Let tv rhs body)
  = case (tv, cseE_check cse_env rhs) of
    (VarPat tv, var_rhs@(Var _)) ->
      -- See Note [CSE for bindings] (SUBSTITUTE)
      let v        = tVarVar tv
          subst'   = extendSubstMap v var_rhs subst
          body_env = cse_env { cs_subst = subst' }
      in cseE_check body_env body

    (pat@(TupPat _), rhs'@(Var _)) ->
      -- See Note [CSE for bindings) (PATTERN BINDINGS)
      Let pat rhs' (cseE_check cse_env body)

    (_, rhs') ->
      -- See Note [CSE for bindings] (EXTEND THE REVERSE MAPPING)
      let (tv', subst') = traverseState substBndr tv subst
          rev_map'      = M.insert rhs' tv' rev_map
          body_env      = CS { cs_subst = subst', cs_map = rev_map' }
      in Let tv' rhs' (cseE_check body_env body)

-- Special case for (assert (e1 == e2) body)
-- where we want to CSE e2 into e1
-- See Note [CSE of assert]
cseE cse_env@(CS { cs_map = rev_map }) (Assert cond body)
 | Call eq (Tuple [Var v1, Var v2]) <- cond'
 , eq `isThePrimFun` "eq"
 , let replace_v1_with_v2 (VarPat v1') | v1' == v1 = VarPat v2
       replace_v1_with_v2 original = original
 , let cse_env' = cse_env { cs_map = M.map replace_v1_with_v2 rev_map }
 = Assert cond' (cseE cse_env' body)

 | otherwise
 = Assert cond' (cseE cse_env body)
 where
   cond' = cseE cse_env cond

cseE cse_env (If e1 e2 e3)
  = If (cseE_check cse_env e1)
       (cseE_check cse_env e2)
       (cseE_check cse_env e3)

cseE cse_env (Call f es) = Call f (cseE_check cse_env es)
cseE cse_env (Tuple es)  = Tuple (map (cseE_check cse_env) es)
cseE cse_env (App e1 e2) = App (cseE_check cse_env e1)
                               (cseE_check cse_env e2)

cseE cse_env@(CS { cs_subst = subst }) (Lam v e)
  = Lam v' (cseE body_env e)
  where
    (v', subst') = substBndr v subst
    body_env     = cse_env { cs_subst = subst' }

cseE cs_env (Var tv)
  = case lookupSubst (tVarVar tv) (cs_subst cs_env) of
      Just e  -> e
      Nothing -> Var tv

cseE _ e@(Konst {}) = e
cseE _ e@(Dummy {}) = e

cseE_check :: CSEnv -> TExpr -> TExpr
-- Look up the entire expression in the envt
cseE_check cse_env e
  = case M.lookup e' (cs_map cse_env) of
      Just e'' -> patToExpr e''
      Nothing  -> e'
  where
    e' = cseE cse_env e
