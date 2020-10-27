-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances,
             PatternSynonyms,
	     ScopedTypeVariables #-}

module OptLet( optLets
             , Subst
             , mkEmptySubst, lookupSubst
             , substInScope, extendInScopeSet
             , substBndr, extendSubstMap, zapSubst
             , substExpr, substVar )
             where

import Lang
import LangUtils
import Prim
import Ksc.Traversal (traverseState)
import           Data.List (foldl')
import qualified Data.Map as M

optLets :: Subst -> TExpr -> TExpr
optLets subst rhs
  = optLetsE subst (occAnal rhs)

----------------------
-- Dead code elimination - occurrence analysis
----------------------

occAnal :: TExpr -> ExprX OccAnald
occAnal e = fst (occAnalE e)

-- Maps a free variable to its number of syntactic occurrences
-- (but using max for the branches of an If)
type OccMap = M.Map TVar Int

occAnalE :: TExpr -> (ExprX OccAnald, OccMap)
occAnalE (Var v)    = (Var v, M.singleton v 1)
occAnalE (Konst k)  = (Konst k, M.empty)
occAnalE (Dummy ty) = (Dummy ty, M.empty)

occAnalE (App e1 e2)
  = (App e1' e2', unionOccMap vs1 vs2)
  where
    (e1', vs1) = occAnalE e1
    (e2', vs2) = occAnalE e2

occAnalE (Assert e1 e2)
  = (Assert e1' e2', unionOccMap vs1 vs2)
  where
    (e1', vs1) = occAnalE e1
    (e2', vs2) = occAnalE e2

occAnalE (Lam tv e)
  = (Lam tv e', markMany (tv `M.delete` vs))
    -- If a variable is used under a lambda
    -- we must not inline it uncritically, lest
    -- we duplcate work.   E.g.
    --   let x = <expensive> in
    --   build N (\i.  ...x...)
  where
    e' :: ExprX OccAnald
    (e', vs)   = occAnalE e

occAnalE (Call f e) = (Call f e', vs)
                     where
                       (e',vs) = occAnalE e
occAnalE (Tuple es) = (Tuple es', unionsOccMap vs)
                      where
                        (es', vs) = unzip (map occAnalE es)

occAnalE (Let tv rhs body)
  = (Let n_tv rhs' body', vs)
  where
    n_tv = fmap annotate_tv_with_occ_count tv

    annotate_tv_with_occ_count :: TVar -> (Int, TVar)
    annotate_tv_with_occ_count tv' = (n, tv')
      where n = case tv' `M.lookup` vsb of
              Just n  -> n
              Nothing -> 0

    (rhs', vsr)   = occAnalE rhs
    (body', vsb)  = occAnalE body

    without_any_of :: Ord k => M.Map k a -> [k] -> M.Map k a
    without_any_of = foldl' (flip M.delete)

    vsb_no_tv     = vsb `without_any_of` patVars tv

    binding_dead  = all ((== 0) . fst) n_tv

    vs | binding_dead
                   = vsb_no_tv

       -- Note [Inline tuples], Item (1)
       --
       -- When a tuple is on the RHS of a let we want to prevent its
       -- contents from being inlined back into it because we generally
       -- want to fuse tuple construction with a function call that
       -- dismantles it.  In order to stop the contents being inlined we
       -- pretend that it occurs many times.
       | VarPat (n, _) <- n_tv
       , Tuple _ <- rhs
       , n > 1     = vsb_no_tv
                     `unionOccMap` markMany vsr

       -- See Note [Inline tuples], Item (2)
       | otherwise = vsb_no_tv
                     `unionOccMap` vsr

occAnalE (If b t e)
  = (If b' t' e', vsb `unionOccMap` (M.unionWith max vst vse))
  where
    (b', vsb) = occAnalE b
    (t', vst) = occAnalE t
    (e', vse) = occAnalE e

markMany :: OccMap -> OccMap
-- markMany takes each variable in the OccMap
-- and makes occur many times
markMany m = M.map (const manyOcc) m

manyOcc :: Int
manyOcc = 100   -- Obviously ad-hoc; anything >= 2 should be fine

unionOccMap :: OccMap -> OccMap -> OccMap
-- Add occurrences when a variable appears in both subexpressions
-- (we can't just use M.union as this would only count the
-- first subexpression when a variable appears in both)
unionOccMap = M.unionWith (+)

unionsOccMap :: [OccMap] -> OccMap
unionsOccMap = foldr unionOccMap M.empty

-------------------------
-- Substitute trivials
-------------------------

{- Note [Capture-avoiding substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
 f(x) = let y = x+1 in
        let x = x*2 in
        y * x * x

We do not want to substitute for the 'y' giving
 f(x) = let y = x+1 in
        let x = x*2 in
        (x+1) * x * x

because those two x's are quite different! In this bogus result,
the 'x' in the (x+1) has been captured by the inner binding for 'x'.

We must instead rename the inner 'x' so we get
 f(x) = let y   = x+1 in
        let x_1 = x*2 in
        (x+1) * x_1 * x_1
-}

data Subst
  = S { s_env      :: M.Map Var TExpr   -- Keys are Vars not TVars
      , s_in_scope :: InScopeSet        -- Don't bother to compare the types
    }

substInScope :: Subst -> InScopeSet
substInScope = s_in_scope

mkEmptySubst :: [TVar] -> Subst
mkEmptySubst tvs
  = S { s_env = M.empty
      , s_in_scope = mkInScopeSet tvs }

lookupSubst :: Var -> Subst -> Maybe TExpr
lookupSubst v (S { s_env = env }) = v `M.lookup` env

extendSubstMap :: Var -> TExpr -> Subst -> Subst
extendSubstMap v e subst@(S { s_env = env })
  = subst { s_env = M.insert v e env }

zapSubst :: Subst -> Subst
-- Zap the substitution, but preserve the in-scope set
zapSubst (S { s_in_scope = in_scope })
  = S { s_env = M.empty, s_in_scope = in_scope }

-- * It clones the binder if it is already in scope
-- * Extends the substitution and the in-scope set as appropriate
substBndr :: TVar -> Subst -> (TVar, Subst)
substBndr tv (S { s_in_scope = in_scope, s_env = env })
  = (tv', S { s_env      = env'
            , s_in_scope = is' })
  where
    (is', tv') = notInScopeTV in_scope tv
    env' = M.insert (tVarVar tv) (Var tv') env

substVar :: Subst -> TVar -> TExpr
substVar subst tv = case lookupSubst (tVarVar tv) subst of
                      Just e  -> e
                      Nothing -> Var tv

substExpr :: Subst -> TExpr -> TExpr
substExpr subst e
  = go e
  where
    go (Var tv)       = substVar subst tv
    go (Dummy ty)     = Dummy ty
    go (Konst k)      = Konst k
    go (Call f es)    = Call f (go es)
    go (If b t e)     = If (go b) (go t) (go e)
    go (Tuple es)     = Tuple (map go es)
    go (App e1 e2)    = App (go e1) (go e2)
    go (Assert e1 e2) = Assert (go e1) (go e2)
    go (Let v r b)    = Let v' (go r) (substExpr subst' b)
                      where
                        (v', subst') = traverseState substBndr v subst
    go (Lam v e)      = Lam v' (substExpr subst' e)
                      where
                        (v', subst') = substBndr v subst

optLetsE :: Subst -> ExprX OccAnald -> TExpr
-- This function inlines let-bindings that are only used once
-- or whose RHS is trivial (see inline_me for exactly what.
-- Take care: see Note [Capture-avoiding substitution]
optLetsE = go
  where
    go :: Subst -> ExprX OccAnald -> TExpr

    go subst (Let pat r b)
      = go_let (go subst r)
      where
        (tv'', subst') = traverseState (\(_, tVar) -> substBndr tVar) pat subst

        go_let (Let b1 r1 r2)  = Let b1 r1 (go_let r2)
        go_let r'
          -- Note that as well as performing inlining, this clause also
          -- implicitly drops dead code, because bindings that are
          -- used zero times will be "inlined" into their zero uses,
          -- removing them.
          | VarPat (n, TVar _ v) <- pat
          , inline_me n r'     = go (extendSubstMap v r' subst) b
          | TupPat ns_vs <- pat
          , let binding_dead   = all ((== 0) . fst) ns_vs
          , binding_dead
          -- If none of the variables that are bound in the tuple
          -- pattern occur in the body then we simply ignore the
          -- binding entirely.
          = go subst b
          | otherwise          = Let tv'' r' (go subst' b)

    go subst (Var tv)       = substVar subst tv
    go _ubst (Dummy ty)     = Dummy ty
    go _ubst (Konst k)      = Konst k
    go subst (Call f es)    = Call f (go subst es)
    go subst (If b t e)     = If (go subst b) (go subst t) (go subst e)
    go subst (Tuple es)     = Tuple (map (go subst) es)
    go subst (App e1 e2)    = App (go subst e1) (go subst e2)
    go subst (Assert e1 e2) = Assert (go subst e1) (go subst e2)
    go subst (Lam tv e)     = Lam tv' (go subst' e)
                                 where
                                   (tv', subst') = substBndr tv subst

{- Note [Inline tuples]
~~~~~~~~~~~~~~~~~~~~~~~
Consider

  let x = ex in
  let y = ey in
  let p = (x,y) in
  ... p ... p ...

There are two valid ways we could inline these let bindings, firstly

  let p = (ex,ey) in
  ... p ... p ...

and secondly

  let x = ex in
  let y = ey in
  ... (x,y) ... (x,y) ...

We want to choose the second way because we hope that the tuple
constructor will fuse with a tuple selector in the body (perhaps from
a function inlined either by a sufficiently smart compiler pass, or,
as is the case at the time of writing, an explicit $inline call).

We achieve this by marking x and y as occurring many times so that
they are not inlined back into the tuple. [Item (1)]

This has the beneficial consequence that redundant work can be
eliminated, for example if we wrote

 let t = (ex, ey, ez)
 in ...get$1$3(t)...get$2$3(t)....get$1$3(t)...

then ANF followed by this transformation avoids us ever evaluating ez.
Furthermore we can obtain cross-function slicing by inlining an entire
function.  For example

 f(x) = (expensive1(x), expensive2(x))
 g(x) = get$1$2(f(x))

can be rewritten to

 g(x) = expensive1(x)

(Again, at the time of writing, the call to f must be marked with
$inline.)

On the other hand if p only occurs once then we want to transform

  let x = ex in
  let y = ey in
  let p = (x,y) in
  ... p ...

to

  ... (ex,ey) ...

Therefore we only mark x and y as occurring many times if p is used
more than once.  Then if each of p, x and y occur only once they will
all be inlined.  Specifically, we do the 'markMany' call exactly for
those variables occuring in a literal tuple which itself is bound to a
variable (not a tuple pattern) used more than once.  [Item (2)]

Some of this is discussed at https://github.com/awf/knossos/pull/426
and https://github.com/microsoft/knossos-ksc/issues/327

-}

inline_me :: Int -> TExpr -> Bool
inline_me n rhs
  | n==0            = True   -- Dead code
  | n==1            = True   -- Used exactly once
  | otherwise       = inline_me_help rhs

inline_me_help :: TExpr -> Bool
inline_me_help rhs
  | isTrivial rhs   = True   -- RHS is trivial, see isTrivial for what that means
  | isKZero rhs     = True   -- Inline zeros, as they will very likely disappear
  | TypeLM {} <- typeof rhs = True   -- Always inline linear maprs (might not do this in future)
  | Tuple ts <- rhs              -- Always inline tuples whose fields are all trivial
  , all inline_me_help ts = True  -- See Note [Inline tuples]
  | otherwise             = False
