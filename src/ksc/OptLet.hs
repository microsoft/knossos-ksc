-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances, LambdaCase,
             PatternSynonyms, StandaloneDeriving, AllowAmbiguousTypes,
	     ScopedTypeVariables, TypeApplications #-}

module OptLet( optLets
             , Subst, InScopeSet, emptyInScopeSet
             , mkEmptySubst, lookupSubst
             , substInScope, extendInScopeSet
             , substBndr, extendSubstMap, zapSubst
             , substExpr, substVar, notInScope )
             where

import Lang
import LangUtils( isTrivial )
import Prim
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char( isDigit )

optLets :: [TVar] -> TExpr -> TExpr
optLets args rhs
  = optLetsE args (occAnal rhs)

----------------------
-- Dead code elimination - occurrence analysis
----------------------

occAnal :: TExpr -> ExprX OccAnald
occAnal e = fst (occAnalE e)

type OccMap = M.Map TVar Int  -- How often each free variable occurs

occAnalTv :: TVar -> (TVar, OccMap)
occAnalTv (TVar ty v) = (TVar ty' v, vs)
  where
    (ty', vs) = occAnalT ty

occAnalT :: Type -> (Type, OccMap)
occAnalT (TypeVec ty)
  = (TypeVec ty', vs)
  where
    (ty', vs) = occAnalT ty

occAnalT (TypeTuple tys)
  = (TypeTuple tys', unions vs_s)
  where
    (tys', vs_s) = unzip (map occAnalT tys)

occAnalT (TypeLM ty1 ty2)
  = (TypeLM ty1' ty2', M.union vs1 vs2)
  where
    (ty1', vs1) = occAnalT ty1
    (ty2', vs2) = occAnalT ty2

occAnalT (TypeLam ty1 ty2)
  = (TypeLam ty1' ty2', M.union vs1 vs2)
  where
    (ty1', vs1) = occAnalT ty1
    (ty2', vs2) = occAnalT ty2

occAnalT TypeBool    = (TypeBool,    M.empty)
occAnalT TypeInteger = (TypeInteger, M.empty)
occAnalT TypeFloat   = (TypeFloat,   M.empty)
occAnalT TypeString  = (TypeString,  M.empty)
occAnalT TypeUnknown = (TypeUnknown, M.empty)

occAnalE :: TExpr -> (ExprX OccAnald, OccMap)
occAnalE (Var v)    = (Var v, M.singleton v 1)
occAnalE (Konst k)  = (Konst k, M.empty)
occAnalE (Dummy ty) = (Dummy ty, M.empty)

occAnalE (App e1 e2)
  = (App e1' e2', M.union vs1 vs2)
  where
    (e1', vs1) = occAnalE e1
    (e2', vs2) = occAnalE e2

occAnalE (Assert e1 e2)
  = (Assert e1' e2', M.union vs1 vs2)
  where
    (e1', vs1) = occAnalE e1
    (e2', vs2) = occAnalE e2

occAnalE (Lam tv e)
  = (Lam tv' e', vs2 `M.union` markMany (tv `M.delete` vs))
    -- If a variable is used under a lambda
    -- we must not inline it uncritically, lest
    -- we duplcate work.   E.g.
    --   let x = <expensive> in
    --   build N (\i.  ...x...)
  where
    e' :: ExprX OccAnald
    (e', vs)   = occAnalE e
    (tv', vs2) = occAnalTv tv

occAnalE (Call f e) = (Call f e', unions vs)
                     where
                       (e',vs) = unzip (map occAnalE e)
occAnalE (Tuple es) = (Tuple es', unions vs)
                      where
                        (es', vs) = unzip (map occAnalE es)

occAnalE (Let tv (Tuple es) body)
  = (Let (n, tv') (Tuple es') body', vs)
  -- When a tuple is on the RHS of a let we want to prevent its
  -- contents from being inlined back into it because we generally
  -- want to fuse tuple construction with a function call that
  -- dismantles it.  In order to stop the contents being inlined we
  -- pretend that it occurs many times.
  --
  -- See Note [Inline tuples]
  where
    n = case tv `M.lookup` vsb of
          Just n  -> n
          Nothing -> 0
    (tv',   vstv) = occAnalTv tv
    (es',   vsr)  = unzip (map occAnalE es)
    (body', vsb)  = occAnalE body
    vs | n == 0    = tv `M.delete` vsb
       | otherwise = (tv `M.delete` vsb)
                     `union` vstv
                     `union` markMany (unions vsr)

occAnalE (Let tv rhs body)
  = (Let (n, tv') rhs' body', vs)
  where
    n = case tv `M.lookup` vsb of
          Just n  -> n
          Nothing -> 0
    (tv',   vstv) = occAnalTv tv
    (rhs',  vsr)  = occAnalE rhs
    (body', vsb)  = occAnalE body
    vs | n == 0    = tv `M.delete` vsb
       | otherwise = (tv `M.delete` vsb)
                     `union` vstv `union` vsr

occAnalE (If b t e)
  = (If b' t' e', vsb `M.union` vst `M.union` vse)
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

union :: OccMap -> OccMap -> OccMap
union = M.unionWith (+)

unions :: [OccMap] -> OccMap
unions = foldr union M.empty

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

type InScopeSet = S.Set Var

emptyInScopeSet :: InScopeSet
emptyInScopeSet = S.empty

data Subst
  = S { s_env      :: M.Map Var TExpr   -- Keys are Vars not TVars
      , s_in_scope :: InScopeSet        -- Don't bother to compare the types
    }

substInScope :: Subst -> InScopeSet
substInScope = s_in_scope

extendInScopeSet :: TVar -> InScopeSet -> InScopeSet
extendInScopeSet tv in_scope
  = tVarVar tv `S.insert` in_scope

mkEmptySubst :: [TVar] -> Subst
mkEmptySubst tvs
  = S { s_env = M.empty
      , s_in_scope = S.fromList (map tVarVar tvs) }

lookupSubst :: Var -> Subst -> Maybe TExpr
lookupSubst v (S { s_env = env }) = v `M.lookup` env

extendSubstMap :: Var -> TExpr -> Subst -> Subst
extendSubstMap v e subst@(S { s_env = env })
  = subst { s_env = M.insert v e env }

zapSubst :: Subst -> Subst
-- Zap the substitution, but preserve the in-scope set
zapSubst (S { s_in_scope = in_scope })
  = S { s_env = M.empty, s_in_scope = in_scope }

-- * It applies the substitution to the type of the binder
-- * It clones the binder if it is already in scope
-- * Extends the substitution and the in-scope set as appropriate
substBndr :: TVar -> Subst -> (TVar, Subst)
substBndr (TVar ty v) (S { s_in_scope = in_scope, s_env = env })
  = (tv', S { s_env      = env'
            , s_in_scope = v' `S.insert` in_scope })
  where
    v'   = notInScope v in_scope
    tv'  = TVar ty v'
    env' = M.insert v (Var tv') env

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
    go (Call f es)    = Call f (map go es)
    go (If b t e)     = If (go b) (go t) (go e)
    go (Tuple es)     = Tuple (map go es)
    go (App e1 e2)    = App (go e1) (go e2)
    go (Assert e1 e2) = Assert (go e1) (go e2)
    go (Let v r b)    = Let v' (go r) (substExpr subst' b)
                      where
                        (v', subst') = substBndr v subst
    go (Lam v e)      = Lam v' (substExpr subst' e)
                      where
                        (v', subst') = substBndr v subst

notInScope :: Var -> InScopeSet -> Var
-- Find a variant of the input Var that is not in the in-scope set
--
-- Do this by adding "_1", "_2" etc
notInScope v in_scope
  | not (v `S.member` in_scope)
  = v
  | otherwise
  = try (S.size in_scope)
  where
    (str, rebuild) = case v of
            Simple s -> (s, Simple)
            Delta  s -> (s, Delta)
            Grad s m -> (s, \s' -> Grad s' m)

    try :: Int -> Var
    try n | var' `S.member` in_scope = try (n+1)
          | otherwise                = var'
          where
            var' = rebuild str'
            str' = prefix ++ '_' : show n

    (prefix, _n) = parse_suffix [] (reverse str)

    parse_suffix :: [Char]          -- Digits parsed from RH end (in order)
                 -> String          -- String being parsed (reversed)
                 -> (String, Int)   -- Srring before ":", plus number found after
    -- E.g. parse_suffix "foo_23"  = ("foo",    23)
    --      parse_suffix "womabat" = ("wombat", 0)
    parse_suffix ds (c:cs)
      | c == '_'
      , not (null ds)
      = (reverse cs, read ds)
      | isDigit c
      = parse_suffix (c:ds) cs
    parse_suffix ds cs
      = (reverse cs ++ ds, 0)

optLetsE :: [TVar] -> ExprX OccAnald -> TExpr
-- This function inline let-bindings that are only used once
-- or whose RHS is trivial (see inline_me for exactly what.
-- Take care: see Note [Capture-avoiding substitution]
optLetsE params rhs = go (mkEmptySubst params) rhs
  where
    go :: Subst -> ExprX OccAnald -> TExpr

    go subst (Let (n, (TVar ty v)) r b)
      = go_let (go subst r)
      where
        ty' = go_ty subst ty
        tv' = TVar ty' v
        (tv'', subst') = substBndr tv' subst

        go_let (Let b1 r1 r2)  = Let b1 r1 (go_let r2)
        go_let r'
          | inline_me n ty' r' = go (extendSubstMap v r' subst) b
          | otherwise          = Let tv'' r' (go subst' b)

    go subst (Var tv)       = substVar subst tv
    go _ubst (Dummy ty)     = Dummy ty
    go _ubst (Konst k)      = Konst k
    go subst (Call f es)    = Call f (map (go subst) es)
    go subst (If b t e)     = If (go subst b) (go subst t) (go subst e)
    go subst (Tuple es)     = Tuple (map (go subst) es)
    go subst (App e1 e2)    = App (go subst e1) (go subst e2)
    go subst (Assert e1 e2) = Assert (go subst e1) (go subst e2)
    go subst (Lam (TVar ty v) e) = Lam tv'' (go subst' e)
                                 where
                                   (tv'', subst') = substBndr tv' subst
                                   ty' = go_ty subst ty
                                   tv' = TVar ty' v

    go_ty :: Subst -> Type -> Type
    go_ty subst (TypeTuple tys)   = TypeTuple (map (go_ty subst) tys)
    go_ty subst (TypeVec ty)      = TypeVec (go_ty subst ty)
    go_ty subst (TypeLM  ty1 ty2) = TypeLM (go_ty subst ty1) (go_ty subst ty2)
    go_ty subst (TypeLam ty1 ty2) = TypeLam (go_ty subst ty1) (go_ty subst ty2)
    go_ty _ TypeBool    = TypeBool
    go_ty _ TypeInteger = TypeInteger
    go_ty _ TypeFloat   = TypeFloat
    go_ty _ TypeString  = TypeString
    go_ty _ TypeUnknown = TypeUnknown

{- Note [Inline tuples]
~~~~~~~~~~~~~~~~~~~~~~~
Consider
 let t = (expensive1(x), expensive2(x))
 in ...get$1$2(t)...get$2$2(t)....get$1$2(t)...

We want to optimise away the calls to the gets but we don't want to
duplicate the expensive calls.  Our strategy is as follows:

 1. The ANF pass rewrites f to

  f(x) = let t1 = expensive1(x)
             t2 = expensive2(x)
             t  = (t1, t2)
         in ...get$1$2(t)...get$2$2(t)....get$1$2(t)...

 2. We prevent t1 and t2 from being reinlined into the tuple by
 marking them as "occurring many times" in occAnalE.

 3. t is inlined into the body, either by a sufficiently smart
 compiler pass, or, as is the case at the time of writing, an explicit
 $inline call.

 4. The calls to get can be eliminated.

This has the beneficial consequence that redundant work can be
eliminated, for example if we wrote

 let t = (expensive1(x), expensive2(x), expensive3(x))
 in ...get$1$3(t)...get$2$3(t)....get$1$3(t)...

then this transformation avoids ever calculating expensive3(x).
Furthermore we can obtain cross-function slicing by inlining an entire
function.  For example

 f(x) = (expensive1(x), expensive2(x))
 g(x) = get$1$2(f(x))

can be rewritten to

 g(x) = expensive1(x)

(Again, at the time of writing, the call to f must be marked with
$inline.)

Some of this is discussed at

 https://github.com/awf/knossos/pull/426

-}

inline_me :: Int -> Type -> TExpr -> Bool
inline_me n _ty rhs
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

