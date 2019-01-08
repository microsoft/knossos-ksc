module OptLet( optLets
             , Subst, mkEmptySubst, lookupSubst
             , extendSubstInScope, extendSubstMap )
             where

import Lang
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char( isDigit )

optLets :: [TVar] -> TExpr -> TExpr
optLets args rhs
  = optLetsE args (occAnal rhs)

----------------------
-- Dead code elimination - occurrence analysis
----------------------

occAnal :: TExpr -> ExprX TFun (Int,TVar)
occAnal e = fst (occAnalE e)

occAnalE :: TExpr -> (ExprX TFun (Int,TVar), M.Map TVar Int)
occAnalE (Var v)   = (Var (1,v), M.singleton v 1)
occAnalE (Konst k) = (Konst k, M.empty)
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

occAnalE (Lam v e)
  = (Lam v e', v `M.delete` vs)
  where
    (e', vs) = occAnalE e

occAnalE (Call f e) = (Call f e', vs)
                      where
                        (e',vs) = occAnalE e
occAnalE (Tuple es) = (Tuple es', unions vs)
                      where
                        (es', vs) = unzip (map occAnalE es)
occAnalE (Let var rhs body)
  = (Let (n, var) rhs' body', vs)
  where
    n = case var `M.lookup` vsb of
          Just n  -> n
          Nothing -> 0
    (rhs',  vsr) = occAnalE rhs
    (body', vsb) = occAnalE body
    vs | n == 0    = (var `M.delete` vsb)
       | otherwise = (var `M.delete` vsb) `union` vsr

occAnalE (If b t e)
  = (If b' t' e', vsb `M.union` vst `M.union` vse)
  where
    (b', vsb) = occAnalE b
    (t', vst) = occAnalE t
    (e', vse) = occAnalE e

union :: Ord b => M.Map b Int -> M.Map b Int -> M.Map b Int
union = M.unionWith (+)

unions :: Ord b => [M.Map b Int] -> M.Map b Int
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

data Subst
  = S { s_env      :: M.Map Var TExpr   -- Keys are Vars not TVars
      , s_in_scope :: S.Set Var         -- Don't bother to compare the types
    }

mkEmptySubst :: [TVar] -> Subst
mkEmptySubst tvs
  = S { s_env = M.empty
      , s_in_scope = S.fromList (map tVarVar tvs) }

lookupSubst :: Var -> Subst -> Maybe TExpr
lookupSubst v (S { s_env = env }) = v `M.lookup` env

extendSubstMap :: Var -> TExpr -> Subst -> Subst
extendSubstMap v e subst@(S { s_env = env })
  = subst { s_env = M.insert v e env }

extendSubstInScope :: TVar -> Subst -> (TVar, Subst)
extendSubstInScope (TVar ty v)
                   (S { s_in_scope = in_scope, s_env = env })
  = (tv', S { s_env      = env'
            , s_in_scope = v' `S.insert` in_scope })
  where
    v'  = notInScope v in_scope
    tv' = TVar ty v'
    env' | v == v'   = v `M.delete` env
         | otherwise = M.insert v (Var tv') env

notInScope :: Var -> S.Set Var -> Var
-- Find a variant of the input Var that is not in the in-scope set
--
-- Do this by adding "_1", "_2" etc
notInScope v in_scope
  | not (v `S.member` in_scope)
  = v
  | otherwise
  = try (n+1)
  where
    (str, rebuild) = case v of
            Dummy -> error "Can't bind Dummy"
            Simple s   -> (s, Simple)
            Delta  s   -> (s, Delta)
            Grad s dir -> (s, \s' -> Grad s' dir)

    try :: Int -> Var
    try n | var' `S.member` in_scope = try (n+1)
          | otherwise                = var'
          where
            var' = rebuild str'
            str' = prefix ++ '_' : show n

    (prefix, n) = parse_suffix [] (reverse str)

    parse_suffix :: [Char]          -- Digits parsed from RH end (in order)
                 -> String          -- String being parsed (reversed)
                 -> (String, Int)   -- Srring before ":", plus number found after
    -- E.g. parse_suffix "foo:23"  = ("foo",    23)
    --      parse_suffix "womabat" = ("wombat", 0)
    parse_suffix ds (c:cs)
      | c == '_'
      , not (null ds)
      = (reverse cs, read ds)
      | isDigit c
      = parse_suffix (c:ds) cs
    parse_suffix ds cs
      = (reverse cs ++ ds, 0)



optLetsE :: [TVar] -> ExprX TFun (Int,TVar) -> TExpr
-- This function inline let-bindings that are only used once
-- or whose RHS is trivial (see inline_me for eaxctly what.
-- Take care: see Note [Capture-avoiding substitution]
optLetsE args e = go (mkEmptySubst args) e
  where
    go :: Subst -> ExprX TFun (Int,TVar) -> TExpr
    go subst (Let (n, tv@(TVar _ty v)) r b)
      | inline_me n v r' = go (extendSubstMap v r' subst) b
      | otherwise        = Let tv' r' (go subst' b)
      where
        r' = go subst r
        (tv', subst') = extendSubstInScope tv subst

    go subst (Var (_, tv@(TVar _ v)))
      = case lookupSubst v subst of
          Just e  -> e
          Nothing -> Var tv

    go _ubst (Konst k)      = Konst k
    go subst (Call f e)     = Call f (go subst e)
    go subst (If b t e)     = If (go subst b) (go subst t) (go subst e)
    go subst (Tuple es)     = Tuple (map (go subst) es)
    go subst (App e1 e2)    = App (go subst e1) (go subst e2)
    go subst (Assert e1 e2) = Assert (go subst e1) (go subst e2)
    go subst (Lam v e)      = Lam v' (go subst' e)
                            where
                              (v', subst') = extendSubstInScope v subst

inline_me :: Int -> Var -> TExpr -> Bool
inline_me n bndr rhs
  | n==0            = True   -- Dead code
  | n==1            = True   -- Used exactly once
  | isTrivial rhs   = True   -- RHS is just a variable or literal
  | Grad {} <- bndr = True   -- Always inline Grads (might not do this in future)
  | otherwise       = False

isTrivial :: TExpr -> Bool
isTrivial (Tuple [])          = True
isTrivial (Var {})            = True
isTrivial (Konst {})          = True
isTrivial (Call _ (Tuple [])) = True
isTrivial (Assert _ e2)       = isTrivial e2
isTrivial _ = False
