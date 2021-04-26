{-# OPTIONS_GHC -Wname-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Ksc.SUF where

import           Lang hiding ((<>))
import           LangUtils (notInScopeTV)
import           Prim

import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as Merge
import           Data.Traversable (mapAccumL)

sufDef :: TDef -> TDef
sufDef def@Def{ def_pat = vs, def_rhs = UserRhs rhs } =
  check `seq` def { def_pat = vs', def_rhs = UserRhs (perhaps_elim_vs rhs') }
  where ((mrhs', _, perhaps_elim_vs), vs') = rebindMany mrhs avoid1 vs

        check = if not (M.null mrhs')
                then error "Bug in SUF: Expected mrhs to have nothing else in it"
                else ()

        (avoid1, mrhs, rhs') = sufE mempty rhs

sufDef def@Def{ def_rhs = EDefRhs } = def
sufDef def@Def{ def_rhs = StubRhs } = def

-- | @sufE avoid expr@
--
-- Convert expr into Single Use Form by renaming variables and adding
-- Dup and Elim as necessary.
--
-- It contains no variables, free or bound, which appear in avoid
--
-- It returns (avoid', fvMap, sufExpr) where
--
-- * sufExpr is the resulting expression in SUF
--
-- * avoid' is avoid, unioned with all the free or bound variable
--   names that appear in expr
--
-- * fvMap is a Map from the free variables of expr to free variables
--   that play the same role in sufExpr.
sufE :: S.Set Var
     -> TExpr
     -> (S.Set Var, M.Map Var TVar, TExpr)
sufE avoid = \case
  -- SUF{k} -> k
  Konst k -> suf_many_and_dup L0 avoid (\L0 -> Konst k)

  -- SUF{v} -> v'
  Var v   -> (avoid', M.singleton (tVarVar v) v', Var v')
    where (avoid', v') = notInScopeTV avoid v

  -- SUF{let pat = rhs in body} -> [dups] let pat' = SUF{rhs} in [elim patvars?] SUF{body}
  Let pat rhs body -> (avoid4, mlet, dups (Let pat' suf_rhs (perhaps_elim_pat'_vars suf_body)))
    where (avoid2, L2 (mr, suf_rhs) (mb, suf_body)) = sufEMany avoid (L2 rhs body)

          ((mb_no_v, avoid3, perhaps_elim_pat'_vars), pat') = rebindMany mb avoid2 pat

          (avoid4, mlet, dups) = addDups avoid3 [mb_no_v, mr]

  -- SUF{if cond then t else f} -> [dups] if SUF{cond} then [elims and renames] SUF{t} else [elims and renames] SUF{f}
  If cond et ef -> (avoid4, mif, dups (If suf_cond (elim_rename_t suf_et) (elim_rename_f suf_ef)))
    where (avoid2, L3 (condm, suf_cond) (tm, suf_et) (fm, suf_ef))
            = sufEMany avoid (L3 cond et ef)

          ((avoid3, elim_rename_t, elim_rename_f), tfm)
            = mapAccumL elimOrRenameIfVar (avoid2, id, id) (mergeMaps tm fm)

          elimOrRenameIfVar :: (S.Set Var, texpr -> TExpr, texpr' -> TExpr)
                            -> IfVar
                            -> ((S.Set Var, texpr -> TExpr, texpr' -> TExpr), TVar)
          elimOrRenameIfVar (avoidin, elim_rename_t_, elim_rename_f_) = \case
            TrueOnly outVar -> -- Must elim from false branch
              ((avoidin, elim_rename_t_, elim outVar . elim_rename_f_), outVar)
            FalseOnly outVar -> -- Must elim from true branch
              ((avoidin, elim outVar . elim_rename_t_, elim_rename_f_), outVar)
            Both outVarInTrueBranch outVarInFalseBranch ->
              -- Eliminate neither and make a new binding
              ((avoidout,
                rename outVar outVarInTrueBranch . elim_rename_t_,
                rename outVar outVarInFalseBranch . elim_rename_f_),
                outVar)
              where (avoidout, outVar) = notInScopeTV avoidin outVarInTrueBranch
                    rename old new = Let (VarPat new) (Var old)

          (avoid4, mif, dups) =
            addDups avoid3 [condm, tfm]

  -- SUF{\v. body} -> [dups] \v'. [elim v'?] SUF{body}
  Lam v body -> (avoid3, mlam, dups (Lam v' (perhaps_elim_v' suf_body)))
    where (avoid1, mb, suf_body) = sufE avoid body

          (v', mb_no_v, avoid2, perhaps_elim_v') = rebind v mb avoid1

          (avoid3, mlam, dups) = addDups avoid2 [mb_no_v]

  Call f e | f `isThePrimFun` P_sumbuild ->
    sufE avoid (pSum (mkPrimCall P_build e))

  -- TODO: this has rather a lot of duplication with P_build

  -- SUF{map (\s. body) v} -> [dups] map (\s'. SUF{body}) SUF{v}
  Call f (Tuple [Lam s body, v]) | f `isThePrimFun` P_map ->
    (avoid4, mcall, dups (Call f (Tuple [Lam s' (perhaps_elim_s' suf_body), suf_v])))
    where (avoid1, mn, suf_v) = sufE avoid v
          (avoid2, mb, suf_body) = sufE avoid1 body

          (s', mb_no_i, avoid3, perhaps_elim_s') = rebind s mb avoid2

          (avoid4, mcall, dups) = addDups avoid3 [mn, mb_no_i]

  -- SUF{build n (\i. body)} -> [dups] build SUF{n} (\i'. SUF{body})
  Call f (Tuple [n, Lam i body]) | f `isThePrimFun` P_build ->
    (avoid4, mcall, dups (Call f (Tuple [suf_n, Lam i' (perhaps_elim_i' suf_body)])))
    where (avoid1, mn, suf_n) = sufE avoid n
          (avoid2, mb, suf_body) = sufE avoid1 body

          (i', mb_no_i, avoid3, perhaps_elim_i') = rebind i mb avoid2

          (avoid4, mcall, dups) = addDups avoid3 [mn, mb_no_i]

  -- SUF{f e} -> f SUF{e}
  Call f e -> suf_many_and_dup (L1 e) avoid (\(L1 e') -> Call f e')

  -- SUF{(e1, ..., en)} -> [dups] (SUF{e1}, ..., SUF{en})
  Tuple es -> suf_many_and_dup es avoid Tuple

  -- SUF{e1 e2} -> [dups] SUF{e1} SUF{e2}
  App e1 e2 -> suf_many_and_dup (L2 e1 e2) avoid (\(L2 e1' e2') -> App e1' e2')

  -- SUF{dummy T} -> dummy T
  Dummy ty -> suf_many_and_dup L0 avoid (\L0 -> Dummy ty)

  -- SUF{assert cond e} -> SUF{e}
  Assert e1 e2 -> easyVersion
    where -- TODO: The easy version is just to ignore the assertion.
          -- Let's do that for now.  Really we should create a new
          -- assertion.  In the reverse pass that would mean we have
          -- to generate a zero gradient for everything that is
          -- mentioned in the condition.  The pseudocode for the
          -- proper version would be
          --
          -- SUF{assert cond e} -> [dups] assert SUF{cond} SUF{e}
          easyVersion = suf_many_and_dup (L1 e2) avoid (\(L1 e2') -> e2')
          _properVersion =
            suf_many_and_dup (L2 e1 e2) avoid (\(L2 e1' e2') -> Assert e1' e2')

data IfVar' a b = TrueOnly a | FalseOnly b | Both a b
type IfVar = IfVar' TVar TVar

mergeMaps :: M.Map Var a -> M.Map Var b -> M.Map Var (IfVar' a b)
mergeMaps = Merge.merge (Merge.mapMissing (\_ -> TrueOnly))
                        (Merge.mapMissing (\_ -> FalseOnly))
                        (Merge.zipWithMatched (\_ -> Both))

rebind :: TVar -> M.Map Var TVar -> S.Set Var
       -> (TVar, M.Map Var TVar, S.Set Var, TExpr -> TExpr)
rebind v renamings avoid = case pop (tVarVar v) renamings of
            (Just v', renamings_without_v) ->
              -- v was renamed to v'
              (v', renamings_without_v, avoid, id)
            (Nothing, renamings_without_v) ->
              -- v did not appear
              (v', renamings_without_v, avoid', elim v')
              where (avoid', v') = notInScopeTV avoid v

rebindMany :: Traversable t
           => M.Map Var TVar
           -> S.Set Var
           -> t TVar
           -> ((M.Map Var TVar, S.Set Var, TExpr -> TExpr), t TVar)
rebindMany mrhs avoid = mapAccumL (\(m, avoid', f) v ->
                            let (v', m', avoid'', f') = rebind v m avoid'
                            in ((m', avoid'', f . f'), v')
                         ) (mrhs, avoid, id)

sufEMany :: Traversable t
         => S.Set Var -> t TExpr -> (S.Set Var, t (M.Map Var TVar, TExpr))
sufEMany = mapAccumL (\avoid' e ->
                         let (avoid'', me'', e') = sufE avoid' e
                         in (avoid'', (me'', e')))

suf_many_and_dup :: Traversable t
                 => t TExpr
                 -> S.Set Var
                 -> (t TExpr -> TExpr)
                 -> (S.Set Var, M.Map Var TVar, TExpr)
suf_many_and_dup es avoid f = (avoid3, mes', dups (f suf_es))
    where (avoid2, mes_suf_es) = sufEMany avoid es
          suf_es = fmap snd mes_suf_es
          mes = fmap fst mes_suf_es
          (avoid3, mes', dups) = addDups avoid2 (F.toList mes)

addDups :: S.Set Var
        -> [M.Map Var TVar]
        -> (S.Set Var, M.Map Var TVar, TExpr -> TExpr)
addDups avoid = addDupsFromUsages avoid
                 . F.foldl' merge_usages M.empty
                 . (map . fmap) Once
  where addDupsFromUsages avoid1 map_in = (avoid2, map_out, dups)
          where ((avoid2, dups), map_out) = mapAccumL duplicate1 (avoid1, id) map_in
                duplicate1 (avoidA, dupsA) = \case
                  -- v only occurred once. We don't need to dup it.
                  Once v -> ((avoidA, dupsA), v)
                  -- v occurred more than once so come up with a new
                  -- name, v', and duplicate it.
                  Many vs -> ((avoidB, dup v' vs . dupsA), v')
                    where (avoidB, v') = notInScopeTV avoidA (headTwoOrMore vs)

dup :: TVar -> TwoOrMore TVar -> TExpr -> TExpr
dup v vs = Let (TupPat vs') (pDup (length vs') (Var v))
  where vs' = F.toList vs

elim :: TVar -> TExpr -> TExpr
elim v = Let (TupPat []) (pElim (Var v))

pop :: Ord a => a -> M.Map a b -> (Maybe b, M.Map a b)
pop k m = (M.lookup k m, M.delete k m)

data L0 a = L0
  deriving (Functor, Foldable, Traversable)

data L1 a = L1 a
  deriving (Functor, Foldable, Traversable)

data L2 a = L2 a a
  deriving (Functor, Foldable, Traversable)

data L3 a = L3 a a a
  deriving (Functor, Foldable, Traversable)

data L4 a = L4 a a a a
  deriving (Functor, Foldable, Traversable)

data L8 a = L8 a a a a a a a a
  deriving (Functor, Foldable, Traversable)

data NonEmpty a = Once a
                | Many (TwoOrMore a)
  deriving (Functor, Foldable, Traversable)

data TwoOrMore a = TwoOrMore a a [a]
  deriving (Functor, Foldable, Traversable)

headTwoOrMore :: TwoOrMore a -> a
headTwoOrMore (TwoOrMore a _ _) = a

instance Semigroup (TwoOrMore a) where
  TwoOrMore a1 a2 a3s <> TwoOrMore a4 a5 a6s =
    TwoOrMore a1 a2 (a3s ++ [a4, a5] ++ a6s)

-- Concatenation of non-empty lists
instance Semigroup (NonEmpty a) where
  Once a1 <> Once a2 = Many (TwoOrMore a1 a2 [])
  Once a1 <> Many (TwoOrMore a2 a3 a4s) =
    Many (TwoOrMore a1 a2 (a3:a4s))
  Many (TwoOrMore a1 a2 a3s) <> Once a4 =
    Many (TwoOrMore a1 a2 (a3s ++ [a4]))
  Many m1@TwoOrMore{} <> Many m2@TwoOrMore{} =
    Many (m1 <> m2)

-- Associative
merge_usages :: M.Map Var (NonEmpty TVar)
             -> M.Map Var (NonEmpty TVar)
             -> M.Map Var (NonEmpty TVar)
merge_usages = M.unionWith (<>)
