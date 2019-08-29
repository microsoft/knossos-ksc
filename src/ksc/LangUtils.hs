-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances, LambdaCase,
             PatternSynonyms, StandaloneDeriving, AllowAmbiguousTypes,
	     ScopedTypeVariables, TypeApplications #-}

module LangUtils (
  -- Functions over expressions
  isTrivial,

  -- Substitution
  substEMayCapture,

  -- Equality
  cmpExpr,

  -- Free vars
  notFreeIn, notFreeInType, newVarNotIn, freeVarsOf,
  paramsSizeBinders, typesSizeBinders,

  -- Tests
  LangUtils.hspec, test_FreeIn,

  -- Symbol table
  GblSymTab, extendGblST, lookupGblST, emptyGblST, modifyGblST,
  stInsertFun,
  LclSymTab, extendLclST, reduceLclST,
  SymTab(..), newSymTab, emptySymTab

  ) where

import Lang
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec
import Debug.Trace( trace )
import Data.List( nub )

-----------------------------------------------
--     Functions over expressions
-----------------------------------------------

isTrivial :: TExpr -> Bool
isTrivial (Tuple [])    = True
isTrivial (Var {})      = True
isTrivial (Konst {})    = True
isTrivial (Call f args) = all isDummy args
isTrivial (Assert _ e2) = isTrivial e2
isTrivial _ = False

isDummy :: TExpr -> Bool
isDummy (Var v) = isDummyVar (tVarVar v)
isDummy _       = False

-----------------------------------------------
--     Substitution
-----------------------------------------------

substEMayCapture :: M.Map TVar TExpr -> TExpr -> TExpr
-- NOT YET using capture-avoiding substitution
-- But substEMayCapture is not really used
-- The heavy lifting is done by OptLets.optLetsE, which /does/
-- do capture-avoiding substitution.
substEMayCapture subst (Konst k)      = Konst k
substEMayCapture subst (Var v)        = case M.lookup v subst of
                               Just e  -> e
                               Nothing -> Var v
substEMayCapture subst (Call f es)    = Call f (map (substEMayCapture subst) es)
substEMayCapture subst (If b t e)     = If (substEMayCapture subst b) (substEMayCapture subst t) (substEMayCapture subst e)
substEMayCapture subst (Tuple es)     = Tuple (map (substEMayCapture subst) es)
substEMayCapture subst (App e1 e2)    = App (substEMayCapture subst e1) (substEMayCapture subst e2)
substEMayCapture subst (Assert e1 e2) = Assert (substEMayCapture subst e1) (substEMayCapture subst e2)
substEMayCapture subst (Lam v e)      = Lam v (substEMayCapture (v `M.delete` subst) e)
substEMayCapture subst (Dup{})        = error "substEMayCapture Dup unimplemented"
substEMayCapture subst (Elim{})       = error "substEMayCapture Elim unimplemented"
substEMayCapture subst (Let v r b)    = Let v (substEMayCapture subst r) $
                                          substEMayCapture (v `M.delete` subst) b

-----------------------------------------------
--     Free variables
-----------------------------------------------

paramsSizeBinders :: forall p. InPhase p =>  [TVarX p] -> [TVar]
-- Get the vector size binders bound by the type of the parameters
-- E.g.   (x :: Vec n Float, y :: (Vec m (Vec n Float), Float))
--        binds m and n
-- Notes:
--   1. we do not consider (Vec (n+m) Float) as binding anything
--   2. a duplicate (e.g. n above) enters twice with the same definition
--      at codegen the second defn is an assert of equality
paramsSizeBinders vs = nub (concatMap paramSizeBinders vs)

paramSizeBinders :: forall p. InPhase p => TVarX p -> [TVar]
paramSizeBinders (TVar TypeInteger v)
  = [TVar TypeInteger v]     -- An argument of type Integer
paramSizeBinders (TVar ty _)
  = typeSizeBinders ty       -- For other args, look in their types

typesSizeBinders :: forall p. InPhase p => [TypeX p] -> [TVar]
typesSizeBinders tys = concatMap typeSizeBinders tys

typeSizeBinders :: forall p. InPhase p => TypeX p -> [TVar]
-- Find any (Vec n ty) types, and return the 'n' part
typeSizeBinders ty = go ty
  where
    go (TypeVec (Var vx) ty) = TVar TypeInteger v : go ty
                             where
                               (v, _) = getVar @p vx
    go (TypeTuple ts)        = concatMap go ts
    go (TypeLM t1 t2)        = go t1 ++ go t2
    go _                     = []

freeVarsOf :: TExpr -> S.Set TVar
freeVarsOf = go
  where
   go :: TExpr -> S.Set TVar
   go (Var v)        = S.singleton v
   go (Konst _)      = S.empty
   go (Tuple es)     = S.unions (map go es)
   go (If b t e)     = go b `S.union` go t `S.union` go e
   go (Call _ es)    = S.unions (map go es)
   go (App f a)      = go f `S.union` go a
   go (Let v r b)    = go r `S.union` (S.delete v $ go b)
   go (Dup (v1, v2) r b) =
     r `S.insert` (S.delete v1 $ S.delete v2 $ go b)
   go (Elim v b)     = S.insert v (go b)
   go (Lam v e)      = S.delete v $ go e
   go (Assert e1 e2) = go e1 `S.union` go e2

notFreeIn :: TVar -> TExpr -> Bool
notFreeIn = go
 where
   go:: TVar -> TExpr -> Bool
   go v (Var v2)     = v /= v2
   go v (Konst _)    = True
   go v (Tuple es)   = all (go v) es
   go v (If b t e)   = go v b && go v t && go v e
   go v (Call _ e)   = all (go v) e
   go v (App f a)    = go v f && go v a
   go v (Let v2 r b) = go v r && (v == v2 || go v b)
   go v (Lam v2 e)   = v == v2 || go v e
   go v (Dup (v1, v2) r b) =
     (v /= r) && (go v b || v == v1 || v == v2)
   go v (Elim v2 b)  = go v b && (v /= v2)
   go v (Assert e1 e2) = go v e1 && go v e2

notFreeInType :: TVar -> Type -> Bool
notFreeInType v (TypeTuple tys) = all (notFreeInType v) tys
notFreeInType v (TypeVec sz ty) = v `notFreeIn` sz && v `notFreeInType` ty
notFreeInType v (TypeLam t1 t2) = v `notFreeInType` t1 && v `notFreeInType` t2
notFreeInType v (TypeLM t1 t2)  = v `notFreeInType` t1 && v `notFreeInType` t2
notFreeInType _ _ = True

-----------------

newVarNotIn :: Type -> TExpr -> TVar
newVarNotIn ty e = go ty e 1 -- FIXME start with hash of e to reduce retries
  where
    go ty e n
      | v `notFreeIn` e = v
      | otherwise       = trace ("newVarNotIn: Var " ++ pps v ++ " was bound in E, retry")
                          (go ty e (n + 1))
      where
         v = mkTVar ty ("_t" ++ show n)

hspec :: Spec
hspec = do
    let var :: String -> TVar
        var s = TVar TypeFloat (Simple s)
        fun :: String -> TFun
        fun s = TFun TypeFloat (Fun (UserFun s))
        e  = Call (fun "f") [Var (var "i")]
        e2 = Call (fun "f") [Var (var "_t1"), kInt 5]
    describe "notFreeIn" $ do
      it ("i notFreeIn " ++ show (ppr (e::TExpr))) $
        (var "i" `notFreeIn` e) `shouldBe` False
      it ("x not notFreeIn " ++ show (ppr (e::TExpr))) $
        (var "x" `notFreeIn` e) `shouldBe` True
    describe "newVarNotIn" $ do
      it "not in, so new var is _t1..." $
        newVarNotIn TypeFloat e `shouldBe` (var "_t1")
      it "in, so new var is _t2..." $
        newVarNotIn TypeFloat e2 `shouldBe` (var "_t2")

test_FreeIn :: IO ()
test_FreeIn = Test.Hspec.hspec LangUtils.hspec


-----------------------------------------------
--     Symbol table, ST, maps variables to types
-----------------------------------------------

-- Global symbol table
type GblSymTab = M.Map Fun TDef
   -- Maps a function to its definition, which lets us
   --   * Find its return type
   --   * Inline it
   -- Domain is UserFun, and perhaps the Grad of PrimFuns

-- Local symbol table
type LclSymTab = M.Map Var Type
   -- The Type is the type of the variable

-- Entire symbol table
data SymTab
  = ST { gblST :: GblSymTab
       , lclST :: LclSymTab
    }

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
   ppr m = braces $ fsep  $ punctuate comma $
           [ ppr k <+> text ":->" <+> ppr v | (k,v) <- M.toList m ]

instance Pretty SymTab where
  ppr (ST { lclST = lcl_env, gblST = gbl_env })
    = vcat [ hang (text "Global symbol table:")
                2 (ppr gbl_env)
           , hang (text "Local symbol table:")
                2 (ppr lcl_env) ]

emptyGblST :: GblSymTab
emptyGblST = M.empty

emptySymTab :: SymTab
emptySymTab = ST { gblST = M.empty, lclST = M.empty }

newSymTab :: GblSymTab -> SymTab
newSymTab gbl_env = ST { gblST = gbl_env, lclST = M.empty }

stInsertFun :: Fun -> TDef -> GblSymTab -> GblSymTab
stInsertFun = M.insert

lookupGblST :: HasCallStack => Fun -> GblSymTab -> Maybe TDef
lookupGblST = M.lookup

extendGblST :: GblSymTab -> [TDef] -> GblSymTab
extendGblST = foldl add
  where
    add env def@(Def { def_fun = f }) = stInsertFun f def env

modifyGblST :: (GblSymTab -> GblSymTab) -> SymTab -> SymTab
modifyGblST g = \env -> env { gblST = g (gblST env) }

extendLclST :: LclSymTab -> [TVar] -> LclSymTab
extendLclST lst vars = foldl add lst vars
  where
    add :: LclSymTab -> TVar -> LclSymTab
    add env (TVar ty v) = M.insert v ty env

reduceLclST :: LclSymTab -> [TVar] -> LclSymTab
reduceLclST lst vars = foldl remove lst vars
  where remove :: LclSymTab -> TVar -> LclSymTab
        remove env (TVar _ v) = M.delete v env
