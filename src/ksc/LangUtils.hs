{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE FlexibleInstances #-}

module LangUtils (
  -- Substitution 
  substEMayCapture,

  -- Equality
  cmpExpr,

  -- Free vars
  notFreeIn, newVarNotIn,

  -- Tests
  LangUtils.hspec,
  test_FreeIn
  ) where
  
import Lang
import qualified Data.Map as M
import Test.Hspec
import Debug.Trace( trace )

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
substEMayCapture subst (Call f e)     = Call f (substEMayCapture subst e)
substEMayCapture subst (If b t e)     = If (substEMayCapture subst b) (substEMayCapture subst t) (substEMayCapture subst e)
substEMayCapture subst (Tuple es)     = Tuple (map (substEMayCapture subst) es)
substEMayCapture subst (App e1 e2)    = App (substEMayCapture subst e1) (substEMayCapture subst e2)
substEMayCapture subst (Assert e1 e2) = Assert (substEMayCapture subst e1) (substEMayCapture subst e2)
substEMayCapture subst (Lam v e)      = Lam v (substEMayCapture (v `M.delete` subst) e)
substEMayCapture subst (Let v r b)    = Let v (substEMayCapture subst r) $
                                          substEMayCapture (v `M.delete` subst) b

-----------------------------------------------
--     Equality modulo alpha
-----------------------------------------------

instance Eq TExpr where
  e1 == e2 = case e1 `cmpExpr` e2 of
               EQ -> True
               _  -> False

instance Ord TExpr where
  compare = cmpExpr

thenCmp :: Ordering -> Ordering -> Ordering
EQ `thenCmp` o = o
o  `thenCmp` _ = o

cmpExpr :: TExpr -> TExpr -> Ordering
cmpExpr e1 e2
 = go e1 M.empty e2
 where
   go :: TExpr -> M.Map TVar TVar -> TExpr -> Ordering
   go (Konst k1) subst e2
     = case e2 of
         Konst k2 -> k1 `compare` k2
         _        -> LT

   go (Var v1) subst e2
     = case e2 of
         Konst {} -> GT
         Var v2   -> v1 `compare` M.findWithDefault v2 v2 subst
         _        -> LT

   go (Call f1 e1) subst e2
     = case e2 of
         Konst {} -> GT
         Var {} -> GT
         Call f2 e2 -> (f1 `compare` f2) `thenCmp` (go e1 subst e2)
         _ -> LT

   go (Tuple es1) subst e2
     = case e2 of
         Konst {} -> GT
         Var {}  -> GT
         Call {} -> GT
         Tuple es2 -> gos es1 subst es2
         _        -> LT

   go (Lam b1 e1) subst e2
      = case e2 of
         Konst {}  -> GT
         Var {}    -> GT
         Call {}   -> GT
         Tuple es  -> GT
         Lam b2 e2 -> go e1 (M.insert b2 b1 subst) e2
         _         -> LT

   go (App e1a e1b) subst e2
     = case e2 of
         Konst {} -> GT
         Var {}   -> GT
         Call {}  -> GT
         Tuple {} -> GT
         Lam {}   -> GT
         App e2a e2b -> go e1a subst e2a `thenCmp` go e1b subst e2b
         _           -> LT

   go (Let b1 r1 e1) subst e2
     = case e2 of
         If {}     -> LT
         Assert {} -> LT
         Let b2 r2 e2 ->
                go r1 subst r2 `thenCmp` go e1 (M.insert b2 b1 subst) e2
         _ -> GT

   go (If e1c e1t e1f) subst e2
      = case e2 of
          Assert {} -> LT
          If e2c e2t e2f -> go e1c subst e2c `thenCmp`
                            go e1t subst e2t `thenCmp`
                            go e1f subst e2f
          _ -> GT

   go (Assert e1a e1b) subst e2
      = case e2 of
          Assert e2a e2b -> go e1a subst e2a `thenCmp` go e1b subst e2b
          _              -> GT

   gos :: [TExpr] -> M.Map TVar TVar -> [TExpr] -> Ordering
   gos [] subst [] = EQ
   gos [] subst (_:_) = LT
   gos (_:_) subst [] = GT
   gos (e1:es1) subst (e2:es2) = go e1 subst e2 `thenCmp` gos es1 subst es2


-----------------------------------------------
--     Free variables
-----------------------------------------------

notFreeIn :: TVar -> TExpr -> Bool
notFreeIn v e = go v e
 where
   go:: TVar -> TExpr -> Bool
   go v (Var v2) = v /= v2
   go v (Konst _) = True
   go v (Tuple es) = all (go v) es
   go v (If b t e) = go v b && go v t && go v e
   go v (Call _ e) = go v e
   go v (App f a)  = go v f && go v a
   go v (Let v2 r b) = go v r && (v == v2 || go v b)
   go v (Lam v2 e)   = v == v2 || go v e
   go v (Assert e1 e2) = go v e1 && go v e2

-----------------

newVarNotIn :: Type -> TExpr -> TVar
newVarNotIn ty e = go ty e 1 -- FIXME start with hash of e to reduce retries
  where
    go ty e n
      | v `notFreeIn` e = v
      | otherwise       = trace ("newVarNotIn: Var " ++ show v ++ " was bound in E, retry")
                          (go ty e (n + 1))
      where
         v = mkTVar ty ("_t" ++ show n)

hspec :: Spec
hspec = do
    let var :: String -> TVar
        var s = TVar TypeFloat (Simple s)
        fun :: String -> TFun
        fun s = TFun TypeFloat (Fun (UserFun s))
        e  = Call (fun "f") (Var (var "i"))
        e2 = Call (fun "f") (Tuple [Var (var "_t1"), kInt 5])
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
