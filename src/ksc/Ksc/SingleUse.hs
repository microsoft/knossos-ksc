{-# LANGUAGE BangPatterns, MonoLocalBinds #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- Ksc.SingleUse: Language for compilation to the SingleUse language
module Ksc.SingleUse(
     SUDef, toSUDefs
     ) where

import Prelude hiding( (<>) )
import Lang
import LangUtils
-- import Prim
import Data.Maybe( mapMaybe )
import qualified Data.Map as M
import qualified Data.Set as S
-- import GHC.Stack( HasCallStack )
import Control.Monad

data SUDef = SUDef { sudef_fun  :: FunId
                   , sudef_arg  :: TVar
                   , sudef_body :: SUStmts
                   , sudef_res  :: TVar }  -- Result type T

data SUStmts
  = SUNop
  | SUSeq SUStmts SUStmts       -- s1 ; s2
  | SUCall TVar Fun TVar        -- x = f(y)
  | SUKonst TVar Konst          -- x = k
  | SUCopy TVar TVar            -- x = y
  | SUElim TVar                 -- Elim x
  | SUDup TVar TVar             -- x = Dup y
  | SUPack TVar [TVar]          -- x = (y1, .. yn)
  | SUUnpack [TVar] TVar        -- (x1,...xn) = y
  | SUIf TVar SUStmts SUStmts   -- if x s1 s2


suSeq :: SUStmts -> SUStmts -> SUStmts
suSeq SUNop s2 = s2
suSeq s1 SUNop = s1
suSeq s1 s2    = SUSeq s1 s2

-----------------------------------------------
--  Pretty printing
-----------------------------------------------

instance Pretty SUDef where
  ppr (SUDef { sudef_fun = f
             , sudef_arg = arg
             , sudef_body = stmts
             , sudef_res = res })
     = vcat [ hang (text "def" <+> ppr f <> parens (pprTypedTVar arg))
                 2 (equals <+> pprTypedTVar res)
            , nest 2 (text "where" <+> pprSUStmts stmts) ]

instance Pretty SUStmts where
  pprPrec _ ss = pprSUStmts ss

pprSUStmts :: SUStmts -> SDoc
pprSUStmts SUNop          = empty
pprSUStmts (SUSeq s1 s2)  = ppr s1 $$ ppr s2
pprSUStmts (SUCall x f y)  = pprLhs x <+> ppr f <> parens (ppr y)
pprSUStmts (SUKonst x k)   = pprLhs x <+> ppr k
pprSUStmts (SUElim x)      = text "Elim" <+> ppr x
pprSUStmts (SUDup x y)     = pprLhs x <+> text "Dup" <+> ppr y
pprSUStmts (SUCopy x y)    = pprLhs x <+> ppr y
pprSUStmts (SUPack x ys)   = pprLhs x <+> parens (pprList ppr ys)
pprSUStmts (SUUnpack xs y) = parens (pprList ppr xs) <+> equals <+> pprTypedTVar y
pprSUStmts (SUIf x s1 s2)  = vcat [ text "If" <+> ppr x
                                  , nest 2 (text "then" <+> pprSUStmts s1)
                                  , nest 2 (text "else" <+> pprSUStmts s2) ]

pprLhs :: TVar -> SDoc
pprLhs v = parens (pprTypedTVar v) <+> equals


-----------------------------------------------
--  Conversion to SU
-----------------------------------------------

toSUDefs :: [TDef] -> [SUDef]
toSUDefs defs = mapMaybe toSUDef_maybe defs

toSUDef_maybe :: TDef -> Maybe SUDef
toSUDef_maybe  (Def { def_fun    = fun
                    , def_pat    = pat
                    , def_rhs    = rhs })
  | Fun f     <- fun
  , UserRhs e <- rhs
  , let (stmts, (arg,res)) = runSUM is0 $
                             do { arg <- unpackArg pat
                                ; dropDead emptySUEnv pvs e
                                ; res <- toSUStmts emptySUEnv e
                                ; return (arg,res) }
  = Just SUDef { sudef_fun  = f
               , sudef_arg  = arg
               , sudef_body = stmts
               , sudef_res  = res }
  | otherwise
  = Nothing
  where
     pvs = patVars pat
     is0 = mkInScopeSet pvs

unpackArg :: Pat -> SUM TVar
unpackArg (VarPat v)  = return v
unpackArg (TupPat vs) = do { arg <- newTVar tup_ty "arg"
                           ; emitStmt (SUUnpack vs arg)
                           ; return arg }
                      where
                           tup_ty = mkTupleTy (map typeof vs)

dropDead :: SUEnv -> [TVar] -> TExpr -> SUM ()
-- Emit an elim-stmt for each variable in vs that is not free in e
dropDead env vs e = emitStmts [ SUElim (lookupSUEnv env v)
                              | v <- vs
                              , not (v `S.member` fvs) ]
  where
    fvs = freeVarsOf e

--------------------------
type SUEnv = M.Map TVar TVar    -- Maps source-language TVars to target-language ones

emptySUEnv :: SUEnv
emptySUEnv = M.empty

lookupSUEnv :: SUEnv -> TVar -> TVar
lookupSUEnv env v
  = case M.lookup v env of
      Nothing -> v
      Just v' -> v'

extendSUEnv :: SUEnv -> [(TVar,TVar)] -> SUEnv
extendSUEnv env prs
  = foldr (\(v1,v2) env -> M.insert v1 v2 env) env prs

------------------------
-- Simple monad for conversion
--   The in-scope set is like a state;
--   the accumulating statments are like a writer monad
newtype SUM a = SUM { unSUM :: InScopeSet -> (InScopeSet, SUStmts, a) }

instance Functor SUM where
  fmap f km = do { r <- km; return (f r) }

instance Applicative SUM where
  pure x = SUM (\is -> (is, SUNop, x))
  (<*>) = ap

instance Monad SUM where
   m >>= k  = SUM (\is ->
                   let !(is1, ss1, v) = unSUM m is
                       !(is2, ss2, r) = unSUM (k v) is1
                   in (is2, ss1 `suSeq` ss2, r))

runSUM :: InScopeSet -> SUM a -> (SUStmts, a)
runSUM is (SUM f) = (stmts, r)
  where
    (_, stmts, r) = f is


newTVar :: Type -> String -> SUM TVar
newTVar ty str
  = SUM (\is -> let (is', tv) = notInScopeTV is (mkTVar ty str)
                in (is', SUNop, tv))

emitStmts :: [SUStmts] -> SUM ()
emitStmts = foldr ((>>) . emitStmt) (return ())

emitStmt :: SUStmts -> SUM ()
emitStmt ss = SUM (\is -> (is, ss, ()))

forkSUM :: SUM a -> SUM (SUStmts, a)
forkSUM (SUM f) = SUM (\is -> let (_, ss, r) = f is
                              in (is, SUNop, (ss,r)))

--------------------------
-- toSUStmts: the main conversion function
--------------------------

toSUStmts :: SUEnv -> TExpr -> SUM TVar

toSUStmts _ (Konst k)
  = do { res <- newTVar (typeof k) "res"
       ; emitStmt (SUKonst res k)
       ; return res}
toSUStmts env (Var v)
  = return (lookupSUEnv env v)

toSUStmts env (Tuple es)
  = do { prs <- split env es
       ; vs <- mapM do_one prs
       ; tres <- newTVar (mkTupleTy (map typeof vs)) "tres"
       ; emitStmt (SUPack tres vs)
       ; return tres }
  where
    do_one (env,e) = toSUStmts env e

toSUStmts env (Call (TFun res_ty fun) e)
  = do { arg <- toSUStmts env e
       ; res <- newTVar res_ty "cres"
       ; emitStmt (SUCall res fun arg)
       ; return res }

toSUStmts env (Let v rhs body)
  = do { v' <- toSUStmts env rhs
       ; let env' = extendSUEnv env [(v,v')]
       ; dropDead env' [v] body
       ; toSUStmts env' body }

toSUStmts env (If b e1 e2)
  = do { bv <- toSUStmts env b
       ; (s1,v1) <- forkSUM (toSUStmts env e1)
       ; (s2,_)  <- forkSUM (do { v2 <- toSUStmts env e2
                                ; unless (v1 == v2) $
                                  emitStmt (SUCopy v1 v2) })
       ; emitStmt (SUIf bv s1 s2)
       ; return v1 }

toSUStmts _ e@(Assert {}) = pprPanic "toSUStmts" (ppr e)
toSUStmts _ e@(Lam {})    = pprPanic "toSUStmts" (ppr e)
toSUStmts _ e@(App {})    = pprPanic "toSUStmts" (ppr e)
toSUStmts _ e@(Dummy {})  = pprPanic "toSUStmts" (ppr e)


------------------------
split :: SUEnv -> [TExpr] -> SUM [(SUEnv, TExpr)]
-- Emit Dup statements, and create a per-TExpr envt
split env es
  = go env ann_es
  where
  go _ [] = return []
  go env ((e,fv_e,fv_tail) : rest)
     = do { prs <- mapM dup $
                   S.toList (fv_e `S.intersection` fv_tail)
          ; let (prs1, prs2) = unzip prs
                env1 = extendSUEnv env prs1
                env2 = extendSUEnv env prs2
          ; prs <- go env2 rest
          ; return ((env1,e) : prs) }


  ann_es :: [( TExpr         -- The expr
             , S.Set TVar    -- Its free vars
             , S.Set TVar)]  -- Union of free vars of all exprs /later/ in the list
  ann_es = foldr do_one [] es
  do_one e triples = (e, freeVarsOf e, all_fvs triples) : triples

  all_fvs []                       = S.empty
  all_fvs ((_, fv_e, fv_tail) : _) = fv_e `S.union` fv_tail

  dup :: TVar -> SUM ((TVar,TVar), (TVar,TVar))
  dup v = do { let ty  = typeof v
                   str = tVarName v
             ; v1  <- newTVar ty str
             ; v2  <- newTVar ty str
             ; tmp <- newTVar (mkTupleTy [ty,ty]) "tmp"
             ; emitStmt (suSeq (SUDup tmp v)
                               (SUUnpack [v1,v2] tmp))
             ; return ((v,v1),(v,v2)) }
