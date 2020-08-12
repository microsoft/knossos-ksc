{-# LANGUAGE BangPatterns, MonoLocalBinds #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- Ksc.SingleUse: Language for compilation to the SingleUse language
module Ksc.SingleUse(
     SUDef, toSUDefs, fromSUDefs
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

headStmts :: SUStmts -> (SUStmts, SUStmts)
-- If headStmts s = (s1,s2)
-- Then   s = s1 `suSeq` s2
-- and    s is not a Seq
-- and    s is SUNop only if s is SUNop
headStmts ss = go ss SUNop
  where
    go (SUSeq s1 s2) tail = go s1 (s2 `suSeq` tail)
    go SUNop         SUNop = (SUNop, SUNop)
    go SUNop         tail  = go tail SUNop
    go s             tail  = (s, tail)

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


{- *********************************************************************
*                                                                      *
*                 Conversion SU -> KSC                                 *
*                                                                      *
********************************************************************* -}

-- In these functions
--   TVar  is *before* substitution
--   SUVar is *after*  substition

type SUVar = TVar  -- After substitution
                   -- See sue_env

toSUDefs :: [TDef] -> [SUDef]
toSUDefs defs = mapMaybe toSUDef_maybe defs

toSUDef_maybe :: TDef -> Maybe SUDef
toSUDef_maybe  (Def { def_fun    = fun
                    , def_pat    = pat
                    , def_res_ty = res_ty
                    , def_rhs    = rhs })
  | Fun f     <- fun
  , UserRhs e <- rhs
  , let (stmts, (arg,res)) = runSUM is0 $
                             do { arg <- unpackArg pat
                                ; res <- newSUVar res_ty "arg"
                                ; let env = initSUEnv res
                                ; dropDead env pvs e
                                ; toSUStmts env e
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
unpackArg (TupPat vs) = do { arg <- newSUVar tup_ty "arg"
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
data SUEnv
  = SUE { sue_subst :: M.Map TVar SUVar   -- Substitution
        , sue_res   :: SUVar }            -- Where to put the result

initSUEnv :: SUVar -> SUEnv
initSUEnv res_var = SUE { sue_subst = M.empty, sue_res = res_var }

lookupSUEnv :: SUEnv -> TVar -> SUVar
lookupSUEnv env v
  = case M.lookup v (sue_subst env) of
      Nothing -> v
      Just v' -> v'

extendSUEnv :: SUEnv -> [(TVar,SUVar)] -> SUEnv
extendSUEnv env@(SUE { sue_subst = subst })  prs
  = env { sue_subst = foldr (\(v1,v2) s -> M.insert v1 v2 s) subst prs }

resultVar :: SUEnv -> SUVar
resultVar env = sue_res env

setResultVar :: SUEnv -> SUVar -> SUEnv
setResultVar env var = env { sue_res = var }

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


newSUVar :: Type -> String -> SUM SUVar
newSUVar ty str
  = SUM (\is -> let (is', tv) = notInScopeTV is (mkTVar ty str)
                in (is', SUNop, tv))

cloneTVar :: TVar -> SUM SUVar
cloneTVar tv
  = SUM (\is -> let (is', tv') = notInScopeTV is tv
                in (is', SUNop, tv'))

emitStmts :: [SUStmts] -> SUM ()
emitStmts = foldr ((>>) . emitStmt) (return ())

emitStmt :: SUStmts -> SUM ()
emitStmt ss = SUM (\is -> (is, ss, ()))

emitCopy :: SUVar -> SUVar -> SUM ()
emitCopy v1 v2 | v1 == v2  = return ()
               | otherwise = emitStmt (SUCopy v1 v2)

forkSUM :: SUM () -> SUM SUStmts
forkSUM (SUM f) = SUM (\is -> let (_, ss, _) = f is
                              in (is, SUNop, ss))

--------------------------
-- toSUStmts: the main conversion function
--------------------------

toSUStmts :: SUEnv -> TExpr -> SUM ()

toSUStmts env (Konst k)
  = emitStmt (SUKonst (resultVar env) k)

toSUStmts env (Var v)
  = emitCopy (resultVar env) (lookupSUEnv env v)

toSUStmts env (Tuple es)
  = do { prs <- split env es
       ; vs <- mapM do_one prs
       ; emitStmt (SUPack (resultVar env) vs) }
  where
    do_one (env,e) = do { tmp <- newSUVar (typeof e) "t"
                        ; toSUStmts (setResultVar env tmp) e
                        ; return tmp }

toSUStmts env (Call (TFun _ fun) e)
  = do { arg <- newSUVar (typeof e) "a"
       ; toSUStmts (setResultVar env arg) e
       ; emitStmt (SUCall (resultVar env) fun arg) }

toSUStmts env (Let v rhs body)
  = do { v' <- cloneTVar v
       ; toSUStmts (setResultVar env v') rhs
       ; let env' = extendSUEnv env [(v,v')]
       ; dropDead env' [v] body
       ; toSUStmts env' body }

toSUStmts env (If b e1 e2)
  = do { bv <- newSUVar (typeof b) "b"
       ; toSUStmts (setResultVar env bv) b
       ; s1 <- forkSUM (toSUStmts env e1)
       ; s2 <- forkSUM (toSUStmts env e2)
       ; emitStmt (SUIf bv s1 s2) }

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
             ; v1  <- newSUVar ty str
             ; v2  <- newSUVar ty str
             ; tmp <- newSUVar (mkTupleTy [ty,ty]) "tmp"
             ; emitStmt (suSeq (SUDup tmp (lookupSUEnv env v))
                               (SUUnpack [v1,v2] tmp))
             ; return ((v,v1),(v,v2)) }



{- *********************************************************************
*                                                                      *
*                 Conversion SU -> KSC                                 *
*                                                                      *
********************************************************************* -}

fromSUDefs :: [SUDef] -> [TDef]
fromSUDefs cldefs = map fromSUDef cldefs

fromSUDef :: SUDef -> TDef
fromSUDef (SUDef { sudef_fun = f
                 , sudef_arg = arg
                 , sudef_body = body
                 , sudef_res = res })
  = Def { def_fun    = Fun f
        , def_pat    = pat
        , def_res_ty = typeof res
        , def_rhs    = UserRhs (fromSUStmts body' (Var res)) }
  where
    -- This bit sees if 'arg' is immediately unpacked;
    -- if so, use a TupPat
    (pat, body') = case headStmts body of
                     (SUUnpack pvs arg', ss)
                        | arg == arg' -> (TupPat pvs, ss)
                     _                -> (VarPat arg, body)

fromSUStmts :: SUStmts -> TExpr -> TExpr
fromSUStmts SUNop           e = e
fromSUStmts (SUSeq s1 s2)   e = fromSUStmts s1 (fromSUStmts s2 e)
fromSUStmts (SUCall r f a)  e = Let r (Call (TFun (typeof r) f) (Var a)) e
fromSUStmts (SUKonst x k)   e = Let x (Konst k) e
fromSUStmts (SUCopy x y)    e = Let x (Var y) e
fromSUStmts (SUElim {})     e = e
fromSUStmts (SUDup t y)     e = Let t (Tuple [Var y, Var y]) e
fromSUStmts (SUPack x ys)   e = Let x (Tuple (map Var ys))   e
fromSUStmts (SUUnpack xs y) e = mkLets prs e
  where n = length xs
        prs = [ (x, pSel i n (Var y)) | (x,i) <- xs `zip` [1..] ]

fromSUStmts (SUIf b s1 s2) e = If (Var b) (fromSUStmts s1 e) (fromSUStmts s2 e)
                               -- NB: duplicates e, which may a problem one day
