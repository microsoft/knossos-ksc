-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances, LambdaCase,
             PatternSynonyms, StandaloneDeriving,
	     ScopedTypeVariables, TypeApplications #-}

module Annotate (
  annotDecls, lintDefs
  ) where

import Lang
import LangUtils
import KMonad
import Prim
import qualified Data.Map   as Map
import GHC.Stack
import Control.Monad( ap )
import Data.List( intersperse )

-----------------------------------------------
--     The type inference pass
-----------------------------------------------

annotDecls :: GblSymTab -> [Decl] -> KM (GblSymTab, [TDecl])
annotDecls gbl_env decls
  = runTc "Type checking" init_env $
    do { rec_defs <- mapM mk_rec_def defs
       ; extendGblSTM rec_defs $
         go decls }
  where
    init_env :: TcEnv
    init_env = TCE { tce_ctxt = []
                   , tce_st = newSymTab gbl_env }

    go [] = do { st <- getSymTabTc
               ; return (gblST st, []) }

    go (decl:decls)
      = do { mb_tdecl <- tryTc (tcDeclX decl)
           ; case mb_tdecl of
                Nothing    -> go decls
                Just tdecl -> do { (env', tdecls) <- extendGblEnv tdecl (go decls)
                                 ; return (env', tdecl:tdecls) } }

    defs = [ def | DefDecl def <- decls ]

    -- mk_rec_def: extend the global env with the
    -- user-specified types for each top-level function
    -- We don't have a type-checked body yet, but that
    -- doesn't matter because we aren't doing inlining
    mk_rec_def :: DefX Parsed -> TcM TDef
    mk_rec_def (Def { def_fun = fun, def_args = args, def_res_ty = res_ty })
       = addCtxt (text "In the definition of" <+> ppr fun) $
         tcArg args $ \args' ->
         do { return (Def { def_fun = fun, def_args = args'
                          , def_res_ty = res_ty
                          , def_rhs = StubRhs }) }

-----------------------------------------------
--     A Lint function to do type checking
-----------------------------------------------

lintDefs :: String -> GblSymTab -> [TDef] -> KM ()
-- Returns a list of error messages
-- The incoming environment already has a type for
-- each top level function
lintDefs what gbl_env defs
  = do { runTc what init_env (mapM_ (tryTc . tcDef) defs)
       ; printK (text "Linted" <+> text what) }
  where
    init_env = TCE { tce_ctxt = []
                   , tce_st = newSymTab gbl_env }


-----------------------------------------------
--     Guts of the type checker
-----------------------------------------------

tcDeclX :: InPhase p => DeclX p -> TcM TDecl
tcDeclX (DefDecl def)   = do { def' <- tcDef def
                             ; return (DefDecl def') }
tcDeclX (RuleDecl rule) = do { rule' <- tcRule rule
                             ; return (RuleDecl rule') }

tcDef :: InPhase p => DefX p -> TcM TDef
tcDef (Def { def_fun    = fun
           , def_args   = vars
           , def_res_ty = res_ty
           , def_rhs    = rhs })
  = addCtxt (text "In the definition of" <+> ppr fun) $
    do { tcArg vars $ \vars' ->
    do { rhs' <- tcRhs fun rhs res_ty
       ; return (Def { def_fun = fun, def_args = vars'
                     , def_rhs = rhs', def_res_ty = res_ty })
    }}

-- CPS form of extendLclSTM.  Formerly types could contain expressions
-- so they were typechecked.  The name of this function is a vestige
-- of those times.
tcArg :: TVarX -> (TVar -> TcM a) -> TcM a
tcArg tv continueWithArg
  = do { extendLclSTM [tv] $ continueWithArg tv }

tcRhs :: InPhase p => Fun -> RhsX p -> Type -> TcM TRhs
tcRhs _ StubRhs _ = return StubRhs
tcRhs _ EDefRhs _ = return EDefRhs
tcRhs fun (UserRhs rhs) res_ty
  = do { TE rhs' rhs_ty <- tcExpr rhs
       ; checkTypes res_ty rhs_ty $
         text "Function result type mis-match for" <+> ppr fun
       ; return (UserRhs rhs') }

tcRule :: InPhase p => RuleX p -> TcM TRule
tcRule (Rule { ru_name = name, ru_qvars = qvars
             , ru_lhs = lhs, ru_rhs = rhs })
  = addCtxt (text "In the rule with lhs:" <+> ppr lhs) $
    do { extendLclSTM qvars $
    do { TE lhs' lhs_ty <- tcExpr lhs
       ; TE rhs' rhs_ty <- tcExpr rhs
       ; checkTypes lhs_ty rhs_ty $
         text "LHS and RHS of a rule have different types"
       ; return (Rule { ru_name = name, ru_qvars = qvars
                      , ru_lhs = lhs', ru_rhs = rhs' })
    }}

tcExpr :: forall p. InPhase p => ExprX p -> TcM TypedExpr
  -- Naming conventions in this function:
  --  e   Expr [TypeUnknown]
  --  ae  Expr [Annotated]
  --  tye Type of e
  --  xe  ExprX part of e
tcExpr (Var vx)
  = do { let (var, mb_ty) = getVar @p vx
       ; ty <- tcVar var mb_ty
       ; return (TE (Var $ TVar ty var) ty) }

tcExpr (Konst k)
  = return (TE (Konst k) (typeofKonst k))

tcExpr (Call fx es)
  = do { let (fun, mb_ty) = getFun @p fx
       ; pairs <- addCtxt (text "In the call of:" <+> ppr fun) $
                  tcExpr es

       ; res_ty <- lookupGblTc fun pairs

       ; res_ty <- checkTypes_maybe mb_ty res_ty $
         text "Function call type mismatch for" <+> ppr fun
       ; let call' = Call (TFun res_ty fun) (exprOf pairs)
       ; return (TE call' res_ty) }

tcExpr (Let vx rhs body)
  = do { let (var, mb_ty) = getLetBndr @p vx
       ; TE arhs rhs_ty <- addCtxt (text "In the rhs of the binding for:" <+> ppr var) $
                           tcExpr rhs
       ; rhs_ty <- checkTypes_maybe mb_ty rhs_ty $
         text "Let binding mis-match for" <+> ppr var
       ; let tvar = TVar rhs_ty var
       ; TE abody tybody <- extendLclSTM [tvar] (tcExpr body)

       ; return (TE (Let tvar arhs abody) tybody) }

tcExpr (Tuple es)
  = do { pairs <- mapM tcExpr es
       ; let (aes, tys) = unzipTEs pairs
       ; return (TE (Tuple aes) (TypeTuple tys)) }

tcExpr (Lam tv body)
  = do { TE abody tybody <- extendLclSTM [tv] (tcExpr body)
       ; return (TE (Lam tv abody)
                    (TypeLam (typeof tv) tybody)) }

tcExpr (If cond texpr fexpr)
  = do { TE acond tycond   <- tcExpr cond
       ; TE atexpr tytexpr <- tcExpr texpr
       ; TE afexpr tyfexpr <- tcExpr fexpr
       ; checkTypes TypeBool tycond $
         text "Predicate of 'if' has non-boolean type"
       ; checkTypes tytexpr tyfexpr $
         text "Branches of 'if' have different types"
       ; return (TE (If acond atexpr afexpr) tytexpr) }

tcExpr (Assert cond body)
  = do { TE acond tycond <- tcExpr cond
       ; TE abody tybody <- tcExpr body
       ; checkTypes TypeBool tycond $
         text "Predicate of 'assert' has non-boolean type"
       ; return (TE (Assert acond abody) tybody) }

tcExpr (App fun arg)
  = do { TE afun fun_ty <- tcExpr fun
       ; TE aarg arg_ty <- tcExpr arg
       ; res_ty <- case fun_ty of
           TypeLam ty1 ty2
              -> do { checkTypes ty1 arg_ty $
                      text "Function application mis-match"
                    ; return ty2 }
           _ -> do { addErr (text "Function does not have function type" <+> ppr fun_ty)
                   ; return TypeUnknown }
       ; return (TE (App afun aarg) res_ty) }

tcExpr (Dummy m_ty)
  = case getMType @p m_ty of
      Just ty -> return (TE (Dummy ty) ty)
      Nothing -> do { addErr (text "Dummy var in untyped code")
                    ; return (TE (Dummy TypeUnknown) TypeUnknown) }

tcVar :: Var -> Maybe Type -> TcM Type
tcVar var mb_ty
  = do { ty <- lookupLclTc var
       ; checkTypes_maybe mb_ty ty $
         text "Variable occurrence mis-match for" <+> ppr var }

------------------------------------------------------------------------------
-- callResultTy_maybe is given a (global) function and the type of its
-- argument, and returns the type of its result.
--
-- It has special cases for a bunch opuilt-in functions with polymorphic
-- types; that is, where the result type is a function of the argument types
-- Otherwise it just looks in the global symbol table.
callResultTy_maybe :: SymTab -> Fun -> Type
                   -> Either SDoc Type
callResultTy_maybe env fun args
  | is_user_fun fun
  = userCallResultTy_maybe fun (gblST env) args
  | otherwise
  = primCallResultTy_maybe fun (typeof args)
  where
    is_user_fun = isUserFun . funIdOfFun

-----------------------------------------------
--     The typecheck monad
-----------------------------------------------

userCallResultTy_maybe :: HasCallStack => Fun -> GblSymTab
                       -> Type -> Either SDoc Type
userCallResultTy_maybe fn env args
  = case lookupGblST fn env of
      Just def -> userCallResultTy_help def args
      Nothing  -> Left (text "Not in scope: userCall:" <+> ppr fn $$ ppr env)

userCallResultTy_help :: HasCallStack
                      => TDef -> Type -> Either SDoc Type
userCallResultTy_help (Def { def_fun  = fn
                           , def_res_ty = ret_ty
                           , def_args = params })
                      args
  = case check_args bndr_tys arg_tys of
      Just err -> Left err
      Nothing  -> Right ret_ty
  where
    bndr_tys   = tVarType params
    arg_tys    = typeof args

    check_args :: Type -> Type -> Maybe SDoc
    -- Return (Just err) if there's a wrong-ness
    check_args bndr_ty arg_ty
      | bndr_ty `compatibleType` arg_ty
      = Nothing
      | otherwise
      = Just (hang (text "Type mis-match in argument"
                     <+> text "of call to" <+> ppr fn)
                 2 (vcat [ text "Expected:" <+> ppr bndr_ty
                         , text "Actual:  " <+> ppr arg_ty ]))

-----------------------------------------------
--     The typecheck monad
-----------------------------------------------

data TcEnv
  = TCE { tce_ctxt :: [SDoc]   -- Context, innermost first
        , tce_st   :: SymTab }

newtype TcM a = TCM { unTc :: TcEnv -> [SDoc] -> (Maybe a, [SDoc]) }
-- Just a writer monad on [SDoc]
--
-- Returns (Nothing, errs) if typechecking failure; it's an exception
--   In that case there should always be a message in the
--   returned 'errs'
-- Returns (Just r, errs) if this piece of typechecking succeeded;
--   We can recover from errors (via tryTc), so the 'errs' may
--   be non-empty even in the (Just r) case, meaning that errors
--   have been encountered, but we have recovered from them.
--
-- Needless to say, the incoming [SDoc] messages are always included
--  in the returned [SDoc]; it's a writer monad that extends the
--  error-message accumulator

instance Functor TcM where
  fmap f (TCM m) = TCM (\ctxt ds -> case m ctxt ds of
                                     (r, ds') -> (fmap f r, ds'))

instance Applicative TcM where
  pure  = return
  (<*>) = ap

instance Monad TcM where
  return v = TCM (\_ ds -> (Just v, ds))
  TCM m >>= k = TCM $ \ctxt ds ->
                case m ctxt ds of
                  (Just r1, ds') -> unTc (k r1) ctxt ds'
                  (Nothing, ds') -> (Nothing, ds')

tryTc :: TcM a -> TcM (Maybe a)
tryTc (TCM m)
  = TCM $ \env ds ->
    case m env ds of
      (mb_r, ds) -> (Just mb_r,  ds)

runTc :: String -> TcEnv -> TcM a -> KM a
runTc what init_env (TCM m)
  | Just res <- mb_res
  , null rev_errs
  = return res
  | otherwise
  = do { printK $
         vcat [ text ""
              , text "--------------------------"
              , text "Type errors in" <+> text what
              , nest 2 $ vcat $ intersperse (text "") $
                reverse rev_errs
              , text "End of type errors"
              , text "--------------------------"
              , text "" ]
       ; error "Exiting" }
  where
    (mb_res, rev_errs) = m init_env []

addErr :: SDoc -> TcM a
addErr d = TCM (\env ds -> (Nothing, mk_err env d : ds))
  where
    mk_err env d =  vcat (d : tce_ctxt env)

addCtxt :: SDoc -> TcM a -> TcM a
addCtxt cd (TCM m) = TCM $ \env@(TCE { tce_ctxt = cds }) ds ->
                     m (env { tce_ctxt = cd : cds }) ds

checkTypes_maybe :: Maybe Type -> Type -> SDoc -> TcM Type
checkTypes_maybe mb_expected actual herald
  -- Check the type match, returning the expected
  -- type if supplied, otherwise the actual one.
  --
  -- This is important because `checkTypes` is ultimately implemented in
  -- terms of `eqType` and so the two types may not actually be
  -- structurally equal.
  --
  -- For Calls and Lets the expected type is the type on the Fun or binder
  -- respectively (if it exists) and (when it exists) we want type
  -- checking to keep it unchanged.
  = case mb_expected of
      Just expected -> do { checkTypes expected actual herald
                          ; return expected }
      Nothing       -> return actual

checkTypes :: Type -> Type -> SDoc -> TcM ()
checkTypes exp_ty act_ty herald
  | exp_ty `compatibleType` act_ty
  = return ()
  | otherwise
  = addErr $ hang herald 2 $
    vcat [ text "Expected type:" <+> ppr exp_ty
         , text "Actual type:  " <+> ppr act_ty ]

compatibleType :: Type -> Type -> Bool
-- Returns True if act_ty is acceptable where exp_ty is expected
compatibleType exp_ty act_ty
  | TypeUnknown <- act_ty
  = True
  | otherwise
  = exp_ty `eqType` act_ty   -- Simple syntactic equality for now

extendGblSTM :: [TDef] -> TcM a -> TcM a
extendGblSTM defs = modifyEnvTc add_defs
  where
    add_defs st = st { gblST = extendGblST (gblST st) defs }

extendLclSTM :: [TVar] -> TcM a -> TcM a
extendLclSTM vars = modifyEnvTc add_vars
  where
    add_vars st = st { lclST = extendLclST (lclST st) vars }

extendGblEnv :: TDecl -> TcM a -> TcM a
extendGblEnv (RuleDecl {}) = id
extendGblEnv (DefDecl tdef@(Def { def_fun = f }))
  = modifyEnvTc (modifyGblST (stInsertFun f tdef))

modifyEnvTc :: (SymTab -> SymTab) -> TcM a -> TcM a
modifyEnvTc extend (TCM f)
  = TCM (\env -> f (env { tce_st = extend (tce_st env) }))

getSymTabTc :: TcM SymTab
getSymTabTc = TCM (\env ds -> (Just (tce_st env), ds))

lookupLclTc :: Var -> TcM Type
lookupLclTc v
  = do { st <- getSymTabTc
       ; case Map.lookup v (lclST st) of
           Nothing -> do {
                             addErr (vcat [ text "Not in scope: local var/tld:" <+> ppr v
                                          , text "Envt:" <+> gblDoc st ])
                             ; return TypeUnknown
                             }
           Just ty -> return ty }

lookupGblTc :: Fun -> TypedExpr -> TcM Type
lookupGblTc fun args
  = do { st <- getSymTabTc
       ; case callResultTy_maybe st fun (typeof args) of
           Left err -> do { addErr $ hang err 2 (mk_extra st)
                          ; return TypeUnknown }
           Right res_ty -> return res_ty }
  where
    mk_extra st
      = vcat [ text "In a call of:" <+> ppr fun <+> parens (text (show fun))
             , text " Arg types:" <+> ppr (typeof args)
             , text " Args:"      <+> ppr (exprOf args)
             , text "ST lookup:" <+> ppr (Map.lookup fun (gblST st))
             -- This is very verbose, and obscures error messages, but can be useful for typos.
             -- Perhaps think about printing it only for failed lookup of userfun
             -- , text "ST keys:" <+> gblDoc
             ]

gblDoc :: SymTab -> SDoc
gblDoc st = vcat (map (text . show) (Map.keys (gblST st)))
