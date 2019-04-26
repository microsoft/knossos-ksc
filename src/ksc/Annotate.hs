{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances, LambdaCase,
             PatternSynonyms, StandaloneDeriving, AllowAmbiguousTypes,
	     ScopedTypeVariables, TypeApplications #-}

module Annotate (
  annotDecls, lintDefs
  ) where

import Lang
import OptLet
import LangUtils
import KMonad
import Prim
import qualified Data.Map   as Map
import GHC.Stack
import Control.Monad( ap, unless, when )
import Data.List( intersperse, nub, (\\) )

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
         extendLclSTM (paramsSizeBinders args) $
         do { args' <- mapM tcTVar args
            ; res_ty' <- tcType res_ty
            ; return (Def { def_fun = fun, def_args = args'
                          , def_res_ty = res_ty'
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
    extendLclSTM (paramsSizeBinders vars) $
    do { tcArgs vars
       ; vars' <- mapM tcTVar vars
       ; extendLclSTM vars' $
    do { res_ty' <- tcType res_ty
       ; rhs' <- tcRhs fun rhs res_ty'
       ; return (Def { def_fun = fun, def_args = vars'
                     , def_rhs = rhs', def_res_ty = res_ty' })
    }}

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
    extendLclSTM (paramsSizeBinders qvars) $
    do { qvars' <- mapM tcTVar qvars
        ; extendLclSTM qvars' $
    do { TE lhs' lhs_ty <- tcExpr lhs
       ; TE rhs' rhs_ty <- tcExpr rhs
       ; checkTypes lhs_ty rhs_ty $
         text "LHS and RHS of a rule have different types"
       ; return (Rule { ru_name = name, ru_qvars = qvars'
                      , ru_lhs = lhs', ru_rhs = rhs' })
    }}

tcArgs :: [TVarX p] -> TcM ()
tcArgs args = when (not distinct) $
                  addErr (text "Duplicated arguments:" <+> commaPpr duplicated)
    where argNames   = map tVarVar args
          duplicated = nub (argNames \\ nub argNames)
          distinct   = null duplicated
          commaPpr   = sep . punctuate comma . map ppr

tcTVar :: InPhase p => TVarX p -> TcM TVar
tcTVar (TVar ty v)
  = do { ty' <- tcType ty
       ; return (TVar ty' v) }

tcType :: InPhase p => TypeX p -> TcM Type
tcType (TypeVec size ty) = do { TE size' size_ty <- tcExpr size
                              ; checkTypes TypeInteger size_ty $
                                text "The size field of a Vec must have type Integer, but is" <+> ppr size_ty
                              ; ty' <- tcType ty
                              ; return (TypeVec size' ty') }
tcType (TypeTuple tys)   = TypeTuple <$> mapM tcType tys
tcType (TypeLM ty1 ty2)  = TypeLM <$> tcType ty1 <*> tcType ty2
tcType (TypeLam ty1 ty2) = TypeLam <$> tcType ty1 <*> tcType ty2
tcType TypeBool          = return TypeBool
tcType TypeInteger       = return TypeInteger
tcType TypeFloat         = return TypeFloat
tcType TypeString        = return TypeString
tcType TypeUnknown       = return TypeUnknown

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
                  mapM tcExpr es

       ; res_ty <- lookupGblTc fun pairs

       ; checkTypes_maybe mb_ty res_ty $
         text "Function call type mismatch for" <+> ppr fun
       ; let call' = Call (TFun res_ty fun) (map exprOf pairs)
       ; return (TE call' res_ty) }

tcExpr (Let vx rhs body)
  = do { TE arhs rhs_ty <- tcExpr rhs
       ; let (var, mb_ty) = getLetBndr @p vx
       ; checkTypes_maybe mb_ty rhs_ty $
         text "Let binding mis-match for" <+> ppr var
       ; let tvar = TVar rhs_ty var
       ; TE abody tybody <- extendLclSTM [tvar] (tcExpr body)

       ; checkFreeness tvar tybody $
         text "in the let binding for" <+> ppr tvar

       ; return (TE (Let tvar arhs abody) tybody) }

tcExpr (Tuple es)
  = do { pairs <- mapM tcExpr es
       ; let (aes, tys) = unzipTEs pairs
       ; return (TE (Tuple aes) (TypeTuple tys)) }

tcExpr (Lam tv@(TVar tyv _v) body)
  = do { tv' <- tcTVar tv
       ; TE abody tybody <- extendLclSTM [tv'] (tcExpr body)
       ; checkFreeness tv' tybody $
         text "in the lambda binding for" <+> ppr tyv
       ; return (TE (Lam tv' abody)
                    (TypeLam (typeof tv') tybody)) }

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

tcVar :: Var -> Maybe Type -> TcM Type
tcVar var mb_ty
  | isDummy var
  = case mb_ty of
      Just ty -> return ty
      Nothing -> do { addErr (text "Dummy var in untyped code")
                    ; return TypeUnknown }

  | otherwise
  = do { ty <- lookupLclTc var
       ; checkTypes_maybe mb_ty ty $
         text "Variable occurrence mis-match for" <+> ppr var
       ; return ty }

------------------------------------------------------------------------------
-- callResultTy_maybe is given a (global) function and the type of its
-- argument, and returns the type of its result.
--
-- It has special cases for a bunch opuilt-in functions with polymorphic
-- types; that is, where the result type is a function of the argument types
-- Otherwise it just looks in the global symbol table.
callResultTy_maybe :: SymTab -> Fun -> [TypedExpr]
                   -> Either SDoc Type
callResultTy_maybe env fun args
  | is_user_fun fun
  = userCallResultTy_maybe fun (gblST env) args
  | otherwise
  = primCallResultTy_maybe fun args
  where
    funId = \case
      Fun     f   -> f
      GradFun f _ -> f
      DrvFun  f _ -> f

    isUserFunConstructor = \case
      UserFun{} -> True
      PrimFun{} -> False
      SelFun{}  -> False

    is_user_fun = isUserFunConstructor . funId

-----------------------------------------------
--     The typecheck monad
-----------------------------------------------

userCallResultTy_maybe :: HasCallStack => Fun -> GblSymTab
                       -> [TypedExpr] -> Either SDoc Type
userCallResultTy_maybe fn env args
  = case lookupGblST fn env of
      Just def -> userCallResultTy_help def args
      Nothing  -> Left (text "Not in scope: userCall:" <+> ppr fn)

userCallResultTy_help :: HasCallStack
                      => TDef -> [TypedExpr] -> Either SDoc Type
userCallResultTy_help (Def { def_fun  = fn
                           , def_res_ty = ret_ty
                           , def_args = params })
                      args
  = case check_args 1 (map (substType size_subst) bndr_tys) arg_tys of
      Just err -> Left err
      Nothing  -> Right (substType size_subst ret_ty)
  where
    bndr_tys   = map tVarType params
    arg_tys    = map typeof args
    size_subst = bindTypeArgs params args

    check_args :: Int -> [Type] -> [Type] -> Maybe SDoc
    -- Return (Just err) if there's a wrong-ness
    check_args n (bndr_ty : bndr_tys) (arg_ty : arg_tys)
      | bndr_ty `compatibleType` arg_ty
      = check_args (n+1) bndr_tys arg_tys
      | otherwise
      = Just (hang (text "Type mis-match in argument" <+> int n
                     <+> text "of call to" <+> ppr fn)
                 2 (vcat [ text "Expected:" <+> ppr bndr_ty
                         , text "Actual:  " <+> ppr arg_ty ]))
    check_args _ [] [] = Nothing
    check_args _ [] _  = Just (text "Too many arguments")
    check_args _ _  [] = Just (text "Too few arguments")

bindTypeArgs :: [TVar]      -- Parameters
             -> [TypedExpr] -- Arguments of this call, each with its type
             -> Subst
-- Given function binders [ x :: Vec (2*n) Float
--                        , y :: (Float, Vec n Float) ]
--
--   and args of type 	  [ Vec e1 Float
--                    	  , (Float, Vec (3*j) Float ]
--
-- extract the substitution [n :-> 3*j]
-- Note that we get the substitution from the y-binding,
-- which binds a simple 'n', ignoring the earlier, but more
-- complex x-binding.
--
-- If matching fails, just return a smaller substitution
-- The caller is going to re-match anyway
bindTypeArgs bndrs args
  = match_args bndrs              args              $
    match_tys  (map typeof bndrs) (map typeof args) $
    empty_subst
  where
    empty_subst = mkEmptySubst []
        -- There are no binders in the types we
        -- are substituting into, so no need for
        -- a full in-scope set

    match_args :: [TVar] -> [TypedExpr] -> Subst -> Subst
    match_args (TVar bndr_ty v : bndrs) (TE arg _ : args) subst
      | TypeInteger <- bndr_ty
      = extendSubstMap v arg $
        match_args bndrs args subst
      | otherwise
      = match_args bndrs args subst
    match_args _ _ subst = subst

    match_tys :: [Type] -> [Type] -> Subst -> Subst
    match_tys (bndr_ty : bndr_tys) (arg_ty : arg_tys) subst
      = match_ty  bndr_ty  arg_ty $
        match_tys bndr_tys arg_tys subst
    match_tys _ _ subst = subst

    match_ty :: Type -> Type -> Subst ->  Subst
    match_ty (TypeVec (Var tv) bndr_ty) (TypeVec size arg_ty) subst
      = extendSubstMap (tVarVar tv) size $
        match_ty bndr_ty arg_ty subst
    match_ty (TypeTuple bndr_tys) (TypeTuple arg_tys) subst
      = match_tys bndr_tys arg_tys subst
    match_ty (TypeLM bndr_ty1 bndr_ty2) (TypeLM arg_ty1 arg_ty2) subst
      = match_ty bndr_ty1 arg_ty1 $
        match_ty bndr_ty2 arg_ty2 subst
    match_ty (TypeLam bndr_ty1 bndr_ty2) (TypeLam arg_ty1 arg_ty2) subst
      = match_ty bndr_ty1 arg_ty1 $
        match_ty bndr_ty2 arg_ty2 subst
    match_ty _ _ subst = subst

-----------------------------------------------
--     The typecheck monad
-----------------------------------------------

data TcEnv
  = TCE { tce_ctxt :: [SDoc]   -- Context, innermost first
        , tce_st   :: SymTab }

newtype TcM a = TCM { unTc :: TcEnv -> [SDoc] -> (Maybe a, [SDoc]) }
-- Just a writer monad on [SDoc]
-- Return Nothing if typechecking failure; it's an exception
-- In that case there should always be a message in the
-- returned [SDoc]

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
  = do { liftIO $ putStrLn $ render $
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

checkTypes_maybe :: Maybe Type -> Type -> SDoc -> TcM ()
checkTypes_maybe mb_ty1 ty2 herald
  = case mb_ty1 of
      Nothing  -> return ()
      Just ty1 -> checkTypes ty1 ty2 herald

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

checkFreeness :: TVar -> Type -> SDoc -> TcM ()
checkFreeness tv ty extra
  = unless (tv `notFreeInType` ty) $
    addErr $ vcat [ hang (text "Locally bound variable" <+> ppr tv)
                       2 (text "appears free in result type" <+> ppr ty)
                  , extra ]

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
              case Map.lookup (varFun v) (gblST st) of
                  Nothing -> do {
                             addErr (text "Not in scope: local var/tld:" <+> ppr v <+> gblDoc st)
                             ; return TypeUnknown
                             }
                  Just (Def { def_fun  = _fn
                           , def_res_ty = ret_ty
                           , def_args = params }) -> return (TypeLam (TypeTuple $ map tVarType params) ret_ty)
           }
           Just ty -> return ty }
  where
     varFun (Simple name) = mk_fun name
     varFun _ = error "varFun"

lookupGblTc :: Fun -> [TypedExpr] -> TcM Type
lookupGblTc fun args
  = do { st <- getSymTabTc
       ; case callResultTy_maybe st fun args of
           Left err -> do { addErr $ hang err 2 (mk_extra st)
                          ; return TypeUnknown }
           Right res_ty -> return res_ty }
  where
    mk_extra st
      = vcat [ text "In a call of:" <+> ppr fun <+> parens (text (show fun))
             , text " Arg types:" <+> ppr (map typeof args)
             , text " Args:"      <+> ppr (map exprOf args)
             , text "ST lookup:" <+> ppr (Map.lookup fun (gblST st))
             -- This is very verbose, and obscures error messages, but can be useful for typos.
             -- Perhaps think about printing it only for failed lookup of userfun
             -- , text "ST keys:" <+> gblDoc
             ]

gblDoc :: SymTab -> SDoc
gblDoc st = vcat (map (text . show) (Map.keys (gblST st)))
