{-# LANGUAGE LambdaCase #-}

module Annotate (
  annotDecls, lintDefs,

  GblSymTab, extendGblST, lookupGblST, emptyGblST,

  callResultTy

  ) where

import Lang
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
  = runTc "Type checking" init_env (go decls)
  where
    go [] = do { st <- getSymTabTc
               ; return (gblST st, []) }

    go (decl:decls)
      = do { tdecl <- tcDeclX decl
           ; (env', tdecls) <- extendGblEnv tdecl (go decls)
           ; return (env', tdecl:tdecls) }

    -- rec_gbl_env: extend the global env with the
    -- user-specified types for each top-level function
    -- TODO: this comment still current?
    -- We don't have a type-checked body yet, but that 
    -- doesn't matter because we aren't doing inlining
    rec_gbl_env = extendGblST gbl_env
                  [ DefX { def_fun = fun, def_args = args
                         , def_rhs = pprPanic "Rec body of" (ppr fun) }
                  | DefDecl (DefX { def_fun = fun, def_args = args }) <- decls ]
    init_env = TCE { tce_ctxt = []
                   , tce_st = newSymTab rec_gbl_env
                   , tce_fun = \fun -> (fun, Nothing)
                   , tce_var = \var -> (var, Nothing) }

-----------------------------------------------
--     A Lint function to do type checking
-----------------------------------------------

lintDefs :: String -> GblSymTab -> [TDef] -> KM ()
-- Returns a list of error messages
lintDefs what gbl_env defs
  = do { runTc what init_env (mapM_ tcDef defs)
       ; printK (text "Linted" <+> text what) }
  where
    init_env = TCE { tce_ctxt = []
                   , tce_st = newSymTab gbl_env
                   , tce_fun = \(TFun ty fun) -> (fun, Just ty)
                   , tce_var = \(TVar ty var) -> (var, Just ty) }


-----------------------------------------------
--     Guts of the type checker
-----------------------------------------------

tcDeclX :: (PrettyVar f, PrettyVar b, HasInfix f)
        => DeclX f b -> TcM f b TDecl
tcDeclX (DefDecl def)   = do { def' <- tcDef def
                             ; return (DefDecl def') }
tcDeclX (RuleDecl rule) = do { rule' <- tcRule rule
                             ; return (RuleDecl rule') }

tcDef :: (PrettyVar f, PrettyVar b, HasInfix f)
      => DefX f b -> TcM f b TDef
tcDef (DefX { def_fun = tf@(TFun fun_ty fun)
            , def_args = vars
            , def_rhs = expr })
  = extendLclEnv vars $
    do { (rhs_ty,e) <- addCtxt (text "In the definition of" <+> ppr fun) $
                       tcExpr expr
       ; checkTypes fun_ty rhs_ty $
         text "Function result type mis-match for" <+> ppr fun
       ; return (DefX tf vars e) }

tcRule :: (PrettyVar f, PrettyVar b, HasInfix f)
       => RuleX f b -> TcM f b TRule
tcRule (Rule { ru_name = name, ru_qvars = qvars
             , ru_lhs = lhs, ru_rhs = rhs })
  = addCtxt (text "In the rule with lhs:" <+> ppr lhs) $
    extendLclEnv qvars $
    do { (lhs_ty, lhs') <- tcExpr lhs
       ; (rhs_ty, rhs') <- tcExpr rhs
       ; checkTypes lhs_ty rhs_ty $
         text "LHS and RHS of a rule have different types"
       ; return (Rule { ru_name = name, ru_qvars = qvars
                      , ru_lhs = lhs', ru_rhs = rhs' }) }

tcExpr :: ExprX f b -> TcM f b (Type, TExpr)
  -- Naming conventions in this function:
  --  e   Expr [TypeUnknown]
  --  ae  Expr [Annotated]
  --  tye Type of e
  --  xe  ExprX part of e
tcExpr (Var v)
  = do { (var, mb_ty) <- getVarTc v
       ; ty <- tcVar var mb_ty
       ; return (ty, Var $ TVar ty var) }

tcExpr (Konst k)
  = return (typeofKonst k, Konst k)

tcExpr (Call f es)
  = do { (fun, mb_ty) <- getFunTc f
       ; pairs <- addCtxt (text "In the call of:" <+> ppr fun) $
                  mapM tcExpr es
       ; let (tyes, aes) = unzip pairs
       ; ty <- lookupGblTc fun tyes
       ; checkTypes_maybe mb_ty ty $
         text "Function call type mismatch for" <+> ppr fun
       ; return (ty, Call (TFun ty fun) aes) }

tcExpr (Let v rhs body)
  = do { (rhs_ty, arhs) <- tcExpr rhs
       ; (var, mb_ty)   <- getVarTc v
       ; checkTypes_maybe mb_ty rhs_ty $
         text "Let binding mis-match for" <+> ppr var
       ; let tvar = TVar rhs_ty var
       ; (tybody,abody) <- extendLclEnv [tvar] (tcExpr body)
       ; return (tybody, Let tvar arhs abody) }

tcExpr (Tuple es)
  = do { pairs <- mapM tcExpr es
       ; let (tys,aes) = unzip pairs
       ; return (TypeTuple tys, Tuple aes) }

tcExpr (Lam tv@(TVar tyv _v) body)
  = do { (tybody,abody) <- extendLclEnv [tv] (tcExpr body)
       ; return (TypeLambda tyv tybody, Lam tv abody) }

tcExpr (If cond texpr fexpr)
  = do { (tycond,acond)   <- tcExpr cond
       ; (tytexpr,atexpr) <- tcExpr texpr
       ; (tyfexpr,afexpr) <- tcExpr fexpr
       ; checkTypes TypeBool tycond $
         text "Predicate of 'if' has non-boolean type"
       ; checkTypes tytexpr tyfexpr $
         text "Branches of 'if' have different types"
       ; return (tytexpr, If acond atexpr afexpr) }

tcExpr (Assert cond body)
  = do { (tycond,acond) <- tcExpr cond
       ; (tybody,abody) <- tcExpr body
       ; checkTypes TypeBool tycond $
         text "Predicate of 'assert' has non-boolean type"
       ; return (tybody, Assert acond abody) }

tcExpr (App fun arg)
  = do { (fun_ty, afun) <- tcExpr fun
       ; (arg_ty, aarg) <- tcExpr arg
       ; res_ty <- case fun_ty of
           TypeLambda ty1 ty2
              -> do { checkTypes ty1 arg_ty $
                      text "Function application mis-match"
                    ; return ty2 }
           _ -> do { addErr (text "Function does not have function type" <+> ppr fun_ty)
                   ; return TypeUnknown }
       ; return (res_ty, App afun aarg) }

tcVar :: Var -> Maybe Type -> TcM f b Type
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

-----------------------------------------------
--     Symbol table, ST, maps variables to types
-----------------------------------------------

-- Global symbol table
type GblSymTab = Map.Map Fun TDef
   -- Maps a function to its definition, which lets us
   --   * Find its return type
   --   * Inline it
   -- Domain is UserFun, and perhaps the Grad of PrimFuns

-- Local symbol table
type LclSymTab = Map.Map Var Type
   -- The Type is the type of the variable

-- Entire symbol table
data SymTab = ST { gblST :: GblSymTab
                 , lclST :: LclSymTab }

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
   ppr m = braces $ fsep  $ punctuate comma $
           [ ppr k <+> text ":->" <+> ppr v | (k,v) <- Map.toList m ]

instance Pretty SymTab where
  ppr (ST { lclST = lcl_env, gblST = gbl_env })
    = vcat [ hang (text "Global symbol table:")
                2 (ppr gbl_env)
           , hang (text "Local symbol table:")
                2 (ppr lcl_env) ]

emptyGblST :: GblSymTab
emptyGblST = Map.empty

newSymTab :: GblSymTab -> SymTab
newSymTab gbl_env = ST { gblST = gbl_env, lclST = Map.empty }

stInsertFun :: Fun -> TDef -> GblSymTab -> GblSymTab
stInsertFun = Map.insert

lookupGblST :: HasCallStack => Fun -> GblSymTab -> Maybe TDef
lookupGblST = Map.lookup

extendGblST :: GblSymTab -> [TDef] -> GblSymTab
extendGblST = foldl add
  where
    add env def@(DefX (TFun _ f) _ _) = stInsertFun f def env

modifygblST :: (GblSymTab -> GblSymTab) -> SymTab -> SymTab
modifygblST g = \env -> env { gblST = g (gblST env) }

------------------------------------------------------------------------------
-- callResultTy is given a (global) function and the type of its
-- argument, and returns the type of its result.
--
-- It has special cases for a bunch of built-in functions with polymorphic
-- types; that is, where the result type is a function of the argument types
-- Otherwise it just looks in the global symbol table.
callResultTy :: HasCallStack => SymTab -> Fun -> [Type] -> Type
callResultTy env fun arg_ty
  = case callResultTy_maybe env fun arg_ty of
      Right res_ty -> res_ty
      Left msg     -> pprPanic "callResultTy" $
                      vcat [ ppr fun <+> ppr arg_ty
                           , msg
                           , text "Env =" <+> ppr env
                           , ppr (map (== fun) gbl_env_keys)
                           , ppr (map (> fun)  gbl_env_keys)
                           , ppr (map (< fun)  gbl_env_keys) ]
  where
    gbl_env_keys = map fst $ Map.toList $ gblST env

callResultTy_maybe :: SymTab -> Fun -> [Type]
                   -> Either SDoc Type
callResultTy_maybe env fun arg_ty
  | is_user_fun fun
  = userCallResultTy_maybe fun (gblST env) arg_ty
  | otherwise
  = primCallResultTy_maybe fun arg_ty
  where
    funId = \case
      Fun     f   -> f
      GradFun f _ -> f
      DrvFun  f _ -> f
      CheckFun f  -> f

    isUserFunConstructor = \case
      UserFun{} -> True
      PrimFun{} -> False
      SelFun{}  -> False

    is_user_fun = isUserFunConstructor . funId

userCallResultTy_maybe :: HasCallStack => Fun -> GblSymTab
                       -> [Type] -> Either SDoc Type
userCallResultTy_maybe f env arg_ty
  | Just def <- lookupGblST f env
  , DefX { def_fun = TFun ret_ty _, def_args = params } <- def
  = check_args ret_ty 1 params arg_ty
  | otherwise
  = Left (text "Not in scope:" <+> ppr f)

  where
    check_args ret_ty _ [] [] = Right ret_ty
    check_args ret_ty n (TVar param_ty _ : params) (arg_ty : arg_tys)
      | param_ty `eqType` arg_ty
      = check_args ret_ty (n+1) params arg_tys
      | otherwise
      = Left (hang (text "Type mis-match in argument" <+> int n)
                 2 (vcat [ text "Expected:" <+> ppr param_ty
                         , text "Actual:  " <+> ppr arg_ty ]))
    check_args _ _ [] _ = Left (text "Too many arguments")
    check_args _ _ _ [] = Left (text "Too few arguments")


-----------------------------------------------
--     The typecheck monad
-----------------------------------------------

data TcEnv f b
  = TCE { tce_ctxt :: [SDoc]   -- Context, innermost first
        , tce_st   :: SymTab
        , tce_fun  :: f -> (Fun, Maybe Type)
        , tce_var  :: b -> (Var, Maybe Type) }

newtype TcM f b a = TCM { unTc :: TcEnv f b -> [SDoc] -> (a, [SDoc]) }
-- Just a writer monad on [SDoc]

instance Functor (TcM f b) where
  fmap f (TCM m) = TCM (\ctxt ds -> case m ctxt ds of
                                     (r, ds') -> (f r, ds'))

instance Applicative (TcM f b) where
  pure  = return
  (<*>) = ap

instance Monad (TcM f b) where
  return v = TCM (\_ ds -> (v, ds))
  TCM m >>= k = TCM $ \ctxt ds ->
                case m ctxt ds of
                  (r1, ds') -> unTc (k r1) ctxt ds'

runTc :: String -> TcEnv f b -> TcM f b a -> KM a
runTc what init_env (TCM m)
  | null rev_errs
  = return result
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
    (result, rev_errs) = m init_env []

addErr :: SDoc -> TcM f b ()
addErr d = TCM (\env ds -> ((), mk_err env d : ds))
  where
    mk_err env d =  vcat (d:tce_ctxt env)

addCtxt :: SDoc -> TcM f b a -> TcM f b a
addCtxt cd (TCM m) = TCM $ \env@(TCE { tce_ctxt = cds }) ds ->
                     m (env { tce_ctxt = cd : cds }) ds

getFunTc :: f -> TcM f b (Fun, Maybe Type)
getFunTc f = TCM (\env ds -> (tce_fun env f, ds))

getVarTc :: b -> TcM f b (Var, Maybe Type)
getVarTc v = TCM (\env ds -> (tce_var env v, ds))

checkTypes_maybe :: Maybe Type -> Type -> SDoc -> TcM f b ()
checkTypes_maybe mb_ty1 ty2 herald
  = case mb_ty1 of
      Nothing  -> return ()
      Just ty1 -> checkTypes ty1 ty2 herald

checkTypes :: Type -> Type -> SDoc -> TcM f b ()
checkTypes exp_ty act_ty herald
  | exp_ty `eqType` act_ty
  = return ()
  | otherwise
  = addErr $ hang herald 2 $
    vcat [ text "Expected type:" <+> ppr exp_ty
         , text "Actual type:  " <+> ppr act_ty ]

extendLclEnv :: [TVar] -> TcM f b a -> TcM f b a
extendLclEnv vars = modifyEnvTc add_vars
  where
    add_vars st = st { lclST = foldl add (lclST st) vars }
    add :: LclSymTab -> TVar -> LclSymTab
    add env (TVar ty v) = Map.insert v ty env

extendGblEnv :: TDecl -> TcM f b a -> TcM f b a
extendGblEnv (RuleDecl {}) = id
extendGblEnv (DefDecl tdef@(DefX { def_fun = TFun _ f }))
  = modifyEnvTc (modifygblST (stInsertFun f tdef))

modifyEnvTc :: (SymTab -> SymTab) -> TcM f b a -> TcM f b a
modifyEnvTc extend (TCM f)
  = TCM (\env -> f (env { tce_st = extend (tce_st env) }))

getSymTabTc :: TcM f b SymTab
getSymTabTc = TCM (\env ds -> (tce_st env, ds))

lookupLclTc :: Var -> TcM f b Type
lookupLclTc v
  = do { st <- getSymTabTc
       ; case Map.lookup v (lclST st) of
           Nothing -> do { addErr (text "Not in scope: local var:" <+> ppr v)
                         ; return TypeUnknown }
           Just ty -> return ty }

lookupGblTc :: Fun -> [Type] -> TcM f b Type
lookupGblTc fun arg_ty
  = do { st <- getSymTabTc
       ; case callResultTy_maybe st fun arg_ty of
           Left err -> do { addErr $ hang err 2 (mk_extra st)
                          ; return TypeUnknown }
           Right res_ty -> return res_ty }
  where
    mk_extra st
      = vcat [ text "In a call of:" <+> ppr fun <+> parens (text (show fun))
             , text " Arg type:" <+> ppr arg_ty
             , text "ST lookup:" <+> ppr (Map.lookup fun (gblST st))
             -- , text "ST keys:" <+> vcat (map (text . show) (Map.keys (gblST st))) 
             ]

