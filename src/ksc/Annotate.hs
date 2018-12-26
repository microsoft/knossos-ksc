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
import Data.List( mapAccumL )
import GHC.Stack
import Data.Foldable( foldlM )
import Control.Monad( ap )
import Debug.Trace( trace )
import Text.PrettyPrint as PP
import Data.List( intersperse )

dbtrace :: String -> a -> a
dbtrace _ e = e -- trace msg e

--------------------------

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

    init_env = TCE { tce_ctxt = []
                   , tce_st = newSymTab gbl_env
                   , tce_fun = \fun -> (fun, Nothing)
                   , tce_var = \var -> (var, Nothing) }

-----------------------------------------------
--     A Lint function to do type checking
-----------------------------------------------

lintDefs :: String -> GblSymTab -> [TDef] -> KM ()
-- Retuns a list of error messages
lintDefs what gbl_env defs
  = runTc what init_env (mapM_ tcDef defs)
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
tcDef (DefX { def_fun = f, def_args = vars, def_rhs = expr })
  = extendLclEnv vars $
    do { (fun, mb_ty) <- getFunTc f
       ; (ty,e) <- addCtxt (text "In the definition of" <+> ppr fun) $
                   tcExpr expr
       ; checkTypes_maybe mb_ty ty $
         text "Function result type mis-match for" <+> ppr fun
       ; return (DefX (TFun ty fun) vars e) }

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
       ; (tyes, aes) <- addCtxt (text "In the call of:" <+> ppr fun) $
                        tcExpr es
       ; ty <- lookupGblTc fun tyes
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

tcExpr (Lam tv@(TVar tyv v) body)
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


 --------------------------------------

stripAnnots :: [TDef] -> [Def]
stripAnnots = map stripAnnot

stripAnnot :: TDef -> Def
stripAnnot (DefX (TFun ty f) tvars texpr) =
  DefX f tvars (stripAnnotExpr texpr)

stripAnnotExpr :: TExpr -> Expr
stripAnnotExpr = \case
  Konst k -> Konst k
  Var (TVar _ v) -> Var v
  Call (TFun _ f) e -> Call f $ stripAnnotExpr e
  Tuple es -> Tuple $ map stripAnnotExpr es
  Lam tv e -> Lam tv $ stripAnnotExpr e
  App e1 e2 -> App (stripAnnotExpr e1) (stripAnnotExpr e2)
  Let (TVar _ v) e1 e2 -> Let v (stripAnnotExpr e1) (stripAnnotExpr e2)
  If c t f -> If (stripAnnotExpr c) (stripAnnotExpr t) (stripAnnotExpr f)
  Assert c e -> Assert (stripAnnotExpr c) (stripAnnotExpr e)


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

sttrace :: String -> a -> a
sttrace _ e = e -- trace msg e

emptyGblST :: GblSymTab
emptyGblST = Map.empty

newSymTab :: GblSymTab -> SymTab
newSymTab gbl_env = ST { gblST = gbl_env, lclST = Map.empty }

stInsertVar :: Var -> Type -> SymTab -> SymTab
stInsertVar v ty env
  = env { lclST = Map.insert v ty (lclST env) }

stInsertFun :: Fun -> TDef -> GblSymTab -> GblSymTab
stInsertFun f ty env = Map.insert f ty env

lookupGblST :: HasCallStack => Fun -> GblSymTab -> Maybe TDef
lookupGblST f env = Map.lookup f env

userCallResultTy_maybe :: HasCallStack => Fun -> GblSymTab -> Maybe Type
userCallResultTy_maybe f env
  | Just (DefX (TFun ret_ty _) _ _) <- lookupGblST f env
  = Just ret_ty
  | otherwise
  = Nothing

extendGblST :: GblSymTab -> [TDef] -> GblSymTab
extendGblST env defs = foldl add env defs
  where
    add env def@(DefX (TFun _ f) _ _) = stInsertFun f def env

------------------------------------------------------------------------------
-- callResultTy is given a (global) function and the type of its
-- argument, and returns the type of its result.
--
-- It has special cases for a bunch of built-in functions with polymorphic
-- types; that is, where the result type is a function of the argument types
-- Otherwise it just looks in the global symbol table.
callResultTy :: HasCallStack => SymTab -> Fun -> Type -> Type
callResultTy env fun arg_ty
  = case callResultTy_maybe env fun arg_ty of
      Just res_ty -> res_ty
      Nothing     -> pprPanic "callResultTy" $
                     vcat [ (ppr fun <+> ppr arg_ty)
                          , text "Env =" <+> ppr env
                          , ppr (map (== fun) gbl_env_keys)
                          , ppr (map (> fun)  gbl_env_keys)
                          , ppr (map (< fun)  gbl_env_keys) ]
  where
    gbl_env_keys = map fst $ Map.toList $ gblST env

callResultTy_maybe :: SymTab -> Fun -> Type -> Maybe Type
callResultTy_maybe env fun arg_ty
  | is_user_fun fun
  = userCallResultTy_maybe fun (gblST env)
  | otherwise
  = primCallResultTy_maybe fun arg_ty
  where
    is_user_fun (Fun     (UserFun{}))    = True
    is_user_fun (GradFun (UserFun {}) _) = True
    is_user_fun (DrvFun  (UserFun {}) _) = True
    is_user_fun _                        = False



-----------------------------------------------
--     The typecheck monad
-----------------------------------------------

data TcEnv f b
  = TCE { tce_ctxt :: [Doc]   -- Context, innermost first
        , tce_st   :: SymTab
        , tce_fun  :: f -> (Fun, Maybe Type)
        , tce_var  :: b -> (Var, Maybe Type) }

newtype TcM f b a = TCM { unTc :: TcEnv f b -> [Doc] -> (a, [Doc]) }
-- Just a writer monad on [Doc]

instance Functor (TcM f b) where
  fmap f (TCM m) = TCM (\ctxt ds -> case m ctxt ds of
                                     (r, ds') -> (f r, ds'))

instance Applicative (TcM f b) where
  pure  = return
  (<*>) = ap

instance Monad (TcM f b) where
  return v = TCM (\ctxt ds -> (v, ds))
  TCM m >>= k = TCM $ \ctxt ds ->
                case m ctxt ds of
                  (r1, ds') -> unTc (k r1) ctxt ds'

runTc :: String -> TcEnv f b -> TcM f b a -> KM a
runTc what init_env (TCM m)
  | null rev_errs
  = return result

  | otherwise
  = do { liftIO $ putStrLn $ PP.render $
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

addErr :: Doc -> TcM f b ()
addErr d = TCM (\env ds -> ((), mk_err env d : ds))
  where
    mk_err env d = d $$ vcat (tce_ctxt env)

addCtxt :: Doc -> TcM f b a -> TcM f b a
addCtxt cd (TCM m) = TCM $ \env@(TCE { tce_ctxt = cds }) ds ->
                     m (env { tce_ctxt = cd : cds }) ds

getFunTc :: f -> TcM f b (Fun, Maybe Type)
getFunTc f = TCM (\env ds -> (tce_fun env f, ds))

getVarTc :: b -> TcM f b (Var, Maybe Type)
getVarTc v = TCM (\env ds -> (tce_var env v, ds))

checkTypes_maybe :: Maybe Type -> Type -> Doc -> TcM f b ()
checkTypes_maybe mb_ty1 ty2 herald
  = case mb_ty1 of
      Nothing  -> return ()
      Just ty1 -> checkTypes ty1 ty2 herald

checkTypes :: Type -> Type -> Doc -> TcM f b ()
checkTypes exp_ty act_ty herald
  | promoteZero exp_ty == promoteZero act_ty
  = return ()
  | otherwise
  = addErr $ hang herald 2 $
    vcat [ text "Expected type:" <+> ppr exp_ty
         , text "Actual type:  " <+> ppr act_ty ]
  where promoteZero = \case
                      TypeZero t -> t
                      TypeTuple ts -> TypeTuple $ map promoteZero ts
                      TypeVec t -> TypeVec $ promoteZero t
                      TypeLambda from t -> TypeLambda from $ promoteZero t
                      t -> t

extendLclEnv :: [TVar] -> TcM f b a -> TcM f b a
extendLclEnv vars = modifyEnvTc add_vars
  where
    add_vars st = st { lclST = foldl add (lclST st) vars }
    add :: LclSymTab -> TVar -> LclSymTab
    add env (TVar ty v) = Map.insert v ty env

extendGblEnv :: TDecl -> TcM f b a -> TcM f b a
extendGblEnv (RuleDecl {}) thing_inside
  = thing_inside
extendGblEnv (DefDecl tdef@(DefX { def_fun = TFun _ f })) thing_inside
  = modifyEnvTc (\env -> env { gblST = stInsertFun f tdef (gblST env) })
                thing_inside

modifyEnvTc :: (SymTab -> SymTab) -> TcM f b a -> TcM f b a
modifyEnvTc extend (TCM f)
  = TCM (\env ds -> f (env { tce_st = extend (tce_st env) }) ds)

getSymTabTc :: TcM f b SymTab
getSymTabTc = TCM (\env ds -> (tce_st env, ds))

lookupLclTc :: Var -> TcM f b Type
lookupLclTc v
  = do { st <- getSymTabTc
       ; case Map.lookup v (lclST st) of
           Nothing -> do { addErr (text "Not in scope: local var:" <+> ppr v)
                         ; return TypeUnknown }
           Just ty -> return ty }

lookupGblTc :: Fun -> Type -> TcM f b Type
lookupGblTc fun arg_ty
  = do { st <- getSymTabTc
       ; case callResultTy_maybe st fun arg_ty of
           Nothing -> do { addErr $ hang (text "Out of scope or ill-typed function:" <+> ppr fun <+> text (show fun))
                                       2 (vcat [ text " Arg type:" <+> ppr arg_ty
                                               , text "ST lookup:" <+> ppr (Map.lookup fun (gblST st))
                                               , text "ST keys:" <+> vcat (map (text . show) (Map.keys (gblST st))) ])
                         ; return TypeUnknown }
           Just res_ty -> return res_ty }
