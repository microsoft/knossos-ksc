-- Ksc.CatLang: Language for compilation to categories
module Ksc.CatLang( CLDef, toCLDefs, fromCLDefs, toCLDef_maybe, fromCLDef ) where

import Prelude hiding( (<>) )
import Lang
import LangUtils
import Prim
import qualified Data.Set as S
import Data.Maybe( mapMaybe )

data CLExpr
  = CLId
  | CLPrune [Int] Int CLExpr         -- ?
  | CLKonst Konst
  | CLCall Type FunId            -- The Type is the result type
  | CLComp CLExpr CLExpr         -- Composition
  | CLTuple [CLExpr]             -- Tuple
  | CLIf CLExpr CLExpr CLExpr    -- If
  | CLLet TVar CLExpr CLExpr     -- Let $var = $rhs in $body
  | CLBuild CLExpr TVar CLExpr   -- Build $size (Lam ($var : Integer) $body)
  | CLFold TVar CLExpr CLExpr CLExpr
  -- ^ Fold (Lam $t body) $acc $vector

data CLDef = CLDef { cldef_fun    :: FunId
                   , cldef_arg    :: Pat     -- Arg type S
                   , cldef_rhs    :: CLExpr
                   , cldef_res_ty :: Type }  -- Result type T

mkCLComp :: CLExpr -> CLExpr -> CLExpr
-- Optimise away the identity function to reduce clutter
mkCLComp CLId e = e
mkCLComp e CLId = e
mkCLComp e1 e2  = CLComp e1 e2

-----------------------------------------------
--  Pretty printing
-----------------------------------------------

instance Pretty CLDef where
  ppr (CLDef { cldef_fun = f
             , cldef_arg = arg
             , cldef_rhs = rhs
             , cldef_res_ty = res_ty })
     = sep [ hang (text "def" <+> ppr f <+> pprParendType res_ty)
                2 (parens (pprPat False arg))
           , nest 2 (text "=" <+> ppr rhs) ]

instance Pretty CLExpr where
  pprPrec = pprCLExpr

pprCLExpr :: Prec -> CLExpr -> SDoc
pprCLExpr p c@(CLComp {})
  = parensIf p precOne $
    sep [ pprCLExpr precTwo c1
        , vcat [ text "." <+> pprCLExpr precTwo c
               | c <- cs ] ]
  where
    (c1,cs) = case collect c of
                (c1:cs) -> (c1,cs)
                [] -> pprPanic "pprCLExpr" (text "")

    collect (CLComp a b) = collect a ++ collect b
    collect e            = [e]

pprCLExpr _ CLId           = text "Id"
pprCLExpr _ (CLKonst k)    = ppr k
pprCLExpr _ (CLCall _ f)   = ppr f
pprCLExpr p (CLPrune ts n c) = parensIf p precOne $
                               sep [ text "Prune" <> char '['
                                     <> cat (punctuate comma (map ppr ts))
                                     <> char '/' <> int n <> char ']'
                                   , nest 2 (pprCLExpr precOne c) ]
pprCLExpr _ (CLTuple cs)
  | [] <- cs  = text "[]"
  | [c] <- cs = char '[' <+> pprCLExpr precZero c <+> char ']'
  | (c1:rest) <- cs = cat ( [ char '[' <+> pprCLExpr precZero c1 ] ++
                            [ char ',' <+> pprCLExpr precZero c | c <- rest ] ) <+> char ']'

pprCLExpr p (CLIf b t e) = parensIf p precZero $
                           sep [ text "if" <+> ppr b
                               , text "then" <+> ppr t
                               , text "else" <+> ppr e]
pprCLExpr p (CLLet x r b)
  = parensIf p precZero $
    vcat [ text "let" <+> (bracesSp $ sep [ ppr x <+> char '=', nest 2 (pprCLExpr precZero r) ])
         , pprCLExpr precZero b ]

pprCLExpr p (CLBuild es x ee)
  = parensIf p precZero $
    text "build" <+> sep [ pprCLExpr precOne es
                         , parens $ char '\\' <> ppr x <> char '.' <+> pprCLExpr precZero ee ]

pprCLExpr p (CLFold t elam eacc ev)
  = parensIf p precZero $
    text "fold" <+> sep [ parens $ char '\\' <> ppr t <> char '.' <+> pprCLExpr precZero elam
                        , pprCLExpr precOne eacc
                        , pprCLExpr precOne ev ]

-----------------------------------------------
--  Convert to CLDedf
-----------------------------------------------

toCLDefs :: [TDef] -> [CLDef]
toCLDefs defs = mapMaybe toCLDef_maybe defs

toCLDef_maybe :: TDef -> Maybe CLDef
toCLDef_maybe  (Def { def_fun    = fun
                    , def_pat    = pat
                    , def_res_ty = res_ty
                    , def_rhs    = rhs })
  | Fun f     <- fun
  , UserRhs e <- rhs
  = Just CLDef { cldef_fun = f
               , cldef_arg = pat
               , cldef_rhs = toCLExpr (patVars pat) e
               , cldef_res_ty = res_ty }
  | otherwise
  = Nothing

data EnvPruned = Pruned | NotPruned

-- CatLang doesn't support tuple patterns in lets yet, but it could
-- and if we do anything serious with CatLang then it *should*.
toCLExpr :: [TVar] -> TExpr -> CLExpr
toCLExpr env = to_cl_expr NotPruned env . oneArgifyExpr (mkInScopeSet env)

to_cl_expr :: EnvPruned -> [TVar] -> TExpr -> CLExpr

to_cl_expr Pruned [w] (Var v) | v == w = CLId
to_cl_expr Pruned env (Var v)    = pprPanic "toCLExpr:var" (ppr v $$ ppr env)
to_cl_expr Pruned []  (Konst k)  = CLKonst k
to_cl_expr Pruned env (Konst k)  = pprPanic "toCLExpr:konst" (ppr k $$ ppr env)
to_cl_expr Pruned env (Tuple es) = CLTuple (map (toCLExpr env) es)
to_cl_expr pruned env (Call f e) = to_cl_call pruned env f e

-- We shouldn't do this because asserts can be essential to good
-- optimisation, but we will do it for now, to make progress with
-- CatLang
to_cl_expr pruned env (Assert _ e) = to_cl_expr pruned env e

to_cl_expr Pruned env (If e1 e2 e3)
  = CLIf (toCLExpr env e1) (toCLExpr env e2) (toCLExpr env e3)
to_cl_expr Pruned env (Let (VarPat tv) rhs body)
  = CLLet tv (toCLExpr env rhs) (toCLExpr (tv:env) body)

to_cl_expr Pruned _ e@(Let (TupPat _) _ _) = pprPanic "toCLExpr Let TupPat" (ppr e)

to_cl_expr _ _ e@(Lam {})    =  pprPanic "toCLExpr Lam" (ppr e)
to_cl_expr _ _ e@(App {})    =  pprPanic "toCLExpr App" (ppr e)
to_cl_expr _ _ e@(Dummy {})  =  pprPanic "toCLExpr Dummy" (ppr e)

to_cl_expr NotPruned env e = prune env e

---------------
to_cl_call :: EnvPruned -> [TVar] -> TFun -> TExpr -> CLExpr
-- Calls:
--   * for build, prune before the build
--   * for other calls, prune in the argument

to_cl_call pruned env f e
  | f `isThePrimFun` "build"
  , Tuple [n, Lam tvi body] <- e
  = case pruned of
      NotPruned -> prune env call
      Pruned    -> CLBuild (toCLExpr env n) tvi (toCLExpr (tvi:env) body)

  | f `isThePrimFun` "sumbuild"
  , Tuple [n, lam] <- e
  = to_cl_expr pruned env (pSum (pBuild n lam))

  | f `isThePrimFun` "fold"
  , Tuple [Lam t body, acc, v] <- e
  = case pruned of
      NotPruned -> prune env call
      Pruned    ->
        CLFold t (toCLExpr (t:env) body) (toCLExpr env acc) (toCLExpr env v)

  | TFun ty (Fun fun_id) <- f
  = CLCall ty fun_id `mkCLComp` to_cl_expr pruned env e

  | TFun _ (GradFun _ _) <- f
  = pprPanic "toCLExpr Call of GradFun" (ppr call)
  | TFun _ (DrvFun _ _) <- f
  = pprPanic "toCLExpr Call of DrvFun" (ppr call)
  | TFun _ (ShapeFun _) <- f
  = pprPanic "toCLExpr Call of ShapeFun" (ppr call)
  where
    call = Call f e

---------------
prune :: [TVar] -> TExpr -> CLExpr
-- See Note [Pruning]
prune env e
  | no_prune_reqd = to_cl_expr Pruned env e
  | otherwise     = CLPrune trim_indices (length env)
                            (to_cl_expr Pruned trimmed_env e)
  where
    fvs = freeVarsOf e
    no_prune_reqd = all (\tv -> tv `S.member` fvs) env

    trimmed_prs :: [(TVar,Int)]
    trimmed_prs = trim fvs 1 env

    (trimmed_env, trim_indices) = unzip trimmed_prs

    trim :: S.Set TVar -> Int -> [TVar] -> [(TVar,Int)]
    trim _ _ [] = []
    trim fvs n (tv:tvs)
      | tv `S.member` fvs = (tv,n) : trim (tv `S.delete` fvs) (n+1) tvs
      | otherwise         = trim fvs (n+1) tvs

{- Note [Pruning]
~~~~~~~~~~~~~~~~~
We need to be careful when we have shadowing.  Then
we might have a call
   to_cl_expr [x,v,x,w] e
where the first 'x' in the environment shadows the second.
So when pruning we must be careful to pick the first element of
the tuple, not the third, let alone both!
-}

-----------------------------------------------
--  Convert from CLDef
-----------------------------------------------

fromCLDefs :: [CLDef] -> [TDef]
fromCLDefs cldefs = map fromCLDef cldefs

fromCLDef :: CLDef -> TDef
fromCLDef (CLDef { cldef_fun = f
                 , cldef_arg = pat
                 , cldef_rhs = rhs
                 , cldef_res_ty = res_ty })
  = Def { def_fun    = Fun f
        , def_pat    = pat
        , def_res_ty = res_ty
        , def_rhs    = UserRhs rhs' }
  where
    pvs = patVars pat
    rhs' = fromCLExpr (mkInScopeSet pvs) (map Var pvs) rhs

fromCLExpr :: InScopeSet -> [TExpr] -> CLExpr -> TExpr
-- (fromCLExpr is arg c)
-- We may freely duplicate 'arg', so make sure that all
-- of the TExprs in 'arg' are trivial -- usually variables --
-- and hence can be duplicated without duplicating work
fromCLExpr _  _   (CLKonst k)      = Konst k
fromCLExpr _  arg CLId             = mkTuple arg
fromCLExpr is arg (CLPrune ts _ c) = fromCLExpr is (pick ts arg) c
fromCLExpr is arg (CLTuple es)     = Tuple (map (fromCLExpr is arg) es)
fromCLExpr is arg (CLIf b t e)     = If (fromCLExpr is arg b)
                                        (fromCLExpr is arg t)
                                        (fromCLExpr is arg e)

fromCLExpr _  arg (CLCall ty f)
  = Call (TFun ty (Fun f)) (mkTuple arg)

fromCLExpr is arg (CLComp e1 e2)
  | CLCall ty f <- e1   -- Shortcut to avoid an unnecessary let
  = Call (TFun ty (Fun f)) (fromCLExpr is arg e2)
  | otherwise
  = mkTempLet is "ax" (fromCLExpr is arg e2) $ \ is v2 ->
    fromCLExpr is [v2] e1

fromCLExpr is arg (CLLet tv rhs body)
  = mkLet tv' rhs' (fromCLExpr is' (Var tv' : arg) body)
  where
    rhs'      = fromCLExpr is arg rhs
    (is', tv') = notInScopeTV is tv

fromCLExpr is arg (CLBuild size tv elt)
  = pBuild (fromCLExpr is arg size)
           (Lam tv' (fromCLExpr is' (Var tv' : arg) elt))
  where
    (is', tv') = notInScopeTV is tv

fromCLExpr is arg (CLFold t lam acc v)
  = mkPrimCall3 "fold"
          (Lam t' (fromCLExpr is' (Var t' : arg) lam))
          (fromCLExpr is arg acc)
          (fromCLExpr is arg v)
  where
    (is', t') = notInScopeTV is t

-----------------------------------------------
-- Utilities
-----------------------------------------------

pick :: Pretty a => [Int] -> [a] -> [a]
-- Pick the specifed items from the list
pick ts es = [ get t | t <- ts ]
  where
    get t | t > length es = pprTrace "pick" (ppr ts $$ ppr es) (head es)
          | otherwise     = es !! (t-1)

mkTempLet :: InScopeSet -> String -> TExpr
          -> (InScopeSet -> TExpr -> TExpr) -> TExpr
mkTempLet is s e thing_inside
  | isTrivial e = thing_inside is e
  | otherwise   = mkLet tv' e $
                  thing_inside is' (Var tv')
  where
    (is', tv') = notInScopeTV is tv
    tv = TVar (typeof e) (Simple s)
