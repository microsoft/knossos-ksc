{-# LANGUAGE MonoLocalBinds #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- Ksc.CatLang: Language for compilation to categories
module Ksc.CatLang( CLDef, toCLDefs, fromCLDefs, toCLDef_maybe, fromCLDef
                  , fwdAdDefs, revAdDefs, fsAdDefs) where

import Prelude hiding( (<>) )
import Lang
import LangUtils
import Prim
import Annotate( callResultTy )
import qualified Data.Set as S
import Data.Maybe( mapMaybe )
import Data.List( mapAccumL )

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

clResultType :: CLExpr -> [Type] -> Type
clResultType CLId             tys = mkTupleTy tys
clResultType (CLPrune ts _ c) tys = clResultType c (pick ts tys)
clResultType (CLKonst k)      _   = typeofKonst k
clResultType (CLComp c1 c2)   tys = clResultType c1 [clResultType c2 tys]
clResultType (CLTuple cs)     tys = mkTupleTy [clResultType c tys | c <- cs]
clResultType (CLLet tv _ b)   tys = clResultType b (typeof tv : tys)
clResultType (CLBuild _ _ b)  tys = TypeVec (clResultType b (TypeInteger : tys))
clResultType (CLCall ty _)    _   = ty
clResultType (CLIf _ c _)     tys = clResultType c tys
clResultType (CLFold t l _ _) tys = clResultType l (typeof t : tys)

foldUnimplemented :: String -> error
foldUnimplemented s = error (s ++ ": CLFold not yet implemented")

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

toCLExpr :: [TVar] -> TExpr -> CLExpr
toCLExpr = to_cl_expr NotPruned

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
to_cl_expr Pruned env (Let tv rhs body)
  = CLLet tv (toCLExpr env rhs) (toCLExpr (tv:env) body)

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

find :: Eq a => a -> [a] -> Maybe Int
-- Return 1-indexed position of the item in the list
find x xs
  = go 1 xs
  where
    go j (x1:xs) | x == x1   = Just j
                 | otherwise = go (j+1) xs
    go _ [] = Nothing


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
-- and hence can be duplicated without dupliating work
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
  = Let tv' rhs' (fromCLExpr is' (Var tv' : arg) body)
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

-------------------------------------------
--   Forward AD, tupled
--   S => T  =   (S, dS) -> (T, dT)
-------------------------------------------


fwdAdDefs :: GblSymTab -> [CLDef] -> (GblSymTab, [TDef])
fwdAdDefs gst cldefs
  = let defs = map fwdAdDef cldefs
    in (extendGblST gst defs, defs)

fwdAdDef :: CLDef -> TDef
fwdAdDef (CLDef { cldef_fun = f
                , cldef_arg = VarPat (TVar ty v)
                , cldef_rhs = rhs
                , cldef_res_ty = res_ty })
  = Def { def_fun    = DrvFun f (AD { adPlan = TupleAD, adDir = Fwd })
        , def_pat    = VarPat arg'
        , def_res_ty = tangentPair res_ty
        , def_rhs    = UserRhs rhs' }
  where
    arg' = TVar (tangentPair ty) v
    rhs' = mkPairLet (mkInScopeSet [arg']) "x" "dx" (Var arg') $ \ is x dx ->
           fwdAdExpr is [x] [dx] rhs

fwdAdExpr :: InScopeSet -> [TExpr] -> [TExpr] -> CLExpr -> TExpr
fwdAdExpr _  _ _  (CLKonst k)  = Tuple [ Konst k, mkTangentZero (Konst k) ]
fwdAdExpr is s ds (CLIf a b c) = If (fromCLExpr is s a)
                                    (fwdAdExpr is s ds b)
                                    (fwdAdExpr is s ds c)

--  (.) :: (b => c) -> (a => b) -> (a => c)
--  (f . g) <ft> s = f <ft> (g <ft> s)
fwdAdExpr is s ds (CLComp f g)
  = mkPairLet is "gr" "dgr" (fwdAdExpr is s ds g) $ \is a da ->
    fwdAdExpr is [a] [da] f

fwdAdExpr _ s ds CLId
  = Tuple [mkTuple s, mkTuple ds]

fwdAdExpr is s ds (CLPrune ts _ c)
  = fwdAdExpr is (pick ts s) (pick ts ds) c

fwdAdExpr _ s ds (CLCall ty f)
  = Call (TFun (tangentPair ty) fwd_fun)
         (Tuple [mkTuple s, mkTuple ds])
  where
    fwd_fun = DrvFun f ad_mode
    ad_mode = AD { adPlan = TupleAD, adDir = Fwd }

--  (,) :: (s => t1) -> (s => t2) -> (s => (t1,t2))
--  (c1,c2) <ft> (s,ds) = let (ar,dar) = c1 <ft> (s,ds)
--                            (br,dbr) = c2 <ft> (s,ds)
--                        in ((ar,br),(dar,dbr))
fwdAdExpr is s ds (CLTuple cs)
  = mkTempLets is "t" es $ \vs ->
    Tuple [ mkTuple (map pFst vs), mkTuple (map pSnd vs) ]
  where
    es = map (fwdAdExpr is s ds) cs

--  (let x = c in b) <ft> (s,ds) = let (x,dx) = c <ft> (s,ds)
--                                 in b <ft> (x:s, dx:ds)
fwdAdExpr is s ds (CLLet tv rhs body)
  = mkPairLet is nm ("d" ++ nm) (fwdAdExpr is s ds rhs) $ \ is x dx ->
    fwdAdExpr is (x:s) (dx:ds) body
  where
    nm = tVarName tv

--  (build s i e) <> (s,ds) = let (n,_) = c <f> (s,ds)
--                            in unzip (build n (\i. e <f> (i:s, ():ds))
fwdAdExpr is s ds (CLBuild n tvi elt)
  = mkTempLet is "np" (fwdAdExpr is s ds n) $ \ is np ->
    let (is', tvi') = notInScopeTV is tvi
    in pUnzip $ pBuild (pFst np) $
       Lam tvi' (fwdAdExpr is' (Var tvi' : s) (unitExpr:ds) elt)

fwdAdExpr _ _ _ CLFold{} = foldUnimplemented "fwdAdExpr"


tangentPair :: Type -> Type
tangentPair ty = TypeTuple [ty, tangentType ty]


-------------------------------------------
--     Reverse AD, not tupled
--     S => T  =   (S, dT) -> dS
-------------------------------------------

revAdDefs :: GblSymTab -> [CLDef] -> (GblSymTab, [TDef])
revAdDefs gst cldefs
  = let defs = map revAdDef cldefs
    in (extendGblST gst defs, defs)

revAdDef :: CLDef -> TDef
revAdDef (CLDef { cldef_fun = f
                , cldef_arg = VarPat (TVar arg_ty v)
                , cldef_rhs = rhs
                , cldef_res_ty = res_ty })
  = Def { def_fun    = DrvFun f (AD { adPlan = BasicAD, adDir = Rev })
        , def_pat    = VarPat arg'
        , def_res_ty = tangentType arg_ty
        , def_rhs    = UserRhs rhs' }
  where
    arg' = TVar (TypeTuple [arg_ty, tangentType res_ty]) (Simple "arg")
    nm   = varName v
    rhs' = mkPairLet (mkInScopeSet [arg']) nm "dr" (Var arg') $ \ is x dr ->
           revAdExpr is [x] dr rhs

revAdExpr :: InScopeSet -> [TExpr] -> TExpr -> CLExpr -> TExpr
revAdExpr _  s _  (CLKonst _)  = mkTangentZero (mkTuple s)
revAdExpr is s dt (CLIf a b c) = If (fromCLExpr is s a)
                                    (revAdExpr is s dt b)
                                    (revAdExpr is s dt c)

--  (.) :: (b => c) -> (a => b) -> (a => c)
--  (f . g) <r> (a,dc) = let b = g <> a
--                           db = f <r> (b,dc)
--                       in
--                       g <r> (a,db)
revAdExpr is s dt (CLComp f g)
  = mkTempLet is "b"  (fromCLExpr is s g)     $ \ is b ->
    mkTempLet is "db" (revAdExpr is [b] dt f) $ \ is db ->
    revAdExpr is s db g

revAdExpr _ _ dt CLId
  = dt

revAdExpr is s dt (CLPrune ts _ c)
  = mkTempLet is "dps" (revAdExpr is (pick ts s) dt c) $ \ _ dps ->
    let do_one (s,i)
          | Just j <- find i ts = pSel j n dps
          | otherwise           = mkTangentZero s
    in mkTuple (map do_one (s `zip` [1..]))
  where
    n = length ts

revAdExpr _ s dt (CLCall _ f)
  = Call (TFun res_ty rev_fun) (Tuple [mkTuple s, dt])
  where
    res_ty  = tangentType (typeof (mkTuple s))
    rev_fun = DrvFun f ad_mode
    ad_mode = AD { adPlan = BasicAD, adDir = Rev }

--  (,) :: (s => t1) -> (s => t2) -> (s => (t1,t2))
--  (c1,c2) <r> (s,dt) = let (dt1,dt2) = dt
--                           ds1 = c1 <r> (s,dt1)
--                           ds2 = c2 <r> (s,dt2)
--                     in (ds1 + ds2)
revAdExpr is s dt (CLTuple cs)
  = foldr1 pAdd $
    [ revAdExpr is s (pSel i n dt) c
    | (c,i) <- cs `zip` [1..] ]
  where
    n = length cs

--  (build sz (\i.e) <r> (s,dt)
--    = sumbuild (sz <> s) (\i. (e <r> (i:s, dt[i])))
revAdExpr is s dt (CLBuild sz tvi elt)
  = pSumBuild (fromCLExpr is s sz) $
      Lam tvi' $ pTupTail $
      revAdExpr is' (Var tvi' : s)
                (pIndex (Var tvi') dt) elt
  where
    (is', tvi') = notInScopeTV is tvi

--  (let x = c in b) <r> (s,dt) = let x         = c <> s
--                                    (dxb:dsb) = b <r> (x:s, dt)
--                                    dsx       = c <r> (s,dxb)
--                                in
--                                dsb + dsx
revAdExpr is s dt (CLLet tv rhs body)
  = mkTempLet is (tVarName tv) (fromCLExpr is s rhs) $ \is x ->
    mkTempLet is "dx" (revAdExpr is (x : s) dt body) $ \ is dx ->
    pAdd (pTupTail dx) (revAdExpr is s (pTupHead dx) rhs)

revAdExpr _ _ _ CLFold{} = foldUnimplemented "revAdExpr"

--------------------------------------------
--   Spilt AD
--   fwds:  S => T  =   S -> (T, X)
--   revs:  S => T  =   (dT,X) -> dS
--------------------------------------------

type FwdSplit = InScopeSet -> [TExpr] -> TExpr
type RevSplit = InScopeSet -> TExpr -> TExpr -> TExpr

data SplitResult = SR { sr_fwd  :: FwdSplit
                      , sr_rev  :: RevSplit
                      , sr_empx :: XShape }
-- For an expression of type S => T
-- if XShape = XNonEmpty, then
--     fwds:  S -> (T, X)
--     revs:  (dT,X) -> dS
-- if XShape = XEmpty, then
--     fwds:  S -> X
--     revs:  dT -> dS
--
-- The *top-level* fwds$f is always S -> (T, X)
-- even if X = (), so that we can tell what X is
-- But the top-level revs$g is dT -> dS if X = ()

data XShape = XEmpty | XNonEmpty

instance Pretty XShape where
  ppr XEmpty    = text "XEmpty"
  ppr XNonEmpty = text "XEmpty"

andXS :: XShape -> XShape -> XShape
andXS XEmpty XEmpty = XEmpty
andXS _      _      = XNonEmpty

isNonEmpty :: XShape -> Bool
isNonEmpty XNonEmpty = True
isNonEmpty XEmpty    = False

-------------------
fsAdDefs :: GblSymTab -> [CLDef] -> (GblSymTab, [TDef])
fsAdDefs gst cldefs = (gst', concat defs)
  where
    (gst', defs) = mapAccumL fsAdDef gst cldefs

-------------------
fsAdDef :: GblSymTab -> CLDef -> (GblSymTab, [TDef])
fsAdDef gst (CLDef { cldef_fun = f
                   , cldef_arg = VarPat arg
                   , cldef_rhs = rhs
                   , cldef_res_ty = res_ty })
  = ( extendGblST gst [fwd_def, rev_def]
    , [fwd_def, rev_def])
  where
    arg_ty = typeof arg

    SR { sr_fwd = fwd, sr_rev = rev, sr_empx = emp } = fsAdExpr gst [arg_ty] rhs

    fwd_def = Def { def_fun    = DrvFun f (AD { adPlan = SplitAD, adDir = Fwd })
                  , def_pat    = VarPat arg
                  , def_res_ty = fwd_rhs_ty
                  , def_rhs    = UserRhs final_fwd_rhs }
    fwd_in_scope   = mkInScopeSet [arg]
    fwd_rhs        = fwd fwd_in_scope [Var arg]
    final_fwd_rhs = case emp of
                       XEmpty    -> Tuple [fwd_rhs, unitExpr]
                       XNonEmpty -> fwd_rhs

    fwd_rhs_ty = typeof final_fwd_rhs
    x_ty = case fwd_rhs_ty of
             TypeTuple [_, x_ty] -> x_ty
             _ -> pprPanic "fsAdDef" (ppr fwd_rhs_ty)

    rev_def = Def { def_fun    = DrvFun f (AD { adPlan = SplitAD, adDir = Rev })
                  , def_pat    = VarPat rev_arg
                  , def_res_ty = tangentType (typeof arg)
                  , def_rhs    = UserRhs rev_rhs }

    (rev_arg, rev_rhs)
       = case emp of
           XNonEmpty -> (dtx, mkPairLet is_dtx "dt" "x" (Var dtx) $ \is dt x ->
                              rev is dt x)
           XEmpty    -> (dt,  rev is_dt (Var dt) unitExpr)

    dtx = TVar (TypeTuple [tangentType res_ty, x_ty]) (Simple "dtx")
    dt  = TVar (tangentType res_ty)                   (Simple "dt")
    is_dtx = mkInScopeSet [dtx]
    is_dt  = mkInScopeSet [dt]

-------------------
fsAdExpr :: GblSymTab -> [Type] -> CLExpr -> SplitResult
-- This is the main workhorse
-- The [Type] is the type of the argument tuple at that point

fsAdExpr _ _ (CLKonst k)
  = SR { sr_fwd  = \ _ _   -> Konst k
       , sr_rev  = \ _ _ _ -> mkTangentZero (Konst k)
       , sr_empx = XEmpty }

fsAdExpr _ _ CLId
  = SR { sr_fwd  = \ _ s    -> mkTuple s
       , sr_rev  = \ _ dt _ -> dt
       , sr_empx = XEmpty }

-- prune ts c <sf> s  =  let (rc, xc) = c <sf> (pick is s)
--                       in (rc, (xc,zeros))
-- prune ts c <sr> (dt,x)  =  let (xc,zeros) = x
--                                ds = c <sr> (dt,xc)
--                            in spread ts ds zeros

fsAdExpr gst tys (CLPrune ts _ c)
  = SR{ sr_fwd  = fwd, sr_rev  = rev, sr_empx = c_emp `andXS` zero_emp }
  where
    fwd is s = mkXPairLet is "rc" "xc" (c_emp, c_fwd is (pick ts s)) $ \_ rc xc ->
               mkXFwdPair rc [ (c_emp, xc), (zero_emp, xzero) ]
      where
        xzero = mkTuple [  (s!!(i-1)) | i <- zero_ts ]

    rev is dt x = mkTempLet is "ds" (c_rev is dt xc) $ \_ ds ->
                  mkTuple (map (do_one ds xzero) (tys `zip` [1..]))
      where
        (xc:xzero:_) = splitXTuple x [c_emp, zero_emp]

    m     = length ts
    z     = length zero_ts
    t_prs = ts      `zip` [1..]
    z_prs = zero_ts `zip` [1..]

    do_one ds xzero (ty, i_n)
      | Just j_m <- findIndex t_prs i_n = pSel j_m m ds
      | Just k_m <- findIndex z_prs i_n = pSel k_m z xzero
      | otherwise                       = mkTangentZeroFromType ty


    SR { sr_fwd = c_fwd, sr_rev = c_rev, sr_empx = c_emp }
       = fsAdExpr gst (pick ts tys) c

    zero_emp | null zero_ts = XEmpty
             | otherwise    = XNonEmpty

    zero_ts :: [Int]  -- Positions in [1..n] for which I need a zero
    zero_ts = [ i | (ty,i) <- tys `zip` [1..]
                  , not (i `elem` ts)
                  , needValueForTangentZero ty ]

{-
fsAdExpr gst is s (CLIf a b c) = If (fromCLExpr is s a)
                                    (fsAdExpr gst is s b)
                                    (fsAdExpr gst is s c)
-}
fsAdExpr _ _ e@(CLIf {}) = pprPanic "fsAdExpr:If" (ppr e)

--  (.) :: (b => c) -> (a => b) -> (a => c)
--  (f . g) <sf> s = let (rg, xg) = g <sf> s
--                       (rf, xf) = f <sf> rg
--                   in (rf, (xf,xg))
--  (f . g) <sr> (dt,x) = let (xf,xg) = x
--                            dtf = f <sr> (dt,xf)
--                            dtg = g <sr> (dtf,xg)
--                        in dtg
fsAdExpr gst tys (CLComp f g)
  = SR { sr_fwd = \ is s ->
                  mkXPairLet is "rg" "xg" (g_empx, fwd_g is s)    $ \is rg xg ->
                  mkXPairLet is "rf" "xf" (f_empx, fwd_f is [rg]) $ \_  rf xf ->
                  mkXFwdPair rf [(f_empx,xf), (g_empx, xg)]
       , sr_rev = \ is dt x ->
                  let (xf:xg:_) = splitXTuple x [f_empx, g_empx] in
                  mkTempLet is "dtf" (rev_f is dt xf) $ \is dtf ->
                  rev_g is dtf xg
       , sr_empx = empx }
  where
    empx = f_empx `andXS` g_empx
    SR { sr_fwd = fwd_f, sr_rev = rev_f, sr_empx = f_empx } = fsAdExpr gst [g_res_ty] f
    SR { sr_fwd = fwd_g, sr_rev = rev_g, sr_empx = g_empx } = fsAdExpr gst tys        g
    g_res_ty = clResultType g tys

fsAdExpr _ [ty] (CLCall _ (SelFun i n))
  = SR{ sr_fwd  = fwd, sr_rev  = rev, sr_empx = zero_emp }
  where
    fwd _ [s] = mkXFwdPair (pSel i n s) [ (zero_emp, xzero) ]
      where
        xzero = mkTuple [ mkTangentZero (pSel i n s) | i <- zero_ts ]
    fwd _ s = pprPanic "fsAdExpr:SelFun" (ppr s)

    rev _ dt x = mkTuple (map (do_one dt x) (tys `zip` [1..]))
    tys = case ty of
            TypeTuple tys -> tys
            _ -> pprPanic "fsAdExpr:SelFuny" (ppr ty)

    z     = length zero_ts
    z_prs = zero_ts `zip` [1..]

    do_one dt xzero (ty, i_n)
      | i_n == i                        = dt
      | Just k_m <- findIndex z_prs i_n = pSel k_m z xzero
      | otherwise                       = mkTangentZeroFromType ty

    zero_emp | null zero_ts = XEmpty
             | otherwise    = XNonEmpty

    zero_ts :: [Int]  -- Positions in [1..n] for which I need a zero
    zero_ts = [ j | (ty,j) <- tys `zip` [1..]
                  , j /= i
                  , needValueForTangentZero ty ]

fsAdExpr gst tys (CLCall _ f)
  = SR { sr_fwd = fwd, sr_rev = rev, sr_empx = emp }
  where
    fs_fun = DrvFun f (AD { adPlan = SplitAD, adDir = Fwd })
    fr_fun = DrvFun f (AD { adPlan = SplitAD, adDir = Rev })

    fwd _ [s] = case emp of
                 XEmpty    -> pFst (mk_call fs_fun s)
                 XNonEmpty -> mk_call fs_fun s
    fwd _ s = pprPanic "fsAdExpr:CLCall" (ppr s)

    rev _ dt x = mk_call fr_fun $
                 case emp of { XEmpty -> dt; XNonEmpty -> Tuple [dt,x] }

    emp = funXShape gst f (mkTupleTy tys)

    mk_call fun arg = Call (TFun res_ty fun) arg
      where
        res_ty = callResultTy gst fun (typeof arg)

--  (,) :: (s => t1) -> (s => t2) -> (s => (t1,t2))
--  (c1,c2) <sf> s = let (ar,x1) = c1 <sf> s
--                       (br,x2) = c2 <sf> s
--                   in ((ar,br),(x1,x2))
--  (c1,c2) <sr> (dt,x) = let (x1,  x2)  = x
--                            (dt1, dt2) = dt
--                            dr1 = c1 <sr> (dt1, x1)
--                            dr2 = c2 <sr> (dt2, x2)
--                        in dt1 + dt2
fsAdExpr gst tys (CLTuple cs)
  = SR { sr_fwd = fwd, sr_rev = rev, sr_empx = all_emp }
  where
    srs     = map (fsAdExpr gst tys) cs
    emps    = map sr_empx srs
    all_emp = foldr andXS XEmpty emps

    fwd is s = mkXPairLets is "r" "x" [ (sr_empx sr, sr_fwd sr is s)
                                      | sr <- srs] $ \_ ts xs  ->
               mkXFwdPair (mkTuple ts) (emps `zip` xs)

    rev is dt x
      = foldr1 pAdd [ sr_rev sr is dt x
                    | (sr,(dt,x)) <- srs `zip` (dts `zip` xs) ]
      where
       -- We should probably use lets for these
       dts = Ksc.CatLang.splitTuple dt (length cs)
       xs  = splitXTuple x  emps


--  (let v = c in b) <sf> s = let (rc,xc) = c <sf> s
--                            let (rb,xb) = b <sf> (rc:s)
--                            in (rb, (xc,xb))
--  (let v = c in b) <sr> (dt,x) = let (xc,xb)  = x
--                                     rb = b <sr> (dt,xb)
--                                     (dc:dr1) = rb
--                                     dr2      = c <sr> (dc,xc)
--                                 in dr1 + dr2
fsAdExpr gst tys (CLLet tv rhs body)
  = SR { sr_fwd = fwd, sr_rev = rev, sr_empx = rhs_emp `andXS` body_emp }
  where
    fwd is s = mkXPairLet is "rc" "xc" (rhs_emp,  fwd_rhs is s)       $ \ is rc xc ->
               mkXPairLet is "rb" "xb" (body_emp, fwd_body is (rc:s)) $ \ _ rb xb ->
               mkXFwdPair rb [(rhs_emp, xc), (body_emp, xb)]
    rev is dt x
      = mkTempLet is "rb" (rev_body is dt xbody) $ \ is rb ->
        pAdd (pTupTail rb) (rev_rhs is (pSel 1 (tupLen rb) rb) xrhs)
      where
       (xrhs:xbody:_) = splitXTuple x [rhs_emp, body_emp]

    SR { sr_fwd = fwd_rhs,  sr_rev = rev_rhs,  sr_empx = rhs_emp  }
      = fsAdExpr gst tys rhs
    SR { sr_fwd = fwd_body, sr_rev = rev_body, sr_empx = body_emp }
      = fsAdExpr gst (typeof tv:tys) body

--  (build n i e) <sf>  s     = unzip (build n (\i. e <sf> (i:s)))
--  (build n i e) <sr> (dt,x) = sumbuild n (\i. e <sr> (dt[i], x[i]))
fsAdExpr gst tys (CLBuild sz tvi elt)
  = SR { sr_fwd = fwd, sr_rev = rev, sr_empx = elt_emp }
  where
    fwd is s = let (is', tvi') = notInScopeTV is tvi in
               pUnzip $ pBuild (fromCLExpr is s sz) $
               Lam tvi' $ fwd_elt is' (Var tvi' : s)
    rev is dt x = let (is', tvi') = notInScopeTV is tvi in
                  pSumBuild (pSize dt) $ Lam tvi' $
                  rev_elt is' (pIndex (Var tvi') dt) (pIndex (Var tvi') x)

    SR { sr_fwd = fwd_elt, sr_rev = rev_elt, sr_empx = elt_emp }
      = fsAdExpr gst (TypeInteger:tys) elt

fsAdExpr _ _ CLFold{} = foldUnimplemented "fsAdEXpr CLFold not yet implemented"


funXShape :: GblSymTab -> FunId -> Type -> XShape
funXShape gst fn arg_ty
  = case lookupGblST (fs_fn, arg_ty) gst of
      Just (Def { def_res_ty = res_ty })
        | TypeTuple [_, x] <- res_ty
        -> case x of
             TypeTuple [] -> XEmpty
             _            -> XNonEmpty
      _ -> pprPanic "hasEmptyX" (ppr fn)
           -- Not in GST, or bad shape
  where
    fs_fn = DrvFun fn (AD { adPlan = SplitAD, adDir = Fwd })

-----------------------------------------------
-- Utilities
-----------------------------------------------

pick :: Pretty a => [Int] -> [a] -> [a]
-- Pick the specifed items from the list
pick ts es = [ get t | t <- ts ]
  where
    get t | t > length es = pprTrace "pick" (ppr ts $$ ppr es) (head es)
          | otherwise     = es !! (t-1)

findIndex :: Eq a => [(a,b)] -> a -> Maybe b
findIndex prs i
  = go prs
  where
    go [] = Nothing
    go ((a,b):prs) | a ==i     = Just b
                   | otherwise = go prs

tupLen :: TExpr -> Int
tupLen e = case typeof e of
             TypeTuple tys -> length tys
             x -> pprTrace "fsAdExpr:CLLet" (ppr x $$ ppr e) 0

mkTempLets :: InScopeSet -> String -> [TExpr]
           -> ([TExpr] -> TExpr) -> TExpr
mkTempLets _ _ [] thing_inside
  = thing_inside []
mkTempLets is s (e:es) thing_inside
  = mkTempLet is   s e      $ \ is' ve ->
    mkTempLets is' s es  $ \ ves ->
    thing_inside (ve : ves)

mkPairLet :: InScopeSet -> String -> String -> TExpr
          -> (InScopeSet -> TExpr -> TExpr -> TExpr)
          -> TExpr
mkPairLet is sf ss e thing_inside
  = mkTempLet is "t" e        $ \is ve ->
    mkTempLet is sf (pFst ve) $ \is vf ->
    mkTempLet is ss (pSnd ve) $ \is vs ->
    thing_inside is vf vs

mkTempLet :: InScopeSet -> String -> TExpr
          -> (InScopeSet -> TExpr -> TExpr) -> TExpr
mkTempLet is s e thing_inside
  | isTrivial e = thing_inside is e
  | otherwise   = Let tv' e $
                  thing_inside is' (Var tv')
  where
    (is', tv') = notInScopeTV is tv
    tv = TVar (typeof e) (Simple s)

mkXFwdPair :: TExpr -> [(XShape,TExpr)] -> TExpr
mkXFwdPair r prs
  | null non_emp_xs = r   -- Ignore xs
  | otherwise       = Tuple [r, mkTuple non_emp_xs ]
  where
    non_emp_xs = [ x | (XNonEmpty,x) <- prs ]

mkXPairLet :: InScopeSet -> String -> String -> (XShape, TExpr)
           -> (InScopeSet -> TExpr -> TExpr -> TExpr)
           -> TExpr
mkXPairLet is sf ss (xshape, e) thing_inside
  = case xshape of
     XNonEmpty -> mkPairLet is sf ss e thing_inside
     XEmpty    -> mkTempLet is sf e $ \is ve ->
                  thing_inside is ve unitExpr

mkXPairLets :: InScopeSet -> String -> String -> [(XShape,TExpr)]
           -> (InScopeSet -> [TExpr] -> [TExpr] -> TExpr)
           -> TExpr
mkXPairLets is sf ss prs thing_inside
  = go is prs thing_inside
  where
    go is []       thing_inside = thing_inside is [] []
    go is (pr:prs) thing_inside = mkXPairLet is sf ss pr $ \is t x ->
                                  go is prs              $ \is ts xs ->
                                  thing_inside is (t:ts) (x:xs)

splitXTuple :: TExpr -> [XShape] -> [TExpr]
-- splitXTuple x [XEmpty,    XEmpty]    = [(), ()]
-- splitXTuple x [XNonEmpty, XEmpty]    = [x, ()]
-- splitXTuple x [XNonEmpty, XNonEmpty] = [fst x, snd x]
-- NB: the all-empty case does not mention x at all
splitXTuple x bs
  | nval == 1 = [ case b of { XEmpty -> unitExpr; XNonEmpty -> x }
                | b <- bs ]
  | otherwise = snd $ mapAccumL do_one 1 bs
  where
    nval = count isNonEmpty bs

    do_one :: Int -> XShape -> (Int, TExpr)
    do_one i XNonEmpty = (i+1, pSel i nval x)
    do_one i XEmpty    = (i,   unitExpr)

splitTuple :: TExpr -> Int -> [TExpr]
splitTuple _ 0 = []
splitTuple e 1 = [e]
splitTuple e n = [ pSel i n e | i <- [1..n] ]
