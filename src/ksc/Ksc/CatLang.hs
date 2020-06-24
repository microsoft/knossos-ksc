{-# LANGUAGE MonoLocalBinds #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- Ksc.CatLang: Language for compilation to categories
module Ksc.CatLang( CLDef, toCLDefs, fromCLDefs, toCLDef_maybe, fromCLDef
                    , fwdAdDefs
                    , revAdDefs
                    , fsAdDefs
     ) where

import Prelude hiding( (<>) )
import Lang
import LangUtils
import Prim
import Annotate( callResultTy_maybe, callResultTy )
import qualified Data.Set as S
import Data.Maybe( mapMaybe )
import Data.List( mapAccumL )
import GHC.Stack( HasCallStack )

data CLExpr
  = CLId
  | CLPrune [Int] Int
  | CLKonst Konst
  | CLCall Type FunId          -- The Type is the result type
  | CLComp CLExpr CLExpr       -- Reverse composition, c1;c2
  | CLTuple [CLExpr]           -- Tuple
  | CLIf CLExpr CLExpr CLExpr  -- If
  | CLLet TVar CLExpr CLExpr   -- Let $var = $rhs in $body
  | CLBuild CLExpr TVar CLExpr -- Build $size (Lam ($var : Integer) $body)
  | CLFold TVar CLExpr CLExpr CLExpr
  -- ^ Fold (Lam $t body) $acc $vector

data CLDef = CLDef { cldef_fun    :: FunId
                   , cldef_pat    :: Pat     -- Arg type S
                   , cldef_rhs    :: CLExpr
                   , cldef_res_ty :: Type }  -- Result type T

mkCLComp :: CLExpr -> CLExpr -> CLExpr
-- Optimise away the identity function to reduce clutter
mkCLComp CLId e = e
mkCLComp e CLId = e
mkCLComp e1 e2  = CLComp e1 e2

gsResult :: GammaShape -> CLExpr -> GammaShape
gsResult gs CLId             = gs
gsResult gs (CLPrune ts _)   = pick ts gs
gsResult _  (CLKonst k)      = [typeofKonst k]
gsResult gs (CLComp c1 c2)   = gsResult (gsResult gs c1) c2
gsResult gs (CLTuple cs)     = [mkTupleTy (gsResult gs c) | c <- cs]
gsResult gs (CLLet tv _ b)   = gsResult (typeof tv : gs) b
gsResult gs (CLBuild _ _ b)  = [TypeVec (mkTupleTy (gsResult (TypeInteger : gs) b))]
gsResult _  (CLCall ty _)    = [ty]
gsResult gs (CLIf _ c _)     = gsResult gs c
gsResult gs (CLFold t l _ _) = gsResult (typeof t : gs) l

foldUnimplemented :: String -> error
foldUnimplemented s = error (s ++ ": CLFold not yet implemented")

-----------------------------------------------
--  Pretty printing
-----------------------------------------------

instance Pretty CLDef where
  ppr (CLDef { cldef_fun = f
             , cldef_pat = arg
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
        , vcat [ text ";" <+> pprCLExpr precTwo c
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
pprCLExpr _ (CLPrune ts n) = sep [ text "Prune" <> char '['
                                   <> cat (punctuate comma (map ppr ts))
                                   <> char '/' <> int n <> char ']' ]
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
               , cldef_pat = pat
               , cldef_rhs = toCLExpr (patVars pat) e
               , cldef_res_ty = res_ty }
  | otherwise
  = Nothing

type Gamma = [TVar]   -- The environment

data EnvPruned = Pruned | NotPruned

toCLExpr :: Gamma -> TExpr -> CLExpr
toCLExpr = to_cl_expr NotPruned

to_cl_expr :: EnvPruned -> Gamma -> TExpr -> CLExpr

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
to_cl_call :: EnvPruned -> Gamma -> TFun -> TExpr -> CLExpr
-- Calls:
--   * for build, prune before the build
--   * for other calls, prune in the argument

to_cl_call pruned env f e
  | f `isThePrimFun` "build"
  , Tuple [n, Lam tvi body] <- e
  = case pruned of
      NotPruned -> prune env call
      Pruned    -> CLBuild (toCLExpr env n) tvi
                           (toCLExpr (tvi:env) body)

  | f `isThePrimFun` "sumbuild"
  , Tuple [n, lam] <- e
  = to_cl_expr pruned env (pSum (pBuild n lam))

  | f `isThePrimFun` "fold"
  , Tuple [Lam t body, acc, v] <- e
  = case pruned of
      NotPruned -> prune env call
      Pruned    -> CLFold t (toCLExpr (t:env) body)
                            (toCLExpr env acc) (toCLExpr env v)

  | TFun _ (Fun (PrimFun (Sel i n))) <- f
  = to_cl_expr pruned env e `mkCLComp` CLPrune [i] n

  | TFun ty (Fun fun_id) <- f
  = to_cl_expr pruned env e `mkCLComp` CLCall ty fun_id

  | TFun _ (GradFun _ _) <- f
  = pprPanic "toCLExpr Call of GradFun" (ppr call)
  | TFun _ (DrvFun _ _) <- f
  = pprPanic "toCLExpr Call of DrvFun" (ppr call)
  where
    call = Call f e

---------------
prune :: Gamma -> TExpr -> CLExpr
-- See Note [Pruning]
prune env e
  | no_prune_reqd = to_cl_expr Pruned env e
  | otherwise     = CLPrune trim_indices (length env)
                    `mkCLComp` to_cl_expr Pruned trimmed_env e
  where
    fvs = freeVarsOf e
    no_prune_reqd = all (\tv -> tv `S.member` fvs) env

    trimmed_prs :: [(TVar,Int)]
    trimmed_prs = trim fvs 1 env

    (trimmed_env, trim_indices) = unzip trimmed_prs

    trim :: S.Set TVar -> Int -> Gamma -> [(TVar,Int)]
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
                 , cldef_pat = pat
                 , cldef_rhs = rhs
                 , cldef_res_ty = res_ty })
  = Def { def_fun    = Fun f
        , def_pat    = pat
        , def_res_ty = res_ty
        , def_rhs    = UserRhs rhs' }
  where
    pvs = patVars pat
    pexp = mkTuple (map Var pvs)
    is   = mkInScopeSet pvs
    gs   = map typeof pvs
    rhs' = fromCLExpr is gs pexp rhs

type GammaShape = [Type]

fromCLExpr :: InScopeSet
           -> GammaShape  -- gs
           -> TExpr       -- Argument tuple of type (mkTupleTy gs)
           -> CLExpr -> TExpr
-- (fromCLExpr is tys arg c)
-- We may freely duplicate 'arg', so make sure that all
-- of the TExprs in 'arg' are trivial -- usually variables --
-- and hence can be duplicated without dupliating work
fromCLExpr _  _  _   (CLKonst k)    = Konst k
fromCLExpr _  _  arg CLId           = arg
fromCLExpr _  _  arg (CLCall ty f)  = Call (TFun ty (Fun f)) arg
fromCLExpr _  _  arg (CLPrune ts n) = mkTuple (pick ts (splitTuple arg n))
fromCLExpr is gs arg (CLTuple es)    = makeArgDupable is arg $ \is arg ->
                                       Tuple (map (fromCLExpr is gs arg) es)
fromCLExpr is gs arg (CLIf b t e)    = makeArgDupable is arg $ \ is arg ->
                                       If (fromCLExpr is gs arg b)
                                          (fromCLExpr is gs arg t)
                                          (fromCLExpr is gs arg e)

fromCLExpr is gs arg (CLComp e1 e2)
  = fromCLExpr is (gsResult gs e1) (fromCLExpr is gs arg e1) e2

fromCLExpr is gs arg (CLLet tv rhs body)
  = let (is', gs', tv', arg') = clExtendTV is gs tv arg
    in Let tv' (fromCLExpr is gs arg rhs)
               (fromCLExpr is' gs' arg' body)

fromCLExpr is gs arg (CLBuild size tv elt)
  = makeArgDupable is arg $ \is arg ->
    let (is', gs', tv', arg') = clExtendTV is gs tv arg
    in pBuild (fromCLExpr is gs arg size)
              (Lam tv' (fromCLExpr is' gs' arg' elt))

fromCLExpr is gs arg (CLFold tv lam acc v)
  = makeArgDupable is arg $ \is arg ->
    let (is', gs', tv', arg') = clExtendTV is gs tv arg
    in mkPrimCall3 "fold"
          (Lam tv' (fromCLExpr is' gs' arg' lam))
          (fromCLExpr is gs arg acc)
          (fromCLExpr is gs arg v)

clExtendTV :: InScopeSet -> GammaShape -> TVar -> TExpr
           -> (InScopeSet, GammaShape, TVar, TExpr)
clExtendTV is gs tv arg
  = (is', typeof tv : gs, tv', arg')
  where
    (is', tv') = notInScopeTV is tv
    arg' = clExtend gs (Var tv') arg

clExtend :: GammaShape -> TExpr -> TExpr -> TExpr
clExtend []  arg  _    = arg
clExtend [_] arg1 arg2 = Tuple [ arg1, arg2 ]
clExtend _   arg args  = pTupCons arg args

clSplit :: GammaShape -> TExpr
        -> (TExpr, TExpr)  -- Head and tail of tuple
clSplit []  arg = pprPanic "clSplit" (ppr arg)
clSplit [_] arg = (arg, Tuple [])
clSplit _   arg = (pTupHead arg, pTupTail arg)

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
                , cldef_pat = pat
                , cldef_rhs = rhs
                , cldef_res_ty = res_ty })
  = Def { def_fun    = DrvFun f (AD { adPlan = TupleAD, adDir = Fwd })
        , def_pat    = TupPat [s, ds]
        , def_res_ty = tangentPair res_ty
        , def_rhs    = UserRhs rhs' }
  where
    arg_ty = typeof pat
    s    = mkTVar arg_ty               "s"
    ds   = mkTVar (tangentType arg_ty) "ds"
    is   = mkInScopeSet [s,ds]
    pvs  = patVars pat
    gs   = map typeof pvs
    rhs' = fwdAdExpr is gs (Var s) (Var ds) rhs

fwdAdExpr :: InScopeSet -> GammaShape -> TExpr -> TExpr -> CLExpr -> TExpr
fwdAdExpr _  _  _ _  (CLKonst k)  = Tuple [ Konst k, mkTangentZero (Konst k) ]
fwdAdExpr _  _  s ds CLId         = Tuple [s, ds]
fwdAdExpr is gs s ds (CLIf a b c) = If (fromCLExpr is gs s a)
                                       (fwdAdExpr is gs s ds b)
                                       (fwdAdExpr is gs s ds c)

--  (.) :: (b => c) -> (a => b) -> (a => c)
--  (f . g) <ft> s = f <ft> (g <ft> s)
fwdAdExpr is gs s ds (CLComp g f)
  = mkPairLet is "gr" "dgr" (fwdAdExpr is gs s ds g) $ \is a da ->
    fwdAdExpr is (gsResult gs g) a da f

fwdAdExpr _ _ s ds (CLPrune ts n)
  = mkTuple [ mkTuple (pick ts (splitTuple s n))
            , mkTuple (pick ts (splitTuple ds n)) ]

fwdAdExpr _ _ s ds (CLCall ty f)
  = Call (TFun (tangentPair ty) fwd_fun)
         (Tuple [s, ds])
  where
    fwd_fun = DrvFun f ad_mode
    ad_mode = AD { adPlan = TupleAD, adDir = Fwd }

--  (,) :: (s => t1) -> (s => t2) -> (s => (t1,t2))
--  (c1,c2) <ft> (s,ds) = let (ar,dar) = c1 <ft> (s,ds)
--                            (br,dbr) = c2 <ft> (s,ds)
--                        in ((ar,br),(dar,dbr))
fwdAdExpr is gs s ds (CLTuple cs)
  = mkTempLets is "t" es $ \vs ->
    Tuple [ mkTuple (map pFst vs), mkTuple (map pSnd vs) ]
  where
    es = map (fwdAdExpr is gs s ds) cs

--  (let x = c in b) <ft> (s,ds) = let (x,dx) = c <ft> (s,ds)
--                                 in b <ft> (x:s, dx:ds)
fwdAdExpr is gs s ds (CLLet tv rhs body)
  = mkPairLet is nm ("d" ++ nm) (fwdAdExpr is gs s ds rhs) $ \ is x dx ->
    let s'  = clExtend gs x  s
        ds' = clExtend gs dx ds
        gs' = typeof tv : gs
    in fwdAdExpr is gs' s' ds' body
  where
    nm = tVarName tv

--  (build s i e) <> (s,ds) = let (n,_) = c <f> (s,ds)
--                            in unzip (build n (\i. e <f> (i:s, ():ds))
fwdAdExpr is gs s ds (CLBuild n tvi elt)
  = mkTempLet is "np" (fwdAdExpr is gs s ds n) $ \ is np ->
    let (is', gs', tvi', s') = clExtendTV is gs tvi s
        ds' = clExtend gs unitExpr ds
    in pUnzip $ pBuild (pFst np) $
       Lam tvi' (fwdAdExpr is' gs' s' ds' elt)

fwdAdExpr _ _ _ _ CLFold{} = foldUnimplemented "fwdAdExpr"


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
                , cldef_pat = pat
                , cldef_rhs = rhs
                , cldef_res_ty = res_ty })
  = Def { def_fun    = DrvFun f (AD { adPlan = BasicAD, adDir = Rev })
        , def_pat    = TupPat [s_arg, dt]
        , def_res_ty = tangentType s_ty
        , def_rhs    = UserRhs rhs' }
  where
    s_ty  = typeof pat
    dt_ty = tangentType res_ty

    pvs       = patVars pat
    gs        = map typeof pvs
    is1       = mkInScopeSet pvs
    (is2, s)  = notInScopeTV is1 (mkTVar s_ty  "s")
    (is3, dt) = notInScopeTV is2 (mkTVar dt_ty "dt")

    (s_arg, prs) = case pat of
                     VarPat x  -> (x, [])
                     TupPat xs -> (s, mkSelPrs xs s)

    rhs'   = mkLets prs $
             revAdExpr is3 gs (mkTuple (map Var pvs)) (Var dt) rhs


mkSelPrs :: [TVar] -> TVar -> [(TVar,TExpr)]
mkSelPrs xs s = [(x, pSel i n sexpr) | (x,i) <- xs `zip` [1..]]
  where
    n     = length xs
    sexpr = Var s

revAdExpr :: InScopeSet -> GammaShape -> TExpr -> TExpr -> CLExpr -> TExpr
revAdExpr _  _  s _  (CLKonst _)  = mkTangentZero s
revAdExpr _  _  _ dt CLId         = dt
revAdExpr is gs s dt (CLIf a b c) = If (fromCLExpr is gs s a)
                                       (revAdExpr is gs s dt b)
                                       (revAdExpr is gs s dt c)

--  (.) :: (b => c) -> (a => b) -> (a => c)
--  (f . g) <r> (a,dc) = let b = g <> a
--                           db = f <r> (b,dc)
--                       in
--                       g <r> (a,db)
-- Note the duplication of g
revAdExpr is gs s dt (CLComp g f)
  = mkTempLet is "b"  (fromCLExpr is gs s g)     $ \ is b ->
    mkTempLet is "db" (revAdExpr is gs_f b dt f) $ \ is db ->
    revAdExpr is gs s db g
  where
    gs_f = gsResult gs g

revAdExpr _ _ s dt (CLPrune ts n_s)
  = let do_one (s,i)
          | Just j <- find i ts = pSel j n_t dt
          | otherwise           = mkTangentZero s
    in mkTuple (map do_one (splitTuple s n_s `zip` [1..]))
  where
    n_t = length ts

revAdExpr _ _ s dt (CLCall _ f)
  = Call (TFun res_ty rev_fun) (Tuple [s, dt])
  where
    res_ty  = tangentType (typeof s)
    rev_fun = DrvFun f ad_mode
    ad_mode = AD { adPlan = BasicAD, adDir = Rev }

--  (,) :: (s => t1) -> (s => t2) -> (s => (t1,t2))
--  (c1,c2) <r> (s,dt) = let (dt1,dt2) = dt
--                           ds1 = c1 <r> (s,dt1)
--                           ds2 = c2 <r> (s,dt2)
--                     in (ds1 + ds2)
revAdExpr is gs s dt (CLTuple cs)
  = foldr1 pTsAdd $
    [ revAdExpr is gs s (pSel i n dt) c
    | (c,i) <- cs `zip` [1..] ]
  where
    n = length cs

--  (build sz (\i.e) <r> (s,dt)
--    = sumbuild (sz <> s) (\i. tail (e <r> (i:s, dt[i])))
revAdExpr is gs s dt (CLBuild sz tvi elt)
  = pSumBuild (fromCLExpr is gs s sz) $
    Lam tvi' $ snd $ clSplit gs' $
    revAdExpr is' gs' s' dt' elt
  where
    (is', gs', tvi', s') = clExtendTV is gs tvi s
    dt' = pIndex (Var tvi') dt

--  (let x = c in b) <r> (s,dt) = let x         = c <> s
--                                    (dxb:dsb) = b <r> (x:s, dt)
--                                    dsx       = c <r> (s,dxb)
--                                in
--                                dsb + dsx
revAdExpr is gs s dt (CLLet tv rhs body)
  = mkTempLet is (tVarName tv) (fromCLExpr is gs s rhs) $ \is x ->
    let s'  = clExtend gs x s
        gs' = typeof tv : gs
    in
    mkTempLet is "dx" (revAdExpr is gs' s' dt body) $ \ is dx ->
    let (dxb, dsb) = clSplit gs' dx
    in pTsAdd dsb (revAdExpr is gs s dxb rhs)

revAdExpr _ _ _ _ CLFold{} = foldUnimplemented "revAdExpr"

--------------------------------------------
--   Spilt AD
--   fwds:  S => T  =   S -> (T, X)
--   revs:  S => T  =   (dT,X) -> dS
--------------------------------------------

type FwdSplit = InScopeSet -> TExpr -> TExpr
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
                   , cldef_pat = pat
                   , cldef_rhs = rhs
                   , cldef_res_ty = res_ty })
  = ( extendGblST gst [fwd_def, rev_def]
    , [fwd_def, rev_def])
  where
    pvs = patVars pat
    gs  = map typeof pvs

    SR { sr_fwd = fwd, sr_rev = rev, sr_empx = emp } = fsAdExpr gst gs rhs

    fwd_def = Def { def_fun    = DrvFun f (AD { adPlan = SplitAD, adDir = Fwd })
                  , def_pat    = pat
                  , def_res_ty = fwd_rhs_ty
                  , def_rhs    = UserRhs final_fwd_rhs }
    fwd_in_scope   = mkInScopeSet pvs
    fwd_rhs        = fwd fwd_in_scope (mkTuple (map Var pvs))
    final_fwd_rhs = case emp of
                       XEmpty    -> Tuple [fwd_rhs, unitExpr]
                       XNonEmpty -> fwd_rhs

    fwd_rhs_ty = typeof final_fwd_rhs
    x_ty = case fwd_rhs_ty of
             TypeTuple [_, x_ty] -> x_ty
             _ -> pprPanic "fsAdDef" (ppr fwd_rhs_ty)

    rev_def = Def { def_fun    = DrvFun f (AD { adPlan = SplitAD, adDir = Rev })
                  , def_pat    = rev_pat
                  , def_res_ty = tangentType (typeof pat)
                  , def_rhs    = UserRhs rev_rhs }

    (rev_pat, rev_rhs)
       = case emp of
           XNonEmpty -> (TupPat [dt,x], rev is (Var dt) (Var x))
           XEmpty    -> (VarPat dt,     rev is (Var dt) unitExpr)

    x  = mkTVar x_ty "x"
    dt = mkTVar (tangentType res_ty) "dt"
    is = mkInScopeSet [dt,x]

-------------------
fsAdExpr :: GblSymTab -> GammaShape -> CLExpr -> SplitResult
-- This is the main workhorse
-- The GammaShape is the type of the argument tuple at that point

fsAdExpr _ gs (CLKonst k)
  = assert (text "fsAdExpr:CLConst") (null gs) $
           -- GammaShape is [] so result dT is ()
    SR { sr_fwd  = \ _ _   -> Konst k
       , sr_rev  = \ _ _ _ -> Tuple []
       , sr_empx = XEmpty }

fsAdExpr _ _ CLId
  = SR { sr_fwd  = \ _ s    -> s
       , sr_rev  = \ _ dt _ -> dt
       , sr_empx = XEmpty }

-- prune ts <sf> s  =  (pick ts s, zeros)
-- prune ts <sr> (dt,zeros) =  spread ts dt zeros

fsAdExpr _ gs (CLPrune ts n)
  = SR{ sr_fwd  = fwd, sr_rev  = rev, sr_empx = zero_emp }
  where
    fwd _ s = mkXFwdPair (mkTuple (pick ts ss))
                          [ (zero_emp, mkTuple (pick zero_ts ss)) ]
             where
               ss = splitTuple s n

    tys = case gs of
           [TypeTuple tys] -> tys
           _               -> gs

    rev _ dt xzero = mkTuple (map (do_one dt xzero) (tys `zip` [1..]))

    m     = length ts
    z     = length zero_ts
    t_prs = ts      `zip` [1..]
    z_prs = zero_ts `zip` [1..]

    do_one ds xzero (ty, i_n)
      | Just j_m <- findIndex t_prs i_n = pSel j_m m ds
      | Just k_m <- findIndex z_prs i_n = pSel k_m z xzero
      | otherwise                       = mkTangentZeroFromType ty

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
fsAdExpr gst gs (CLComp g f)
  = SR { sr_fwd = \ is s ->
                  mkXPairLet is "rg" "xg" (g_empx, fwd_g is s)  $ \is rg xg ->
                  mkXPairLet is "rf" "xf" (f_empx, fwd_f is rg) $ \_  rf xf ->
                  mkXFwdPair rf [(f_empx,xf), (g_empx, xg)]
       , sr_rev = \ is dt x ->
                  let (xf:xg:_) = splitXTuple x [f_empx, g_empx] in
                  mkTempLet is "dtf" (rev_f is dt xf) $ \is dtf ->
                  rev_g is dtf xg
       , sr_empx = empx }
  where
    empx = f_empx `andXS` g_empx
    SR { sr_fwd = fwd_f, sr_rev = rev_f, sr_empx = f_empx } = fsAdExpr gst gs_f f
    SR { sr_fwd = fwd_g, sr_rev = rev_g, sr_empx = g_empx } = fsAdExpr gst gs   g
    gs_f = gsResult gs g

fsAdExpr gst gs (CLCall _ f)
  = SR { sr_fwd = fwd, sr_rev = rev, sr_empx = emp }
  where
    fs_fun = DrvFun f (AD { adPlan = SplitAD, adDir = Fwd })
    fr_fun = DrvFun f (AD { adPlan = SplitAD, adDir = Rev })

    fwd _ s = case emp of
                 XEmpty    -> pFst (mk_call fs_fun s)
                 XNonEmpty -> mk_call fs_fun s

    rev _ dt x = mk_call fr_fun $
                 case emp of { XEmpty -> dt; XNonEmpty -> Tuple [dt,x] }

    emp = funXShape gst f (mkTupleTy gs)

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
fsAdExpr gst gs (CLTuple cs)
  = SR { sr_fwd = fwd, sr_rev = rev, sr_empx = all_emp }
  where
    srs     = map (fsAdExpr gst gs) cs
    emps    = map sr_empx srs
    all_emp = foldr andXS XEmpty emps

    fwd is s = mkXPairLets is "r" "x" [ (sr_empx sr, sr_fwd sr is s)
                                      | sr <- srs] $ \_ ts xs  ->
               mkXFwdPair (mkTuple ts) (emps `zip` xs)

    rev is dt x
      = foldr1 pTsAdd [ sr_rev sr is dt x
                      | (sr,(dt,x)) <- srs `zip` (dts `zip` xs) ]
      where
       -- We should probably use lets for these
       dts = splitTuple  dt (length cs)
       xs  = splitXTuple x  emps


--  (let v = c in b) <sf> s = let (rc,xc) = c <sf> s
--                            let (rb,xb) = b <sf> (rc:s)
--                            in (rb, (xc,xb))
--  (let v = rhs in b) <sr> (dt,x) = let (xrhs,xbody)  = x
--                                     rb = b <sr> (dt,xbody)
--                                     (drhs:dr1) = rb
--                                     dr2        = rhs <sr> (drhs,xrhs)
--                                 in dr1 + dr2
fsAdExpr gst gs (CLLet tv rhs body)
  = SR { sr_fwd = fwd, sr_rev = rev, sr_empx = rhs_emp `andXS` body_emp }
  where
    fwd is s = mkXPairLet is "rc" "xc" (rhs_emp, fwd_rhs is s)    $ \ is rc xc ->
               let s' = clExtend gs rc s in
               mkXPairLet is "rb" "xb" (body_emp, fwd_body is s') $ \ _ rb xb ->
               mkXFwdPair rb [(rhs_emp, xc), (body_emp, xb)]
    rev is dt x
      = mkTempLet is "rb" (rev_body is dt xbody) $ \ is rb ->
        let (drhs, dr1) = clSplit gs' rb in
        pTsAdd dr1 (rev_rhs is drhs xrhs)
      where
       (xrhs:xbody:_) = splitXTuple x [rhs_emp, body_emp]

    gs' = typeof tv : gs
    SR { sr_fwd = fwd_rhs,  sr_rev = rev_rhs,  sr_empx = rhs_emp  }
      = fsAdExpr gst gs rhs
    SR { sr_fwd = fwd_body, sr_rev = rev_body, sr_empx = body_emp }
      = fsAdExpr gst gs' body

--  (build n i e) <sf>  s     = unzip (build n (\i. e <sf> (i:s)))
--  (build n i e) <sr> (dt,x) = sumbuild n (\i. e <sr> (dt[i], x[i]))
fsAdExpr gst gs (CLBuild sz tvi elt)
  = SR { sr_fwd = fwd, sr_rev = rev, sr_empx = elt_emp }
  where
    fwd is s = let (is', tvi') = notInScopeTV is tvi
                   s' = clExtend gs (Var tvi') s in
               pUnzip $ pBuild (fromCLExpr is gs s sz) $
               Lam tvi' $
               fwd_elt is' s'

    rev is dt x = let (is', tvi') = notInScopeTV is tvi in
                  pSumBuild (pSize dt) $ Lam tvi' $
                  snd $ clSplit gs' $
                  rev_elt is' (pIndex (Var tvi') dt) (pIndex (Var tvi') x)

    gs' = TypeInteger : gs
    SR { sr_fwd = fwd_elt, sr_rev = rev_elt, sr_empx = elt_emp }
      = fsAdExpr gst gs' elt

fsAdExpr _ _ CLFold{} = foldUnimplemented "fsAdEXpr CLFold not yet implemented"


funXShape :: HasCallStack => GblSymTab -> FunId -> Type -> XShape
funXShape gst fn arg_ty
  = case callResultTy_maybe gst fs_fn arg_ty of
      Right res_ty
        | TypeTuple [_, x] <- res_ty
        -> case x of
             TypeTuple [] -> XEmpty
             _            -> XNonEmpty
        | otherwise
        -> pprPanic "hasEmptyX" (ppr fn <+> ppr fs_fn <+> ppr arg_ty)
      Left _ -> pprPanic "hasEmptyX:" (quotes (ppr fs_fn) <+> text "is not in scope")
           -- Not in GST, or bad shape
  where
    fs_fn = DrvFun fn (AD { adPlan = SplitAD, adDir = Fwd })


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


-----------------------------------------------
-- Utilities
-----------------------------------------------


pick :: (HasCallStack, Pretty a) => [Int] -> [a] -> [a]
-- Pick the specifed items from the list
pick ts es = [ get t | t <- ts ]
  where
    get t | t > length es = pprPanic "pick" (ppr ts $$ ppr es) (head es)
          | otherwise     = es !! (t-1)

findIndex :: Eq a => [(a,b)] -> a -> Maybe b
findIndex prs i
  = go prs
  where
    go [] = Nothing
    go ((a,b):prs) | a ==i     = Just b
                   | otherwise = go prs

makeArgDupable :: InScopeSet -> TExpr
               -> (InScopeSet -> TExpr -> TExpr)
               -> TExpr
makeArgDupable is e thing_inside
  | isDupable e
  = thing_inside is e
  | Let v r b <- e
  = Let v r $
    makeArgDupable (v `extendInScopeSet` is) b $ \ is b ->
    thing_inside is b
  | otherwise
  = mkTempLet is "ax" e thing_inside

isDupable :: TExpr -> Bool
isDupable (Tuple es) = all isTrivial es
isDupable (Call f a) = f `isThePrimFun` "tupCons"
                     && isDupable a
isDupable _ = False

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


