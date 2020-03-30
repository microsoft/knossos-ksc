module CatLang( CLDef, toCLDefs, fromCLDefs, fwdAdDefs, revAdDefs ) where

import Prelude hiding( (<>) )
import Lang
import LangUtils
import Prim
import OptLet
import qualified Data.Set as S
import Data.Maybe( mapMaybe )

data CLExpr
  = CLKonst Konst
  | CLCall Type FunId      -- The Type is the result type
  | CLComp CLExpr CLExpr
  | CLPrune [Int] CLExpr
  | CLTuple [CLExpr]
  | CLIf CLExpr CLExpr CLExpr
  | CLLet String CLExpr CLExpr   -- Rhs and body
  | CLBuild CLExpr String CLExpr -- Size and element expr

data CLDef = CLDef { cldef_fun    :: FunId
                   , cldef_args   :: TVar
                   , cldef_res_ty :: Type
                   , cldef_rhs    :: CLExpr }

-----------------------------------------------
--  Pretty printing
-----------------------------------------------

instance Pretty CLDef where
  ppr (CLDef { cldef_fun = f
             , cldef_args = arg
             , cldef_res_ty = res_ty
             , cldef_rhs = rhs })
     = sep [ hang (text "def" <+> ppr f <+> pprParendType res_ty)
                2 (parens (pprTVar arg))
           , nest 2 (text "=" <+> ppr rhs) ]

instance Pretty CLExpr where
  pprPrec = pprCLExpr

pprCLExpr :: Prec -> CLExpr -> SDoc
pprCLExpr p c@(CLComp {})
  = parensIf p precZero $
    sep [ pprCLExpr precOne c1
        , vcat [ text "." <+> pprCLExpr precOne c
               | c <- cs ] ]
  where
    (c1,cs) = case collect c of
                (c1:cs) -> (c1,cs)
                [] -> pprPanic "pprCLExpr" (text "")

    collect (CLComp a b) = collect a ++ collect b
    collect e            = [e]

pprCLExpr _ (CLKonst k)    = ppr k
pprCLExpr _ (CLCall _ f)   = ppr f
pprCLExpr _ (CLPrune ts c) = sep [ text "Prune" <> ppr ts
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
                         , parens $ char '\\' <> text x <> char '.' <+> pprCLExpr precZero ee ]

-----------------------------------------------
--  Convert to CLDedf
-----------------------------------------------

toCLDefs :: [TDef] -> [CLDef]
toCLDefs defs = mapMaybe toCLDef_maybe defs

toCLDef_maybe :: TDef -> Maybe CLDef
toCLDef_maybe  (Def { def_fun  = fun
                    , def_args = arg
                    , def_res_ty = res_ty
                    , def_rhs  = rhs })
  | Fun f     <- fun
  , UserRhs e <- rhs
  = Just CLDef { cldef_fun = f
               , cldef_args = arg
               , cldef_res_ty = res_ty
               , cldef_rhs = toCLExpr [arg] e }
  | otherwise
  = Nothing

data EnvPruned = Pruned | NotPruned

toCLExpr :: [TVar] -> TExpr -> CLExpr
toCLExpr = to_cl_expr NotPruned

to_cl_expr :: EnvPruned -> [TVar] -> TExpr -> CLExpr

to_cl_expr _ env (Var tv)
  = toCLVar env tv

to_cl_expr _ _ (Konst k)
  = CLKonst k

to_cl_expr Pruned env (If e1 e2 e3)
  = CLIf (toCLExpr env e1) (toCLExpr env e2) (toCLExpr env e3)

to_cl_expr Pruned env (Tuple es)
  = CLTuple (map (toCLExpr env) es)

to_cl_expr pruned env c@(Call f e)
  | f `isThePrimFun` "build"
  , Tuple [n, Lam ti@(TVar _ (Simple i)) body] <- e
  = case pruned of
      NotPruned -> prune env c
      Pruned    -> CLBuild (toCLExpr env n) i (toCLExpr (ti:env) body)

to_cl_expr pruned env (Call (TFun ty (Fun f)) e)
  = CLCall ty f `CLComp` to_cl_expr pruned env e

to_cl_expr NotPruned env (Let tv rhs body)
  = CLLet (tVarName tv)
          (toCLExpr env rhs)
          (toCLExpr (tv:env) body)

to_cl_expr NotPruned env e = prune env e
to_cl_expr Pruned    _   e = pprPanic "toCLExpr" (ppr e)

prune :: [TVar] -> TExpr -> CLExpr
prune env e
  | no_prune_reqd = to_cl_expr Pruned env e
  | otherwise     = CLPrune (map snd trimmed_prs)
                            (to_cl_expr Pruned trimmed_env e)
  where
    fvs = freeVarsOf e
    no_prune_reqd = all (\tv -> tv `S.member` fvs) env
    trimmed_prs :: [(TVar,Int)]
    trimmed_prs = [ pr | pr@(tv,_) <- env `zip` [1..]
                       , tv `S.member` fvs ]
    trimmed_env = map fst trimmed_prs

toCLVar :: [TVar] -> TVar -> CLExpr
toCLVar env tv
  | Just i <- find tv env
  = CLCall (typeof tv) (SelFun i (length env))
  | otherwise
  = pprPanic "tcCLVar" (ppr tv $$ ppr env)

find :: Eq a => a -> [a] -> Maybe Int
-- Return 1-indexed position of the item in the list
find x xs
  = go 1 xs
  where
    go j (x1:xs) | x == x1   = Just j
                 | otherwise = go (j+1) xs
    go _ [] = Nothing

-----------------------------------------------
--  Convert from CLDedf
-----------------------------------------------

fromCLDefs :: [CLDef] -> [TDef]
fromCLDefs cldefs = map fromCLDef cldefs

fromCLDef :: CLDef -> TDef
fromCLDef (CLDef { cldef_fun  = f
                 , cldef_args = arg
                 , cldef_res_ty = res_ty
                 , cldef_rhs = rhs })
  = Def { def_fun    = Fun f
        , def_args   = arg
        , def_res_ty = res_ty
        , def_rhs    = UserRhs rhs' }
  where
    rhs' = fromCLExpr (mkInScopeSet [arg]) [Var arg] rhs

fromCLExpr :: InScopeSet -> [TExpr] -> CLExpr -> TExpr
fromCLExpr _  _   (CLKonst k)    = Konst k
fromCLExpr is arg (CLPrune ts c) = fromCLExpr is (pick ts arg) c
fromCLExpr is arg (CLTuple es)   = Tuple (map (fromCLExpr is arg) es)
fromCLExpr is arg (CLIf b t e)   = If (fromCLExpr is arg b)
                                      (fromCLExpr is arg t)
                                      (fromCLExpr is arg e)
fromCLExpr _  arg (CLCall ty f)
  | SelFun i n <- f
  , Just res <- shortCutSelector i n arg
  = res
  | otherwise
  = Call (TFun ty (Fun f)) (mkTuple arg)

fromCLExpr is arg (CLComp e1 e2)
  | CLCall ty f <- e1   -- Shortcut to avoid an unnecessary let
  = Call (TFun ty (Fun f)) (fromCLExpr is arg e2)
  | otherwise
  = mkTempLet is "ax" (fromCLExpr is arg e2) $ \ is v2 ->
    fromCLExpr is [v2] e1

fromCLExpr is arg (CLLet v rhs body)
  = Let tv rhs' (fromCLExpr is' (Var tv : arg) body)
  where
    rhs'      = fromCLExpr is arg rhs
    rhs_ty    = typeof rhs'
    (is', tv) = notInScopeTV is (Simple v) rhs_ty

fromCLExpr is arg (CLBuild size i elt)
  = pBuild (fromCLExpr is arg size)
           (Lam tv (fromCLExpr is' (Var tv : arg) elt))
  where
    (is', tv) = notInScopeTV is (Simple i) TypeInteger

pick :: [Int] -> [TExpr] -> [TExpr]
-- Pick the specifed items from the list
pick ts es = [ es !! (t-1) | t <- ts ]

{- --------------------------------------------
-- Forward AD, tupled
-----------------------------------------------
   S => T  =   (S, dS) -> (T, dT)
-}


fwdAdDefs :: GblSymTab -> [CLDef] -> (GblSymTab, [TDef])
fwdAdDefs gst cldefs
  = let defs = map fwdAdDef cldefs
    in (extendGblST gst defs, defs)

fwdAdDef :: CLDef -> TDef
fwdAdDef (CLDef { cldef_fun  = f
                , cldef_args = TVar ty v
                , cldef_res_ty = res_ty
                , cldef_rhs = rhs })
  = Def { def_fun    = DrvFun f (AD { adPlan = TupleAD, adDir = Fwd })
        , def_args   = arg'
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
--  (f . g) <f> s = f (g s)
fwdAdExpr is s ds (CLComp f g)
  = mkPairLet is "ay" "da" (fwdAdExpr is s ds g) $ \is a da ->
    fwdAdExpr is [a] [da] f

fwdAdExpr is s ds (CLPrune ts c)
  = fwdAdExpr is (pick ts s) (pick ts ds) c

fwdAdExpr _ s ds (CLCall ty f)
  | SelFun i n <- f
  , Just si  <- shortCutSelector i n s
  , Just dsi <- shortCutSelector i n ds
  = Tuple [ si, dsi ]
  | otherwise
  = Call (TFun (tangentPair ty) fwd_fun)
         (Tuple [mkTuple s, mkTuple ds])
  where
    fwd_fun = DrvFun f ad_mode
    ad_mode = AD { adPlan = TupleAD, adDir = Fwd }

--  (,) :: (s => t1) -> (s => t2) -> (s => (t1,t2))
--  (c1,c2) <f> sds = let (ar,dar) = c1 <f> sds
--                        (br,dbr) = c2 <f> sds
--                    in ((ar,br),(dar,dbr))
fwdAdExpr is s ds (CLTuple cs)
  = mkTempLets is (repeat "t") es $ \vs ->
    Tuple [ mkTuple (map pFst vs), mkTuple (map pSnd vs) ]
  where
    es = map (fwdAdExpr is s ds) cs

--  (let x = c in b) <f> (s,ds) = let (x,dx) = c <f> (s,ds)
--                                in b <f> (x:s, dx:ds)
fwdAdExpr is s ds (CLLet nm rhs body)
  = mkPairLet is nm ("d" ++ nm) (fwdAdExpr is s ds rhs) $ \ is x dx ->
    fwdAdExpr is (x:s) (dx:ds) body

--  (build s i e) <> (s,ds) = let (n,_) = c <f> (s,ds)
--                            in unzip (build n (\i. e <f> (i:s, ():ds))
fwdAdExpr is s ds (CLBuild n i elt)
  = mkTempLet is "np" (fwdAdExpr is s ds n) $ \ is np ->
    let (is', iv) = notInScopeTV is (Simple i) TypeInteger
    in pUnzip $ pBuild (pFst np) $
       Lam iv (fwdAdExpr is' (Var iv : s) (unit:ds) elt)
  where
    unit = Tuple []

tangentPair :: Type -> Type
tangentPair ty = TypeTuple [ty, tangentType ty]


{- --------------------------------------------
-- Reverse AD, not tupled
-----------------------------------------------
   S => T  =   (S, dT) -> dS
-}

revAdDefs :: GblSymTab -> [CLDef] -> (GblSymTab, [TDef])
revAdDefs gst cldefs
  = let defs = map revAdDef cldefs
    in (extendGblST gst defs, defs)

revAdDef :: CLDef -> TDef
revAdDef (CLDef { cldef_fun  = f
                , cldef_args = TVar arg_ty v
                , cldef_res_ty = res_ty
                , cldef_rhs = rhs })
  = Def { def_fun    = DrvFun f (AD { adPlan = BasicAD, adDir = Rev })
        , def_args   = arg'
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

revAdExpr is s dt (CLPrune ts c)
  = mkTempLet is "dps" (revAdExpr is (pick ts s) dt c) $ \ _ dps ->
    let do_one (s,i)
          | Just j <- find i ts = pSel j n dps
          | otherwise           = mkTangentZero s
    in mkTuple (map do_one (s `zip` [1..]))
  where
    n = length ts

revAdExpr _ s dt (CLCall _ f)
  | SelFun i n <- f
  , Just _ <- shortCutSelector i n s  -- Short cut
  = let mk_dr j | i==j      = dt
                | otherwise = mkTangentZero (s !! (j-1))
    in mkTuple (map mk_dr [1..n])
  | otherwise
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
revAdExpr is s dt (CLBuild sz i elt)
  = let (is', iv) = notInScopeTV is (Simple i) TypeInteger
    in pSumBuild (fromCLExpr is' s sz)
                 (Lam iv (pTupTail (revAdExpr is' (Var iv : s) (pIndex (Var iv) dt) elt)))

--  (let x = c in b) <r> (s,dt) = let x         = c <> s
--                                    (dxb:dsb) = b <r> (x:s, dt)
--                                    dsx       = c <r> (s,dxb)
--                                in
--                                dsb + dsx
revAdExpr is s dt (CLLet v rhs body)
  = mkTempLet is v (fromCLExpr is s rhs)             $ \is x ->
    mkTempLet is "dx" (revAdExpr is (x : s) dt body) $ \ is dx ->
    pAdd (pTupTail dx) (revAdExpr is s (pTupHead dx) rhs)

-----------------------------------------------
-- Utilities
 -----------------------------------------------

shortCutSelector :: Int -> Int -> [TExpr] -> Maybe TExpr
shortCutSelector i n es
  | n == 1, [e] <- es = Just e
  | n == length es    = Just (es !! (i-1))
  | otherwise         = -- assert (text "shortCutSelector") (isSingleton es) Nothing
                        pprTrace "shortCutSelector" (ppr i <+> ppr n <+> ppr es) Nothing
  -- The selector might have come from the original program
  -- in which case the [TExpr] should be a singleton

mkTempLets :: InScopeSet -> [String] -> [TExpr]
           -> ([TExpr] -> TExpr) -> TExpr
mkTempLets _ _ [] thing_inside
  = thing_inside []
mkTempLets is (s:ss) (e:es) thing_inside
  = mkTempLet is s e      $ \ is' ve ->
    mkTempLets is' ss es  $ \ ves ->
    thing_inside (ve : ves)
mkTempLets _ [] es _
  = pprPanic "mkTempLets" (ppr es)

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
  | otherwise   = Let tv e $
                  thing_inside is' (Var tv)
  where
    (is', tv) = notInScopeTV is (Simple s) (typeof e)


