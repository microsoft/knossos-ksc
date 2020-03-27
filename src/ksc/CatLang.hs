module CatLang( CLDef, toCLDefs, fromCLDefs, fwdAdDefs, revAdDefs ) where

import Prelude hiding( (<>) )
import Lang
import LangUtils
import Prim
import OptLet
import Data.Maybe( mapMaybe )

data CLExpr
  = CLKonst Konst
  | CLCall Type FunId      -- The Type is the result type
  | CLComp CLExpr CLExpr
  | CLSel Int Int    -- (CLSel i n) selects the
                     -- i'th component of a n-tuple
                     -- One indexed, so 1 is first component
  | CLTuple [CLExpr]
  | CLIf CLExpr CLExpr CLExpr
  | CLLet Type String CLExpr CLExpr   -- Rhs and body

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
pprCLExpr _ (CLComp e1 e2) = sep [ pprCLExpr precOne e1
                                 , vcat [ text "." <+> pprCLExpr precOne e
                                        | e <- collect e2 ] ]
                           where
                             collect (CLComp a b) = a : collect b
                             collect e            = [e]

pprCLExpr _ (CLKonst k)  = ppr k
pprCLExpr _ (CLCall _ f) = ppr f
pprCLExpr _ (CLSel i n)  = text "pi" <> ppr i <> char '_' <> ppr n
pprCLExpr _ (CLTuple es) = parens (pprList ppr es)
pprCLExpr p (CLIf b t e) = parensIf p precZero $
                           sep [ text "if" <+> ppr b
                               , text "then" <+> ppr t
                               , text "else" <+> ppr e]
pprCLExpr p (CLLet _ty x r b)
  = parensIf p precZero $
    vcat [ text "let" <+> (bracesSp $ sep [ ppr x <+> char '=', nest 2 (pprCLExpr precZero r) ])
         , pprCLExpr precZero b ]


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

toCLExpr :: [TVar] -> TExpr -> CLExpr
toCLExpr _   (Konst k) = CLKonst k
toCLExpr env (If e1 e2 e3) = CLIf (toCLExpr env e1)
                                  (toCLExpr env e2)
                                  (toCLExpr env e3)
toCLExpr env (Tuple es)  = CLTuple (map (toCLExpr env) es)
toCLExpr env (Call (TFun ty (Fun f)) e)
  = CLCall ty f `CLComp` toCLExpr env e
toCLExpr env (Var tv)
  = CLSel (find 1 env) (length env)
  where
    find n (env_v : env_vs)
      | tv == env_v = n
      | otherwise   = find (n+1) env_vs
    find _ [] = pprPanic "tcCLExpr:var" (ppr tv $$ ppr env)

toCLExpr env (Let tv@(TVar ty (Simple v)) rhs body)
  = CLLet ty v (toCLExpr env rhs)
               (toCLExpr (tv:env) body)

toCLExpr _ e = pprPanic "toCLExpr" (ppr e)

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
fromCLExpr is arg (CLTuple es)   = Tuple (map (fromCLExpr is arg) es)
fromCLExpr is arg (CLIf b t e)   = If (fromCLExpr is arg b)
                                      (fromCLExpr is arg t)
                                      (fromCLExpr is arg e)
fromCLExpr is arg (CLComp e1 e2)
  | CLCall ty f <- e1   -- Shortcut to avoid an unnecessary let
  = Call (TFun ty (Fun f)) (fromCLExpr is arg e2)
  | otherwise
  = mkTempLet is "a" (fromCLExpr is arg e2) $ \ is v2 ->
    fromCLExpr is [v2] e1

fromCLExpr _  arg (CLSel i _)
  | i <= length arg = arg !! (i-1)
  | otherwise = pprTrace "fromCLExp:sel" (ppr i $$ ppr arg) $
                Konst (KInteger 77)
fromCLExpr _  arg (CLCall ty f)  = Call (TFun ty (Fun f)) (mkTuple arg)

fromCLExpr is arg (CLLet ty v rhs body)
  = Let tv (fromCLExpr is arg rhs)
           (fromCLExpr is (Var tv: arg) body)
  where
    tv = TVar ty (Simple v)


{- --------------------------------------------
-- Forward AD, tupled
-----------------------------------------------
   S => T  =   (S, dS) -> (T, dT)

  (.) :: (b => c) -> (a => b) -> (a => c)
  (f . g) <f> s = f (g s)

  (,) :: (s => t1) -> (s => t2) -> (s => (t1,t2))
  (c1,c2) <f> sds = let (ar,dar) = c1 <f> sds
                        (br,dbr) = c2 <f> sds
                    in ((ar,br),(dar,dbr))

  sel_i_n <f> (s,ds) = (sel_i_n s, sel_i_n ds)

  (let x = c in b) <> (s,ds) = let (x,dx) = c <> (s,ds)
                               in b <> (x:s, dx:ds)

-}

fwdAdDefs :: [CLDef] -> [TDef]
fwdAdDefs = error "fwdAdDefs"

revAdDefs :: [CLDef] -> [TDef]
revAdDefs = error "revAdDefs"

{-
fwdAdDefs :: [CLDef] -> [TDef]
fwdAdDefs cldefs = map fwdAdDef cldefs

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
    rhs' = fwdAdExpr (mkInScopeSet [arg'])
                     (Var arg')
                     rhs

fwdAdExpr :: InScopeSet -> [TExpr] -> CLExpr -> TExpr
fwdAdExpr _  _   (CLKonst k)  = Konst k
fwdAdExpr is arg (CLIf a b c) = If (fromCLExpr is arg a) (fwdAdExpr is arg b) (fwdAdExpr is arg c)
fwdAdExpr is arg (CLComp f g) = fwdAdExpr is (fwdAdExpr is arg g) f
fwdAdExpr _  arg (CLSel i n)  = Tuple [ pSel i n (pFst arg), pSel i n (pSnd arg) ]

fwdAdExpr _ arg (CLCall ty f)
  = Call (TFun (tangentPair ty) fwd_fun) arg
  where
    fwd_fun = DrvFun f ad_mode
    ad_mode = AD { adPlan = TupleAD, adDir = Fwd }

fwdAdExpr is arg (CLTuple cs)
  = mkTempLets is (repeat "t") es $ \vs ->
    Tuple [ mkTuple (map pFst vs), mkTuple (map pSnd vs) ]
  where
    es = map (fwdAdExpr is arg) cs

fwdAdExpr is arg (CLLet ty v rhs body)
  = Let tv (fwdAdExpr is arg rhs)
        (fwdAdExpr is' arg' body)
  where
    tv   = TVar (tangentPair ty) (Simple v)
    is'  = extendInScopeSet tv is
    arg' = Tuple [ pTupCons (pFst (Var tv)) (pFst arg)
                 , pTupCons (pSnd (Var tv)) (pSnd arg) ]

tangentPair :: Type -> Type
tangentPair ty = TypeTuple [ty, tangentType ty]

{- --------------------------------------------
-- Reverse AD, not tupled
-----------------------------------------------
   S => T  =   (S, dT) -> dS

  (.) :: (b => c) -> (a => b) -> (a => c)
  (f . g) <r> (a,dc) = let (a,dc) = s
                         b = g <> a
                         db = f <r> (b,dc)
                       in
                       g <r> (a,db)

  (,) :: (s => t1) -> (s => t2) -> (s => (t1,t2))
  (c1,c2) <r> (s,dt) = let (dt1,dt2) = dt
                           ds1 = c1 <r> (s,dt1)
                           ds2 = c2 <r> (s,dt2)
                     in (ds1 + ds2)

  sel_i_n <r> (s,dr) = delta i n dr


  (let x = c in b) <r> (s,dt) = let x         = c <> s
                                    (dxb:dsb) = b <r> (x:s, dt)
                                    dsx       = c <r> (s,dxb)
                                in
                                dsx + dsb
-}

revAdDefs :: [CLDef] -> [TDef]
revAdDefs cldefs = map revAdDef cldefs

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
    arg' = TVar (TypeTuple [arg_ty, tangentType res_ty]) v
    rhs' = revAdExpr (mkInScopeSet [arg'])
                     (Var arg')
                     rhs

revAdExpr :: InScopeSet -> TExpr -> CLExpr -> TExpr
revAdExpr _  arg (CLKonst _)  = mkTangentZero (pFst arg)
revAdExpr is arg (CLIf a b c) = If (fromCLExpr is arg a) (revAdExpr is arg b) (revAdExpr is arg c)

revAdExpr _  arg (CLSel i n)
  = mkTuple (map ds [1..n])    -- Returns (0,0,dr,0)
  where
    s  = pFst arg
    dr = pSnd arg
    ds j | i==j      = dr
         | otherwise = mkTangentZero (pSel i n s)

revAdExpr is arg (CLComp f g)
  = mkTempLet is "cx" (revAdExpr is (Tuple [b,dc]) f) $ \ is db ->
    revAdExpr is (Tuple [a,db]) g
  where
    a  = pFst arg
    dc = pSnd arg
    b  = fromCLExpr is a g

revAdExpr _ arg (CLCall _ f)
  = Call (TFun res_ty rev_fun) arg
  where
    res_ty  = tangentType (typeof (pFst arg))
    rev_fun = DrvFun f ad_mode
    ad_mode = AD { adPlan = BasicAD, adDir = Rev }

revAdExpr is arg (CLTuple cs)
  = foldr1 pAdd $
    [ revAdExpr is (Tuple [s,pSel i n dt]) c
    | (c,i) <- cs `zip` [1..] ]
  where
    n = length cs
    s = pFst arg
    dt = pSnd arg

revAdExpr is arg (CLLet ty v rhs body)
  = mkTempLet is "s" (pFst arg) $ \ is s ->
    Let x (fromCLExpr is s rhs)    $
    mkTempLet is "db"
      (revAdExpr is (Tuple [pTupCons (Var x) s, pSnd arg]) body) $ \ is db ->
    pAdd (pTupTail db) (revAdExpr is (Tuple [s,pTupHead db]) rhs)
  where
    x = TVar ty (Simple v)
-}

-----------------------------------------------
-- Utilities
 -----------------------------------------------

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

mkTempLet :: InScopeSet -> String -> TExpr
          -> (InScopeSet -> TExpr -> TExpr) -> TExpr
mkTempLet is s e thing_inside
  | isTrivial e = thing_inside is e
  | otherwise   = Let tv e $
                  thing_inside is' (Var tv)
  where
    v = notInScope (Simple s) is
    tv = TVar (typeof e) v
    is' = extendInScopeSet tv is


