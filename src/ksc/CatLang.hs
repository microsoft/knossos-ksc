module CatLang where

import Prelude hiding( (<>) )
import Lang
import Prim
import Data.Maybe( mapMaybe )

data CLExpr
  = CLKonst Konst
  | CLCall TFun
  | CLComp CLExpr CLExpr
  | CLSel Int Int    -- (CLSel i n) selects the
                     -- i'th component of a n-tuple
                     -- One indexed, so 1 is first component
  | CLTuple [CLExpr]
  | CLIf CLExpr CLExpr CLExpr
  | CLLet TVar CLExpr CLExpr   -- Rhs and body

data CLDef = CLDef { cldef_fun    :: Fun
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
     = sep [ hang (text "def" <+> pprFun f <+> pprParendType res_ty)
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
pprCLExpr _ (CLCall f)   = ppr f
pprCLExpr _ (CLSel i n)  = text "pi" <> ppr i <> char '_' <> ppr n
pprCLExpr _ (CLTuple es) = parens (pprList ppr es)
pprCLExpr p (CLIf b t e) = parensIf p precZero $
                           sep [ text "if" <+> ppr b
                               , text "then" <+> ppr t
                               , text "else" <+> ppr e]
pprCLExpr p (CLLet x r b)
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
  = Just CLDef { cldef_fun = CLFun f
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
  = CLCall (TFun ty (CLFun f)) `CLComp` toCLExpr env e
toCLExpr env (Var tv)
  = CLSel (find 1 env) (length env)
  where
    find n (env_v : env_vs)
      | tv == env_v = n
      | otherwise   = find (n+1) env_vs
    find _ [] = pprPanic "tcCLExpr:var" (ppr tv $$ ppr env)

toCLExpr env (Let tv rhs body)
  = CLLet tv (toCLExpr env rhs)
              (toCLExpr (tv:env) body)

toCLExpr _ e = pprPanic "toCLExpr" (ppr e)

-----------------------------------------------
--  Convert from CLDedf
-----------------------------------------------

fromCLDefs :: [CLDef] -> [TDef]
fromCLDefs cldefs = map fromCLDef cldefs

fromCLDef :: CLDef -> TDef
fromCLDef (CLDef { cldef_fun = CLFun f
                 , cldef_args = arg
                 , cldef_res_ty = res_ty
                 , cldef_rhs = rhs })
  = Def { def_fun  = Fun f
        , def_args = arg
        , def_res_ty = res_ty
        , def_rhs  = UserRhs (fromCLExpr (1, Var arg) rhs) }

fromCLDef def = pprPanic "fromCLDef" (ppr def)  -- Not a CLFun

fromCLExpr :: (Int, TExpr) -> CLExpr -> TExpr
-- In (fromClExpr arg cl)
--   If 'arg' is (1, arg)  then the arg is not a tuple
--   If 'arg' is (n, args) then args is a n-tuple
fromCLExpr _   (CLKonst k)  = Konst k
fromCLExpr arg (CLTuple es) = Tuple (map (fromCLExpr arg) es)
fromCLExpr arg (CLIf b t e) = If (fromCLExpr arg b) (fromCLExpr arg t) (fromCLExpr arg e)

fromCLExpr (_,arg) (CLCall fun)
  | TFun ty (CLFun f) <- fun
  = Call (TFun ty (Fun f)) arg
  | otherwise
  = pprPanic "fromCLExpr:CLCall" (ppr fun)

fromCLExpr (n, arg) (CLSel i n')
  = assert (text "fromCLExpr1" <+> ppr (n,n')) (n == n') $
    pSel i n arg

fromCLExpr arg (CLLet tv rhs body)
  = Let tv (fromCLExpr arg rhs)
           (fromCLExpr (extendArg (Var tv) arg) body)

fromCLExpr arg (CLComp e1 e2)
  = fromCLExpr (1, fromCLExpr arg e2) e1

extendArg :: TExpr -> (Int,TExpr) -> (Int,TExpr)
extendArg arg2 (1,arg1)       = (2, Tuple [arg2,arg1])
extendArg arg  (n,Tuple args) = (n+1, Tuple (arg:args))
extendArg arg  (n,args)       = (n+1, pConsTup arg args)


-----------------------------------------------
-- AD with tupling
 -----------------------------------------------

{-

  S => T  =   S -> (T, S -o T)

  (.) :: (b => c) -> (a => b) -> (a => c)
  (f . g) <> s = let (b, lmab) = g <> s
                     (c, lmbc) = f <> b
                 in (c, lmbc `lmComp` lmab)

  (,) :: (s => t1) -> (s => t2) -> (s =-> (t1,t2))
  (a,b) <> s = let (a, lmsa) = a <> s
                   (b, lmsb) = b <> s
               in )(a,b), lmsa x lmsb)

-}

{-
adCLExpr :: CLExpr -> TExpr
adCLExpr _ (CLKonst k) = Konst k

adCLExpr args (CLCall fun)
  | TFun ty (CLFun f) <- fun
  = Call (TFun ty (Fun f)) args
  | otherwise
  = pprPanic "fromCLExpr:CLCall" (ppr fun)

adCLExpr args (CLComp e1 e2)
  = mkTempLet "b" (adCLExpr args         e2) $ \ bpair ->
    mkTempLet "c" (adCLExpr (pFst bpair) e1) $ \ cpair ->
    Tuple [ pFst cpair
          , pSnd cpair `lmCompose` pSnd bpair ]

adCLExpr args (CLTuple es)
  = mkTempLets "t" es $ \ tvs ->
    Tuple [ Tuple (map pFst tvs)
          , lmVCat (map pSnd tvs) ]

mkTempLets :: String -> [TExpr] -> ([TExpr] -> TExpr) -> TExpr
mkTempLets s es body_fn
  = body_fn (go 1 es)
  where
    go n []     tvs = body_fn (revverse tvs)
    go n (e:es) tvs = mkTempLet (s ++ show n) e $ \tv ->
                      go (n+1) es (tv:tvs)

mkTempLet :: String -> TExpr -> (TExpr -> TExpr) -> TExpr
mkTempLet s rhs body_fn
  = mkLet tv rhs (body_fn (Var tv))
  where
    tv = TVar (typeof rhs) (Simple s)
-}