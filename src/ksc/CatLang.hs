module CatLang where

import Prelude hiding( (<>) )
import Lang
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
pprCLExpr p (CLLet x r b) = parensIf p precOne $
                            sep [ text "let" <+> ppr x <+> char '=' <+> pprCLExpr precZero r
                                , text "in" <+> pprCLExpr precZero b ]


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
        , def_rhs  = UserRhs (fromCLExpr [Var arg] rhs) }

fromCLDef def = pprPanic "fromCLDef" (ppr def)  -- Not a CLFun

fromCLExpr :: [TExpr] -> CLExpr -> TExpr
fromCLExpr _   (CLKonst k)  = Konst k
fromCLExpr args (CLTuple es) = Tuple (map (fromCLExpr args) es)
fromCLExpr args (CLIf b t e) = If (fromCLExpr args b) (fromCLExpr args t) (fromCLExpr args e)
fromCLExpr args (CLCall fun)
  | TFun ty (CLFun f) <- fun
  = Call (TFun ty (Fun f)) args
  | otherwise
  = pprPanic "fromCLExpr:CLCall" (ppr fun)

fromCLExpr args sel@(CLSel i n)
  | n == length args = args !! (i-1)
  | otherwise        = pprPanic "fromCLExpr" (ppr args $$ ppr sel)

fromCLExpr args (CLLet tv rhs body)
  = Let tv (fromCLExpr args rhs)
           (fromCLExpr (Var tv : args) body)

fromCLExpr args (CLComp e1 e2)
  = case fromCLExpr args e2 of       -- Ugh!!
      Tuple es -> fromCLExpr es e1
      e        -> fromCLExpr [e] e1

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