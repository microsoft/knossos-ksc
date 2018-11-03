{-# LANGUAGE LambdaCase #-}

module Cgen where

import Debug.Trace (trace, traceM, traceShowId)

import qualified Data.Map                      as Map
import           Data.List                      ( intercalate, reverse )
import           Data.Maybe                     ( fromJust )

import           Control.Monad                  ( (<=<) )
import qualified Control.Monad.State           as S

import qualified Lang                          as L
import qualified ANF
 
type M = S.State Int

dbtrace msg e = e

spc:: String -> String -> String
spc x y = x ++ " " ++ y

assertEqualThen msg t1 t2 =
  if t1 == t2 then t1 else error ("Asserts unequal ["++msg++"] " ++ show t1 ++ " == " ++ show t2)

runM :: M a -> a
runM = flip S.evalState 0

freshCVar :: M String
freshCVar = do
  s <- S.get
  S.put (s + 1)
  return ("c" ++ show s)

freshVar :: M L.Var
freshVar = do
  -- This doesn't communicate very well but is quick (and dirty)
  s <- freshCVar
  return (L.Simple s)
  
type ST = Map.Map L.Var L.Type
stInsert:: L.Var -> L.Type -> ST -> ST
stInsert (L.TVar ty v) t env = stInsert v (assertEqualThen ("putting " ++ show v ++ " into " ++ show env) t ty) env
stInsert v t env = dbtrace("Inserting " ++ show v ++ " = " ++ show t ++ " in " ++ show env ++ "\n") (Map.insert v t env)

stLookup:: L.Var -> ST -> L.Type
stLookup (L.TVar ty v) env = assertEqualThen ("getting " ++ show v ++ " from " ++ show env) ty (stLookup v env)
stLookup v env = 
  case Map.lookup v env of
    Just a  -> a
    Nothing -> error ("Couldn't find " ++ show v ++ " in " ++ show env)

stCreate::ST
stCreate = Map.empty


anf :: L.Expr -> M L.Expr
anf = \case
  L.Konst k -> return (L.Konst k)
  L.Var   v -> return (L.Var v)
  L.Call f (L.Tuple ts) -> do
    anfArgs <- mapM letAnf ts
    return
      (foldr (.) id (map fst anfArgs) (L.Call f (L.Tuple (map (L.Var . snd) anfArgs))))
  L.Call f e -> do
    (lete, ve) <- letAnf e
    return (lete (L.Call f (L.Var ve)))
  L.Tuple ts -> do
    anfArgs <- mapM letAnf ts
    return
      (foldr (.) id (map fst anfArgs) (L.Tuple (map (L.Var . snd) anfArgs)))
  L.Lam x e -> do
    anfe <- anf e
    return (L.Lam x anfe)
  L.App f x -> do
    (letf, vf) <- letAnf f
    (letx, vx) <- letAnf x
    return (letf (letx (L.App (L.Var vf) (L.Var vx))))
  L.Let x e body -> do
    anfe    <- anf e
    anfbody <- anf body
    return (L.Let x anfe anfbody)
  L.If cond ift iff -> do
    anfcond <- anf cond
    anfift  <- anf ift
    anfiff  <- anf iff
    return (L.If anfcond anfift anfiff)
  L.Assert cond body -> do
    anfcond <- anf cond
    anfbody <- anf body
    return (L.Assert anfcond anfbody)

letAnf :: L.Expr -> M (L.Expr -> L.Expr, L.Var)
letAnf e = do
  anfe <- anf e
  ve   <- freshVar
  return (L.Let ve anfe, ve)

-- NB SPJ's ANF doesn't actually seem to replace function arguments
-- with variables
anfSPJ :: L.Expr -> L.Expr
anfSPJ = snd . ANF.runAnf 0 . ANF.anfE

cgenDef :: L.Def -> String
cgenDef def = let (line,vf,tf)  = cgenDefE stCreate def in line

cgenDefs :: [L.Def] -> [String]
cgenDefs defs =
  let env = stCreate in
  reverse $ snd $ foldl accum (Map.empty,[]) defs
    where accum :: (ST, [String]) -> L.Def -> (ST, [String])
          accum (env, ls) def = let (l,vf,tf) = go env def in 
                                (stInsert (L.Simple vf) tf env, l:ls)
                                  where go :: ST -> L.Def -> CGenResult 
                                        go env def =
                                          cgenDefE (dbtrace ("Passing ENV[" ++ show env ++ "]" ++ " to " ++ (show $ L.ppr def) ++ "\n") env) def

cgenDefE :: ST -> L.Def -> CGenResult
cgenDefE env (L.Def f vars expr) =
  let _ = trace ("Def " ++ show f ++ "\n") () in
  let body_env = foldr addVarToEnv env vars
        where addVarToEnv :: L.Var -> ST -> ST
              addVarToEnv (L.TVar ty v) env = stInsert v ty env
              addVarToEnv v _ = error $ "Untyped parameter [" ++ show v ++ "] in def " ++ show f
        in 
  let (cExpr, cVar, cType) = runM $ cgenExpr body_env expr in

    (  cgenType cType `spc` cgenFun f
    ++ "("
    ++ intercalate
         ", "
         (map (\(L.TVar ty var) -> cgenType ty `spc` cgenVar var) vars)
    ++ ") {\n"
    ++ cExpr
    ++ "return "
    ++ cVar
    ++ ";\n}\n"
    , cgenFun f
    , cType
    )

cgenType :: L.Type -> String
cgenType = \case
  L.TypeFloat    -> "double"
  L.TypeInteger  -> "int"
  L.TypeTuple ts -> "tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  L.TypeVec t    -> "vec<" ++ cgenType t ++ ">"
  L.TypeBool     -> "bool"
  L.TypeUnknown  -> "auto"
  L.TypeLambda from to -> "std::function<" ++ cgenType to ++ "(" ++ cgenType from ++ ")>"

typeofKonst = \case
  L.KZero      -> L.TypeZero
  L.KInteger _ -> L.TypeInteger
  L.KFloat   _ -> L.TypeFloat
  L.KBool    _ -> L.TypeBool


-- CGenResult is (C declaration, C expression, L.Type)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r",
--       TypeFloat)
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0",      -- this is what we use at the occurrence
--       TypeFloat)  -- and this is the type of the occurrence
type CGenResult = (String, String, L.Type)

cgenExpr :: ST -> L.Expr -> M CGenResult
cgenExpr env = cgenExprR env <=< anf

-- The input expression must be in ANF
cgenExprR :: ST -> L.Expr -> M CGenResult
cgenExprR env = \case
  L.Konst k -> do
    return
      ( ""
      , cgenKonst k
      , typeofKonst k
      )
  L.Var v -> return
    ( ""
    , cgenVar v
    , stLookup v env 
    )
  
  L.Call f x -> case x of
    L.Tuple vs -> do
      v <- freshCVar
      let cgresults = map (runM <$> cgenExprR env) vs
      let decls = map (\ (d,_,_) -> d) cgresults
      let exprs = map (\ (_,d,_) -> d) cgresults
      let types = map (\ (_,_,d) -> d) cgresults
      let ty = typeofFun env f types
      
      return
        ( cgenType ty
        ++ " "
        ++ v
        ++ " = "
        ++ cgenFun f
        ++ "("
        ++ intercalate "," exprs
        ++ ");\n"
        , v
        , ty
        )
    L.Var v -> do
      vf <- freshCVar
      let ty = typeofFun env f [(stLookup v env)]
      
      return
        ( cgenType ty `spc` vf ++ " = "
        ++ cgenFun f ++ "(" ++ cgenVar v ++ ");\n"
        , vf
        , ty
        )
    
    _ -> error $ "Function arguments should be Var in ANF, not" ++ show x ++ " in call to " ++ show f

  L.Let v e1 e2 -> do
    (cE1, vE1, t1) <- cgenExprR env e1
    (cE2, vE2, t2) <- cgenExprR (stInsert v t1 env) e2
    return
      ( cE1 ++ cgenType t1 ++ " " ++ cgenVar v ++ " = " ++ vE1 ++ ";\n" ++ cE2
      , vE2
      , t2
      )

  L.Tuple ts -> do
    cT <- freshCVar

    let unVar :: L.Expr -> L.Var
        unVar = \case
          L.Var v -> v
          _       -> error "Tuple: Expected arguments to be Vars"

        vars = map unVar ts

        ty   = L.TypeTuple (map (flip stLookup env) vars)

    return
      ( ""
      , "std::make_tuple(" ++ intercalate "," (map cgenVar vars) ++ ")"
      , ty
      )

  L.Lam v body -> do
    case v of
      L.TVar tv v -> do
        l <- freshCVar
        let body_env = stInsert v tv env
        (cE, vE, t) <- cgenExprR body_env body
        let tret = L.TypeLambda tv t
        return (
            cgenType tret `spc` l ++ " = [&](" ++ cgenType tv `spc` cgenVar v ++ ") { " ++ cE ++ 
            "   return "++ vE ++"; };\n"
            , l
            , tret
            )
      _ -> error $ "Bad Lambda ["++show v++"] ["++show body++"] -- need type declarations on parameters"
  L.If c texpr fexpr  -> do
    cret <- freshCVar

    (cc, vc, tyc) <- cgenExprR env c
    (ct, vt, tyt) <- cgenExprR env texpr
    (cf, vf, tyf) <- cgenExprR env fexpr
    let tret = tyt

    return (   cc -- emit condition generation
            ++ cgenType tret ++ "/* Should = " ++ cgenType tyf ++ "*/" `spc` cret ++ ";\n" -- emit decl for "return" type
            ++ "if ("++vc++") {" 
            ++ "  " ++ ct ++ ";\n" -- compute true value
            ++ "  " ++ cret ++ "=" ++ vc ++ ";\n" -- assign to "return"
            ++ "} else {\n" -- else
            ++ "  " ++ cf ++ ";\n" -- compute false value
            ++ "  " ++ cret ++ "=" ++ vf ++ ";\n" -- assign to "return"
            ++ "}\n" -- phew
              , cret
              , tret
          )

  L.Assert _cond body -> do
    (cc, vc, tyc) <- cgenExprR env _cond
    (ce, ve, tye) <- cgenExprR env body
    return ( cc `spc` "ASSERT(" ++ vc ++ ");\n" ++ ce
      , ve
      , tye
      )
  L.App{}             -> error "App"

cgenFun :: L.Fun -> String
cgenFun = \case
  L.Fun funId -> case funId of
    L.SFun fun -> translateFun fun
    L.SelFun i n -> "selfun_" ++ show i ++ "_" ++ show n
  f -> error ("cgenFun: " ++ show f)

cgenKonst :: L.Konst -> String
cgenKonst = \case
  L.KZero      -> "0"
  L.KInteger i -> show i
  L.KFloat   f -> show f
  L.KBool    b -> if b then "TRUE" else "FALSE"

cgenVar :: L.Var -> String
cgenVar = \case
  L.TVar _ v -> "/*T*/" ++ cgenVar v
  L.Simple s -> "s_" ++ s
  L.Delta  d -> "d_" ++ d
  L.Grad g m ->
    "g_"
      ++ g
      ++ "_"
      ++ (case m of
           L.Fwd -> "f"
           L.Rev -> "r"
         )
 
-- A single place for "domain knowledge" about functions -- to be dumped when we get symtabs
typeofFun env f tys =
  case (f,tys) of
    (L.Fun (L.SFun "build"), [tysize, L.TypeLambda L.TypeInteger t]) -> L.TypeVec t
    (L.Fun (L.SFun "index"), [tind, (L.TypeVec t)]) -> t
    (L.Fun (L.SFun "size"), [(L.TypeVec t)]) -> L.TypeInteger
    (L.Fun (L.SFun "sum"), [(L.TypeVec t)]) -> t
    (L.Fun (L.SFun "exp"), [(L.TypeFloat)]) -> L.TypeFloat
    (L.Fun (L.SFun "log"), [(L.TypeFloat)]) -> L.TypeFloat
    (L.Fun (L.SFun "+"  ), (t:tys)) -> t
    (L.Fun (L.SFun "/"  ), (t:tys)) -> t
    (L.Fun (L.SFun "*"  ), (t:tys)) -> t
    (L.Fun (L.SFun "-"  ), (t:tys)) -> t
    (L.Fun (L.SFun "=="  ), _) -> L.TypeBool
    (L.Fun (L.SFun "<"  ), _) -> L.TypeBool
    (L.Fun (L.SelFun i n), [t]) -> case t of
                        L.TypeTuple tys -> tys!!(i-1)
                        L.TypeVec t -> t
                        _ -> error ("SelFun" ++ show i ++ " " ++ show n ++ "[" ++ show t ++ "]")
    (L.Fun (L.SFun f), _) ->   case Map.lookup (L.Simple f) env of
                                Just a  -> a
                                Nothing -> trace("Failed to type fun [" ++ show f ++ "], types [" ++ show tys ++ "]") L.TypeUnknown
    _                -> let emsg = "Failed to type fun [" ++ show f ++ "], types [" ++ show tys ++ "]" in
                          trace emsg L.TypeUnknown 

translateFun = \case
  "*" -> "mul"    
  "+" -> "add"    
  "/" -> "div"    
  "-" -> "sub"    
  "==" -> "eq"    
  "<" -> "lt"    
  s -> s
