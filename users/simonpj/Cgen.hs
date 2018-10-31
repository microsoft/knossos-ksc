{-# LANGUAGE LambdaCase #-}

module Cgen where

import Debug.Trace (trace, traceM, traceShowId)
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromJust  )
import qualified Data.Map                      as Map
import           Control.Monad                  ( (>=>)
                                                , (<=<)
                                                )
import qualified Control.Monad.State           as S
import qualified ANF                           as ANF
import qualified Lang as L 

type M = S.State Int

spc:: String -> String -> String
spc x y = x ++ " " ++ y

assertEqualThen t1 t2 =
  if t1 == t2 then t1 else error ("Asserts unequal " ++ show t1 ++ " == " ++ show t2)

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
  
{-
type ST = Map.Map Var L.Type
stInsert:: Var -> L.Type -> ST -> ST
stInsert (TVar ty v) t env = stInsert v (assertEqualThen t ty) env
stInsert v t env = Map.insert v t env

stLookup:: Var -> ST -> L.Type
stLookup (TVar ty v) env = assertEqualThen ty (stLookup v env)
stLookup v env = 
  case Map.lookup v env of
    Just a  -> a
    Nothing -> error ("Couldn't find " ++ show v ++ " in " ++ show env)

stCreate::ST
stCreate = Map.empty


-------------------- C++ generation

cgenDef :: Def -> String
cgenDef def = let (str,env) = (runM <$> (cgenDefM stCreate)) def in str

xcgenDefs :: [Def] -> [String]
xcgenDefs defs =
  let env = stCreate in
  fst $ foldr strenvAddDef ([],env) defs
  where strenvAddDef:: Def -> ([String], ST) -> ([String], ST)
        strenvAddDef def (strs, env) = 
          let (strdef, newenv) = (runM <$> (cgenDefM env)) def in
            (strs ++ [strdef], newenv)

cgenDefs :: [Def] -> [String]
cgenDefs defs =
   let env = Map.empty in
   fst $ foldr go ([],env) defs
   where go def (strs,env) = 
            let (newstr,newenv) = (runM <$> (cgenDefM env)) def in
               (newstr:strs, trace("NEWENV = " ++ show newenv) newenv)

cgenDefM :: ST -> Def -> M (String, ST)
cgenDefM env (Def f vars expr) = do
  let body_env = foldr addVarToEnv env vars
        where addVarToEnv :: Var -> ST -> ST
              addVarToEnv (TVar ty v) env = stInsert v ty env
              addVarToEnv v _ = error $ "Untyped parameter [" ++ show v ++ "] in def " ++ show f 
  
  (cExpr, cVar, cType) <- cgenExpr body_env expr

  let newenv = stInsert (Simple (cgenFun f)) cType env 

  return 
    (  "\n"
    ++ cgenType cType `spc` cgenFun f
    ++ "("
    ++ intercalate ", " (map (\ (TVar ty v) -> cgenType ty `spc` cgenVar v) vars)
    ++ ") {\n"
    ++ cExpr
    ++ "return "
    ++ cVar
    ++ ";\n}\n"
    ,
    newenv
    )


cgenExpr :: ST -> Expr -> M CGenResult
cgenExpr env  = cgenExprR env <=< anf

typeof :: ST -> Expr -> L.Type 
typeof env expr = case expr of
  Konst (KZero     ) -> TypeZero
  Konst (KInteger i) -> TypeInteger
  Konst (KFloat   f) -> TypeFloat
  Konst (KBool    b) -> TypeBool
  Var v -> stLookup v env
  Call f (Var xv) -> let traceMsg = show (ppr expr) in
                     case stLookup xv env of
                     TypeTuple ts -> typeofFun traceMsg env f ts
                     t -> typeofFun traceMsg env f [t]
  _ -> error "typeof"

typeofFun msg env f ts =
  case (f,ts) of
    (Fun (SFun "build"), [tsize, TypeLambda TypeInteger t]) -> TypeVec t
    (Fun (SFun "index"), [tind, (TypeVec t)]) -> t
    (Fun (SFun "size"), [(TypeVec t)]) -> TypeInteger
    (Fun (SFun "sum"), [(TypeVec t)]) -> t
    (Fun (SFun "exp"), [(TypeFloat)]) -> TypeFloat
    (Fun (SFun "+"  ), (t:ts)) -> t
    (Fun (SFun "/"  ), (t:ts)) -> t
    (Fun (SFun "*"  ), (t:ts)) -> t
    (Fun (SFun "-"  ), (t:ts)) -> t
    (Fun (SFun "=="  ), _) -> TypeBool
    (Fun (SFun "<"  ), _) -> TypeBool
    (Fun (SelFun i n), [t]) -> case t of
                        TypeTuple ts -> ts!!i
                        TypeVec t -> t
                        _ -> error ("oiks[" ++ show (t:ts) ++ "]")
    (Fun (SFun f), _) ->   case Map.lookup (Simple f) env of
                            Just a  -> a
                            Nothing -> trace("Failed to type fun [" ++ show f ++ "], types [" ++ show ts ++ "], in ["++msg++"]") TypeUnknown
    _                -> let emsg = "Failed to type fun [" ++ show f ++ "], types [" ++ show ts ++ "], in ["++msg++"]" in
                          trace emsg TypeUnknown 

translateFun = \case
  "*" -> "mul"    
  "+" -> "add"    
  "/" -> "div"    
  "-" -> "sub"    
  "==" -> "eq"    
  "<" -> "lt"    
  s -> s

-- The input expression must be in ANF
cgenExprR :: ST -> Expr -> M CGenResult
cgenExprR env expr = case expr of
  Konst k -> do
    let ty = typeof env expr
    let c_str = cgenKonst k
    return ("", c_str, ty)
  Var v -> 
    return ("", cgenVar v, stLookup v env)
  Call f x -> case x of
    Var xv -> do
      v <- freshCVar
      let ty = typeof env expr
      return
        ( cgenType ty
        ++ " "
        ++ v
        ++ " = "
        ++ cgenFun f
        ++ "("
        ++ cgenVar xv
        ++ ");\n"
        , v
        , ty
        )
    _ -> error $ "Function arguments should be Var in ANF, not" ++ show x ++ " in call to " ++ show f
  Let v e1 e2 -> do
    (cE1, vE1, t1) <- cgenExprR env e1
    (cE2, vE2, t2) <- cgenExprR (stInsert v t1 env) e2
    return
      ( cE1 ++ cgenType t1 `spc` cgenVar v ++ " = " ++ vE1 ++ ";\n" ++ cE2
      , vE2
      , t2
      )

  Lam v body -> do
    case v of
      TVar tv v -> do
        l <- freshCVar
        let body_env = stInsert v tv env
        (cE, vE, t) <- cgenExprR body_env body
        let tret = TypeLambda tv t
        return (
            cgenType tret `spc` l ++ " = [&](" ++ cgenType tv `spc` cgenVar v ++ ") { " ++ cE ++ 
            "   return "++ vE ++"; };\n"
            , l
            , tret
            )
      _ -> error $ "Bad Lambda ["++show v++"] ["++show body++"] -- need type declarations on parameters"

  Tuple ts -> do
    cT <- freshCVar

    let unVar :: Expr -> (Var, L.Type)
        unVar = \case
          Var v -> (v, stLookup v env)
          _     -> error "Tuple: Expected arguments to be Vars"

        vars = map unVar ts

    let tupleType = TypeTuple (map (\ (v,t) -> t) vars)

    return
      ( ""
      , "std::make_tuple(" ++ intercalate "," (map (\ (v,t) -> cgenVar v) vars) ++ ")"
      , tupleType
      )

  App{} -> error "App"
  Assert c e -> do
    (cc, vc, tc) <- cgenExprR env c
    (ce, ve, te) <- cgenExprR env e
    return ( cc `spc` "ASSERT(" ++ vc ++ ");\n" ++ ce
      , ve
      , te
      )

  If c t f  -> do
    cret <- freshCVar

    (cc, vc, tc) <- cgenExprR env c
    (ct, vt, tt) <- cgenExprR env t
    (cf, vf, tf) <- cgenExprR env f
    let tret = tt

    return (   cc -- emit condition generation
            ++ cgenType tret ++ "/* Should = " ++ cgenType tf ++ "*/" `spc` cret ++ ";\n" -- emit decl for "return" type
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



cgenFun :: Fun -> String
cgenFun = \case
  Fun funId -> case funId of
    SFun fun -> translateFun fun
    SelFun i n -> "selfun_" ++ show i ++ "_" ++ show n

cgenKonst :: Konst -> String
cgenKonst = \case
  KZero      -> "0"
  KInteger i -> show i
  KFloat   f -> show f
  KBool    b -> if b then "TRUE" else "FALSE"

cgenVar :: Var -> String
cgenVar = \case
  TVar _ v -> "/*T*/" ++ cgenVar v
  Simple s -> "s_" ++ s
  Delta  d -> "d_" ++ d
  Grad g m ->
    "g_"
      ++ g
      ++ "_"
      ++ (case m of
           Fwd -> "f"
           Rev -> "r"
         )

cgenType :: L.Type -> String
cgenType = \case
  TypeInteger  -> "int"
  TypeBool  -> "bool"
  TypeFloat  -> "double"
  TypeUnknown  -> "auto"
  TypeTuple ts -> "std::tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  TypeVec t -> "vec<" ++ (cgenType t) ++ ">"
  TypeLambda from to -> "std::function<" ++ cgenType to ++ "(" ++ cgenType from ++ ")>"


anf :: Expr -> M Expr
anf = \case
  Konst k    -> return (Konst k)
  Var   v    -> return (Var v)
  Call f arg -> do
    (let_, var) <- do
      anfArg <- anf arg
      v      <- freshVar
      return (Let v anfArg, v)
    return (let_ (Call f (Var var)))
  Tuple ts -> do
    anfArgs <- flip mapM ts $ \t -> do
      anft <- anf t
      v    <- freshVar
      return (Let v anft, v)
    return
      (foldr (.) id (map fst anfArgs) (Tuple (map (Var . snd) anfArgs)))
  Lam x e -> do
    anfe <- anf e
    return (Lam x anfe)
  App f x -> do
    anff <- anf f
    anfx <- anf x
    vf   <- freshVar
    vx   <- freshVar
    return (Let vf anff (Let vx anfx (App (Var vf) (Var vx))))
  Let x e body -> do
    anfe    <- anf e
    anfbody <- anf body
    return (Let x anfe anfbody)
  If cond ift iff -> do
    anfcond <- anf cond
    anfift  <- anf ift
    anfiff  <- anf iff
    return (If anfcond anfift anfiff)
  Assert cond e -> do
    anfcond <- anf cond
    anfe  <- anf e
    return (Assert anfcond anfe)

-- NB SPJ's ANF doesn't actually seem to replace function arguments
-- with variables
anfSPJ :: Expr -> Expr
anfSPJ = snd . ANF.runAnf 0 . ANF.anfE

-}


anf :: L.Expr -> M L.Expr
anf = \case
  L.Konst k -> return (L.Konst k)
  L.Var   v -> return (L.Var v)
  -- We treat "call of build with lambda argument" as a language
  -- primitive
  L.Call (L.Fun (L.SFun "build")) (L.Tuple [n, L.Lam var body]) -> do
    (letn, vn) <- letAnf n
    anfBody    <- anf body
    return
      (letn
        (L.Call (L.Fun (L.SFun "build")) (L.Tuple [L.Var vn, L.Lam var anfBody])
        )
      )
  L.Call f arg -> do
    (let_, var) <- letAnf arg
    return (let_ (L.Call f (L.Var var)))
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

cgenDefs :: [L.Def] -> [String]
cgenDefs defs =
    map (runM <$> cgenDef) defs

cgenDef :: L.Def -> M String
cgenDef (L.Def f vars expr) = do
  -- FIXME: We don't find the types of function arguments properly.
  -- They should really be attached to the function definition.  For
  -- the moment I'm just fixing the type of function arguments based
  -- on my domain expertise.
  let typeEnvList :: [(L.Var, L.Type)]
      typeEnvList = case f of
        L.Fun (L.SFun "f3"  ) -> map (\v -> (v, L.TypeTuple [L.TypeFloat, L.TypeFloat])) vars
        L.Fun (L.SFun "dot2") -> map (\v -> (v, L.TypeVec L.TypeFloat)) vars
        L.Fun (L.SFun "f8"  ) -> map (\v -> (v, L.TypeVec L.TypeFloat)) vars
        _                     -> map (\v -> (v, L.TypeFloat)) vars

      typeEnv :: Map.Map L.Var L.Type
      typeEnv = Map.fromList typeEnvList

  (cExpr, cVar, _) <- cgenExpr typeEnv expr

  return
    (  "double "
    ++ cgenFun f
    ++ "("
    ++ intercalate
         ", "
         (map (\(var, type_) -> cgenType type_ ++ " " ++ cgenVar var)
              typeEnvList
         )
    ++ ") {\n"
    ++ cExpr
    ++ "return "
    ++ cVar
    ++ ";\n}\n"
    )

cgenType :: L.Type -> String
cgenType = \case
  L.TypeFloat   -> "double"
  L.TypeInteger  -> "int"
  L.TypeTuple ts -> "tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  L.TypeVec t    -> "vec<" ++ cgenType t ++ ">"

-- CGenResult is (C declaration, C expression, L.Type)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r",
--       TypeFloat)
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0",      -- this is what we use at the occurrence
--       TypeFloat)  -- and this is the type of the occurrence
type CGenResult = (String, String, L.Type)

cgenExpr :: Map.Map L.Var L.Type -> L.Expr -> M CGenResult
cgenExpr env = cgenExprR env <=< anf

-- The input expression must be in ANF
cgenExprR :: Map.Map L.Var L.Type -> L.Expr -> M CGenResult
cgenExprR env = \case
  L.Konst k -> do
    v <- freshCVar
    return
      ( "double " ++ v ++ " = " ++ cgenKonst k ++ ";\n"
      , v
      , L.TypeFloat {- FIXME we need to deal with polymorphism -}
      )
  L.Var v -> return
    ( ""
    , cgenVar v
    , case Map.lookup v env of
      Just a  -> a
      Nothing -> error ("Couldn't find " ++ show v)
    )
  L.Call (L.Fun (L.SFun "build")) arg -> case arg of
    L.Tuple [L.Var n, L.Lam var body] -> do
      (cbody, cbodyVar, _ty) <- cgenExprR (Map.insert var L.TypeInteger env) body
      let i = cgenVar var

      let forBody = unlines
            [ "struct vector vector;" -- FIXME: Need to choose the name uniquely!
            , "vector.length = " ++ cgenVar n ++ ";"
            , "vector.data = malloc(" ++ cgenVar n ++ " * sizeof(double));"
            , "for (int "
            ++ i
            ++ " = 0; "
            ++ i
            ++ " < "
            ++ cgenVar n
            ++ "; "
            ++ i
            ++ "++) {"
            , cbody
            , "vector.data[" ++ i ++ "] = " ++ cbodyVar ++ ";"
            , "}"
            ]

      return (forBody, "vector", L.TypeVec L.TypeFloat)


    _ -> error "Need a lambda for build"

  L.Call f x -> case x of
    L.Var xv -> do
      v <- freshCVar
      let ty = case f of
            L.Fun (L.SFun "+"    ) -> L.TypeFloat
            L.Fun (L.SFun "*"    ) -> L.TypeFloat
            L.Fun (L.SFun "/"    ) -> L.TypeFloat
            L.Fun (L.SFun "size" ) -> L.TypeInteger
            L.Fun (L.SFun "index") -> L.TypeFloat
            L.Fun (L.SFun "sum"  ) -> L.TypeFloat
            L.Fun (L.SFun "neg"  ) -> L.TypeFloat
            L.Fun (L.SFun other  ) -> error ("Call of " ++ other)
            L.Fun L.SelFun{}       -> L.TypeFloat -- FIXME: This is probably not
                                   -- quite right since an unstated
                                   -- assumption is that SelFuns are
                                   -- polymorphic
            fother                 -> error ("Call: " ++ show fother)
      return
        ( cgenType ty
        ++ " "
        ++ v
        ++ " = "
        ++ cgenFun f
        ++ "("
        ++ cgenVar xv
        ++ ");\n"
        , v
        , ty
        )
    _ -> error
      ("Function arguments should be Var in ANF.  Function was " ++ show f)
  L.Let v e1 e2 -> do
    (cE1, vE1, t1) <- cgenExprR env e1
    (cE2, vE2, t2) <- cgenExprR (Map.insert v t1 env) e2
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

        ty   = L.TypeTuple (map (fromJust . flip Map.lookup env) vars)

    return
      ( cgenType ty
      ++ " "
      ++ cT
      ++ " = {"
      ++ intercalate "," (map cgenVar vars)
      ++ "};\n"
      , cT
      , ty
      )

  L.Lam{}             -> error "Lam"
  L.App{}             -> error "App"
  L.If{}              -> error "If"
  L.Assert _cond body -> cgenExprR env body

cgenFun :: L.Fun -> String
cgenFun = \case
  L.Fun funId -> case funId of
    L.SFun fun -> case fun of
      "*"     -> "mul_double_double"
      "+"     -> "add_double_double"
      "/"     -> "div_double_double"
      "index" -> "vindex"
      s       -> s
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
  _ -> error "cgenVar"
