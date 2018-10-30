{-# LANGUAGE LambdaCase #-}

module Cgen where

import Debug.Trace (trace)
import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map
import           Control.Monad                  ( (>=>)
                                                , (<=<)
                                                )
import qualified Control.Monad.State           as S
import qualified ANF                           as ANF
import Lang

type M = S.State Int

spc:: String -> String -> String
spc x y = x ++ " " ++ y

assertEqualThen t1 t2 =
  if t1 == t2 then t1 else error ("Asserts unequal " ++ show t1 ++ " == " ++ show t2)

type ST = Map.Map Var Type
stInsert:: Var -> Type -> ST -> ST
stInsert (TVar ty v) t env = stInsert v (assertEqualThen t ty) env
stInsert v t env = Map.insert v t env

stLookup:: Var -> ST -> Type
stLookup (TVar ty v) env = assertEqualThen ty (stLookup v env)
stLookup v env = 
  case Map.lookup v env of
    Just a  -> a
    Nothing -> error ("Couldn't find " ++ show v ++ " in " ++ show env)

runM :: M a -> a
runM = flip S.evalState 0

freshCVar :: M String
freshCVar = do
  s <- S.get
  S.put (s + 1)
  return ("c" ++ show s)

freshVar :: M Var
freshVar = do
  -- This doesn't communicate very well but is quick (and dirty)
  s <- freshCVar
  return (Simple s)

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

cgenDefs :: [Def] -> [String]
cgenDefs defs = map (runM <$> cgenDefM) defs  

cgenDef :: Def -> String
cgenDef = runM <$> cgenDefM  

cgenDefM :: Def -> M String
cgenDefM (Def f vars expr) = do
  let env = foldr addVarToEnv Map.empty vars
        where addVarToEnv :: Var -> ST -> ST
              addVarToEnv (TVar ty v) env = stInsert v ty env
              addVarToEnv v _ = error $ "Untyped parameter [" ++ show v ++ "] in def " ++ show f 
  
  (cExpr, cVar, cType) <- cgenExpr env expr

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
    )

-- CGenResult is (C declaration, C expression, Type)
-- e.g. ("double r; if (b) { r = 1; } else { r = 2; };",
--       "r",
--       TypeFloat)
-- e.g. ("",         -- simple constant needs no pre-declaration
--       "1.0",      -- this is what we use at the occurrence
--       TypeFloat)  -- and this is the type of the occurrence
type CGenResult = (String, String, Type)

cgenExpr :: ST -> Expr -> M CGenResult
cgenExpr env  = cgenExprR env <=< anf

typeof :: ST -> Expr -> Type 
typeof env expr = case expr of
  Konst (KZero     ) -> TypeZero
  Konst (KInteger i) -> TypeInteger
  Konst (KFloat   f) -> TypeFloat
  Konst (KBool    b) -> TypeBool
  Var v -> stLookup v env
  Call f (Var xv) -> case stLookup xv env of
                     TypeTuple ts -> typeofFun (show (ppr expr)) f ts
                     t -> typeofFun (show (ppr expr)) f [t]
  _ -> error "typeof"

typeofFun msg f ts =
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

    let unVar :: Expr -> (Var, Type)
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

cgenType :: Type -> String
cgenType = \case
  TypeInteger  -> "int"
  TypeBool  -> "bool"
  TypeFloat  -> "double"
  TypeUnknown  -> "auto"
  TypeTuple ts -> "std::tuple<" ++ intercalate "," (map cgenType ts) ++ ">"
  TypeVec t -> "vec<" ++ (cgenType t) ++ ">"
  TypeLambda from to -> "std::function<" ++ cgenType to ++ "(" ++ cgenType from ++ ")>"

