{-# LANGUAGE LambdaCase #-}

module Cgen where

import qualified Data.Map                      as Map
import qualified Lang                          as L
import qualified ANF
import           Control.Monad                  ( (<=<) )
import qualified Control.Monad.State           as S
import           Data.List                      ( intercalate )
import           Data.Maybe                     ( fromJust )
import qualified Main

type M = S.State Int

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

cgenDef :: L.Def -> M String
cgenDef (L.Def f vars expr) = do
  -- FIXME: We don't find the types of function arguments properly.
  -- They should really be attached to the function definition.  For
  -- the moment I'm just fixing the type of function arguments based
  -- on my domain expertise.
  let typeEnvList :: [(L.Var, Type)]
      typeEnvList = case f of
        L.Fun (L.SFun "f3"  ) -> map (\v -> (v, Tuple [Double, Double])) vars
        L.Fun (L.SFun "dot2") -> map (\v -> (v, Vector)) vars
        L.Fun (L.SFun "f8"  ) -> map (\v -> (v, Vector)) vars
        _                     -> map (\v -> (v, Double)) vars

      typeEnv :: Map.Map L.Var Type
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

data Type = Double | Tuple [Type] | Int | Vector deriving Show

cgenType :: Type -> String
cgenType = \case
  Double                 -> "double"
  Tuple [Double, Double] -> "struct tuple2"
  Tuple [Int   , Vector] -> "struct tuple_int_vector"
  Tuple ts -> error ("Don't support that size of tuple: " ++ show ts)
  Int                    -> "int"
  Vector                 -> "struct vector"

cgenExpr :: Map.Map L.Var Type -> L.Expr -> M (String, String, Type)
cgenExpr env = cgenExprR env <=< anf

-- The input expression must be in ANF
cgenExprR :: Map.Map L.Var Type -> L.Expr -> M (String, String, Type)
cgenExprR env = \case
  L.Konst k -> do
    v <- freshCVar
    return
      ( "double " ++ v ++ " = " ++ cgenKonst k ++ ";\n"
      , v
      , Double {- FIXME we need to deal with polymorphism -}
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
      (cbody, cbodyVar, _ty) <- cgenExprR (Map.insert var Int env) body
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

      return (forBody, "vector", Vector)


    _ -> error "Need a lambda for build"

  L.Call f x -> case x of
    L.Var xv -> do
      v <- freshCVar
      let ty = case f of
            L.Fun (L.SFun "+"    ) -> Double
            L.Fun (L.SFun "*"    ) -> Double
            L.Fun (L.SFun "/"    ) -> Double
            L.Fun (L.SFun "size" ) -> Int
            L.Fun (L.SFun "index") -> Double
            L.Fun (L.SFun "sum"  ) -> Double
            L.Fun (L.SFun "neg"  ) -> Double
            L.Fun (L.SFun other  ) -> error ("Call of " ++ other)
            L.Fun L.SelFun{}       -> Double -- FIXME: This is probably not
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

        ty   = Tuple (map (fromJust . flip Map.lookup env) vars)

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

example :: IO ()
example = do
  mapM_
    putStrLn
    [ "#include <stdio.h>"
    , "#include <stdlib.h>"
    , "struct vector { int length; double *data; };"
    , "struct tuple2 { double field1_2; double field2_2; };\n\n"
    , "struct tuple_int_vector { int field1_2; struct vector field2_2; };"
    , "double mul_double_double(struct tuple2 arg) { return arg.field1_2 * arg.field2_2; }"
    , "double add_double_double(struct tuple2 arg) { return arg.field1_2 + arg.field2_2; }"
    , "double div_double_double(struct tuple2 arg) { return arg.field1_2 + arg.field2_2; }"
    , "double selfun_1_2(struct tuple2 arg) { return arg.field1_2; }"
    , "double selfun_2_2(struct tuple2 arg) { return arg.field2_2; }"
    , "int size(struct vector arg) { return arg.length; }"
    , "int vindex(struct tuple_int_vector arg) { return arg.field2_2.data[arg.field1_2]; }"
    , "double neg(double x) { return -x; }"
    , "double sum(struct vector arg) { "
    ++ "double sum = 0;"
    ++ "for (int i = 0; i < arg.length; i++) {"
    ++ "sum += arg.data[i];"
    ++ "}"
    ++ "return sum;"
    ++ "}"
    , "struct vector v1 = { 2, (double []) {2, 3} };"
    , "struct vector v2 = { 2, (double []) {4, 5} };"
    , r Main.ex1
    , r Main.ex2
    , r Main.ex2a
    , r Main.ex3
    , r Main.ex4
    , r Main.ex5
    , r Main.ex7
    , r Main.ex8
    , "int main(void) { "
    ++ printFloat "f1(2)"
    ++ printFloat "f2(2)"
    ++ printFloat "f2a(2)"
    ++ printFloat "f3((struct tuple2) { .field1_2 = 2, .field2_2 = 3 })"
    ++ printFloat "f4(2, 3)"
    ++ printFloat "f5(2, 3)"
    ++ printFloat "dot2(v1, v2)"
    ++ printFloat "f8(v1)"
    ++ "}"
    ]
  where r = runM . cgenDef

printFloat :: String -> String
printFloat s = unlines
  ["printf(\"%s\\n\", \"" ++ s ++ "\");", "printf(\"%f\\n\\n\", " ++ s ++ ");"]
