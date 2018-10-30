{-# LANGUAGE LambdaCase #-}

module Cgen where

import qualified Data.Map                      as Map
import qualified Lang                          as L
import qualified ANF                           as ANF
import           Control.Monad                  ( (>=>)
                                                , (<=<)
                                                )
import qualified Control.Monad.State           as S
import           Data.List                      ( intercalate )
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
  L.Konst k    -> return (L.Konst k)
  L.Var   v    -> return (L.Var v)
  L.Call f arg -> do
    (let_, var) <- do
      anfArg <- anf arg
      v      <- freshVar
      return (L.Let v anfArg, v)
    return (let_ (L.Call f (L.Var var)))
  L.Tuple ts -> do
    anfArgs <- flip mapM ts $ \t -> do
      anft <- anf t
      v    <- freshVar
      return (L.Let v anft, v)
    return
      (foldr (.) id (map fst anfArgs) (L.Tuple (map (L.Var . snd) anfArgs)))
  L.Lam x e -> do
    anfe <- anf e
    return (L.Lam x anfe)
  L.App f x -> do
    anff <- anf f
    anfx <- anf x
    vf   <- freshVar
    vx   <- freshVar
    return (L.Let vf anff (L.Let vx anfx (L.App (L.Var vf) (L.Var vx))))
  L.Let x e body -> do
    anfe    <- anf e
    anfbody <- anf body
    return (L.Let x anfe anfbody)
  L.If cond ift iff -> do
    anfcond <- anf cond
    anfift  <- anf ift
    anfiff  <- anf iff
    return (L.If anfcond anfift anfiff)

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
        L.Fun (L.SFun "f3") -> map (\v -> (v, Tuple 2)) vars
        _                   -> map (\v -> (v, Double)) vars

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

data Type = Double | Tuple Int

cgenType :: Type -> String
cgenType = \case
  Double  -> "double"
  Tuple 2 -> "struct tuple2"
  Tuple _ -> error "Don't support that size of tuple"

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
  L.Call f x -> case x of
    L.Var xv -> do
      v <- freshCVar
      let ty = case f of
            L.Fun (L.SFun "+"  ) -> Double
            L.Fun (L.SFun "*"  ) -> Double
            L.Fun (L.SFun "/"  ) -> Double
            L.Fun (L.SelFun i n) -> Double -- FIXME: This is probably not
                                   -- quite right since an unstated
                                   -- assumption is that SelFuns are
                                   -- polymorphic
            _                    -> error "Call"
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
    _ -> error "Function arguments should be Var in ANF"
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

    return
      ( "struct tuple2 "
      ++ cT
      ++ " = {"
      ++ intercalate "," (map cgenVar vars)
      ++ "};\n"
      , cT
      , Tuple (length ts)
      )

  L.Lam{} -> error "Lam"
  L.App{} -> error "App"
  L.If{}  -> error "If"


cgenFun :: L.Fun -> String
cgenFun = \case
  L.Fun funId -> case funId of
    L.SFun fun -> case fun of
      "*" -> "mul_double_double"
      "+" -> "add_double_double"
      "/" -> "div_double_double"
      s   -> s
    L.SelFun i n -> "selfun_" ++ show i ++ "_" ++ show n

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

example :: IO ()
example = do
  mapM_
    putStrLn
    [ "#include <stdio.h>"
    , "struct tuple2 { double field1_2; double field2_2; };\n\n"
    , "double mul_double_double(struct tuple2 arg) { return arg.field1_2 * arg.field2_2; }"
    , "double add_double_double(struct tuple2 arg) { return arg.field1_2 + arg.field2_2; }"
    , "double div_double_double(struct tuple2 arg) { return arg.field1_2 + arg.field2_2; }"
    , "double selfun_1_2(struct tuple2 arg) { return arg.field1_2; }"
    , "double selfun_2_2(struct tuple2 arg) { return arg.field2_2; }"
    , runM (cgenDef Main.ex1)
    , runM (cgenDef Main.ex2)
    , runM (cgenDef Main.ex2a)
    , runM (cgenDef Main.ex3)
    , runM (cgenDef Main.ex4)
    , runM (cgenDef Main.ex5)
    , "int main(void) { "
    ++ printFloat "f1(2)"
    ++ printFloat "f2(2)"
    ++ printFloat "f2a(2)"
    ++ printFloat "f3((struct tuple2) { .field1_2 = 2, .field2_2 = 3 })"
    ++ printFloat "f4(2, 3)"
    ++ printFloat "f5(2, 3)"
    ++ "}"
    ]

printFloat :: String -> String
printFloat s = "printf(\"%f\\n\", " ++ s ++ ");\n"
