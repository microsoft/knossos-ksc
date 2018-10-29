{-# LANGUAGE LambdaCase #-}

module Cgen where

import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map
import           Control.Monad                  ( (>=>)
                                                , (<=<)
                                                )
import qualified Control.Monad.State           as S
import qualified ANF                           as ANF
import Lang

type M = S.State Int

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
  let typeEnv = Map.fromList (map (\v -> (v, TypeFloat)) vars) -- FIXME: do this properly!

  (cExpr, cVar, _) <- cgenExpr typeEnv expr

  return
    (  "double "
    ++ cgenFun f
    ++ "("
    ++ intercalate ", " (map (("double " ++) . cgenVar) vars)
    ++ ") {\n"
    ++ cExpr
    ++ "return "
    ++ cVar
    ++ ";\n}\n"
    )

cgenType :: Type -> String
cgenType = \case
  TypeFloat  -> "double"
  TypeTuple ts -> "std::tuple<" ++ intercalate "," (map cgenType ts) ++ ">"

cgenExpr :: Map.Map Var Type -> Expr -> M (String, String, Type)
cgenExpr env = cgenExprR env <=< anf

-- The input expression must be in ANF
cgenExprR :: Map.Map Var Type -> Expr -> M (String, String, Type)
cgenExprR env = \case
  Konst k -> do
    v <- freshCVar
    return
      ( "double " ++ v ++ " = " ++ cgenKonst k ++ ";\n"
      , v
      , TypeFloat {- FIXME we need to deal with polymorphism -}
      )
  Var v -> return
    ( ""
    , cgenVar v
    , case Map.lookup v env of
      Just a  -> a
      Nothing -> error ("Couldn't find " ++ show v)
    )
  Call f x -> case x of
    Var xv -> do
      v <- freshCVar
      let ty = case f of
            Fun (SFun "+"  ) -> TypeFloat
            Fun (SFun "*"  ) -> TypeFloat
            Fun (SFun "/"  ) -> TypeFloat
            Fun (SelFun i n) -> TypeFloat -- FIXME: This is probably not
                                   -- quite right since an unstated
                                   -- assumption is that SelFuns are
                                   -- polymorphic
            _                    -> error ("Call " ++ show f)
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
  Let v e1 e2 -> do
    (cE1, vE1, t1) <- cgenExprR env e1
    (cE2, vE2, t2) <- cgenExprR (Map.insert v t1 env) e2
    return
      ( cE1 ++ cgenType t1 ++ " " ++ cgenVar v ++ " = " ++ vE1 ++ ";\n" ++ cE2
      , vE2
      , t2
      )

  Lam v body -> do
    l <- freshCVar
    (cE, vE, t) <- cgenExprR env body
    let tv = case Map.lookup v env of
                Just a  -> a
                Nothing -> error ("Couldn't find " ++ show v)
    return (
      "auto " ++ l ++ "[](" ++ cgenType tv ++ " " ++ cgenVar v ++ ") { return " ++ cE ++ "; }"
      , l
      , TypeUnknown
      )

  Tuple ts -> do
    cT <- freshCVar

    let unVar :: Expr -> Var
        unVar = \case
          Var v -> v
          _       -> error "Tuple: Expected arguments to be Vars"

        vars = map unVar ts

    return
      ( cgenType (TypeTuple []) -- FIXME
      ++ cT
      ++ " = {"
      ++ intercalate "," (map cgenVar vars)
      ++ "};\n"
      , cT
      , TypeTuple []-- FIXME
      )

  App{} -> error "App"
  If{}  -> error "If"


cgenFun :: Fun -> String
cgenFun = \case
  Fun funId -> case funId of
    SFun fun -> case fun of
      "*" -> "mul_double_double"
      "+" -> "add_double_double"
      "/" -> "div_double_double"
      s   -> s
    SelFun i n -> "selfun_" ++ show i ++ "_" ++ show n

cgenKonst :: Konst -> String
cgenKonst = \case
  KZero      -> "0"
  KInteger i -> show i
  KFloat   f -> show f
  KBool    b -> if b then "TRUE" else "FALSE"

cgenVar :: Var -> String
cgenVar = \case
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
