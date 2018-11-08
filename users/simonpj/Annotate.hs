{-# LANGUAGE LambdaCase #-}

module Annotate where

import           Debug.Trace                    ( trace )
import qualified Data.Map                      as Map

import           Lang

dbtrace :: String -> a -> a
dbtrace _ e = e -- trace msg e

------------------ ST (SymTab) ------------------

type ST = Map.Map Var Type

stCreate :: ST
stCreate = Map.empty

stInsert :: Var -> Type -> ST -> ST
stInsert v ty env = dbtrace
  ("Inserting " ++ show v ++ " = " ++ show ty ++ " in " ++ show env ++ "\n")
  (Map.insert v ty env)

stInsertFun :: Fun -> Type -> ST -> ST
stInsertFun f = stInsert (Simple (show $ ppr f))

stLookup :: String -> Var -> ST -> Type
stLookup msg v env = case Map.lookup v env of
  Just a  -> a
  Nothing -> trace
    ("Couldn't find " ++ show v ++ " in " ++ msg ++ ", env = " ++ show env)
    TypeUnknown

stLookupFun :: String -> Fun -> ST -> Type
stLookupFun msg (Fun (SFun v)) = stLookup msg (Simple v)

--------------------------

annotDef :: Def -> Def
annotDef = annotDefE stCreate

annotDefs :: [Def] -> [Def]
annotDefs defs =
  let env = stCreate in reverse $ snd $ foldl accum (env, []) defs
 where
  accum :: (ST, [Def]) -> Def -> (ST, [Def])
  accum (env, tdefs) def =
    let tdef@(Def (TFun tf f) _ _) = annotDefE env $ dbg env def
    in  (stInsertFun f tf env, tdef : tdefs)

  dbg :: ST -> Def -> Def
  dbg env def = -- returns def
                dbtrace
    ("Passing ENV[" ++ show env ++ "]" ++ " to " ++ (show $ ppr def) ++ "\n")
    def

annotDefE :: ST -> Def -> Def
annotDefE env (Def (TFun _ f) vars expr) =
  let _ = trace ("Def " ++ show f ++ "\n") ()
  in  let body_env = foldr addVarToEnv env vars
      in  let te@(Expr ty _) = annotExpr body_env expr
          in  Def (TFun ty f) vars te
 where
  addVarToEnv :: TVar -> ST -> ST
  addVarToEnv (TVar ty v) = stInsert v ty

annotExpr :: ST -> Expr -> Expr
annotExpr env (Expr _ ex) = annotExprX env ex

chUnk :: Type -> Type -> a -> a
chUnk TypeUnknown _  val = val
chUnk ty0         ty val = if ty0 == ty
  then trace "Types matched" val
  else error ("Declared types mismatched: " ++ show ty0 ++ " /= " ++ show ty)

checkLookup :: String -> Type -> Var -> ST -> Type
checkLookup msg ty0 v env = let ty = stLookup msg v env in chUnk ty0 ty ty

annotExprX :: ST -> ExprX -> Expr
annotExprX env = \case
  -- Naming conventions in this function:
  --  e   Expr [TypeUnkown]
  --  ae  Expr [Annotated]
  --  tye Type of e
  --  xe  ExprX part of e
  Var   v -> Expr (stLookup "Var" v env) $ Var v

  Konst k -> Expr (typeofKonst k) $ Konst k

  Call (TFun ty0 f) es ->
    let aes@(Expr tyes _) = annotExpr env es
    in  let stty = stLookupFun "Call" f env
        in  let ty = typeofFun env f tyes
            in  let _ = chUnk ty0 ty ()
                in  let _ = chUnk stty ty () in Expr ty (Call (TFun ty f) aes)

  Let (TVar ty0 v) e1 body ->
    let ae1@(Expr tyv _) = annotExpr env e1
    in  let body_env = stInsert v tyv env
        in  let abody@(Expr tybody _) = annotExpr body_env body
            in  let _ = chUnk ty0 tyv
                in  Expr tybody $ Let (TVar tyv v) ae1 abody

  Tuple es ->
    let aes = map (annotExpr env) es
    in  Expr (TypeTuple $ map typeOf aes) (Tuple aes)

  Lam av@(TVar tyv v) body ->
    let body_env = stInsert v tyv env
    in  let abody@(Expr tybody _) = annotExpr body_env body
        in  Expr (TypeLambda tyv tybody) $ Lam av abody

  If cond texpr fexpr ->
    let acond@(Expr tycond _) = annotExpr env cond
    in  let atexpr@(Expr tytexpr _) = annotExpr env texpr
        in  let afexpr@(Expr tyfexpr _) = annotExpr env fexpr
            in  let _ = assertEqual "tycond" tycond TypeBool
                in  let _ = assertEqual "ttexpr" tytexpr tyfexpr
                    in  Expr tytexpr $ If acond atexpr afexpr

  Assert cond body ->
    let acond@(Expr tycond _) = annotExpr env cond
    in  let abody@(Expr tybody _) = annotExpr env body
        in  let _ = assertEqual "Assert" tycond TypeBool
            in  Expr tybody $ Assert acond abody

  e -> error $ "Cannot annotate " ++ (show $ ppr e)


-- A single place for "domain knowledge" about polymorphic functions -- to be pruned when we get primdefs
typeofFun :: ST -> Fun -> Type -> Type
typeofFun env f (TypeTuple tys) = typeofFunTys env f tys
typeofFun env f ty              = typeofFunTys env f [ty]

typeofFunTys :: ST -> Fun -> [Type] -> Type
typeofFunTys env tf ttys = case (tf, ttys) of
  (Fun (SFun "pr")       , _                            ) -> TypeUnknown
  (GradFun (SFun "pr") _ , _                            ) -> TypeUnknown
  (Fun (SFun "build")    , [_, TypeLambda TypeInteger t]) -> TypeVec t
  (Fun (SFun "index")    , [_, TypeVec t]               ) -> t
  (Fun (SFun "size" )    , [TypeVec _]                  ) -> TypeInteger
  (Fun (SFun "sum"  )    , [TypeVec t]                  ) -> t

  (Fun (SFun "exp"  )    , [TypeFloat]                  ) -> TypeFloat
  (Fun (SFun "log"  )    , [TypeFloat]                  ) -> TypeFloat
  (Fun (SFun "+"    )    , t : _                        ) -> t
  (Fun (SFun "/"    )    , t : _                        ) -> t
  (Fun (SFun "*"    )    , t : _                        ) -> t
  (GradFun (SFun "*") Fwd, t : tys) -> TypeLM (TypeTuple (t : tys)) t
  (Fun (SFun "-"    )    , t : _                        ) -> t

  (Fun (SFun "=="   )    , _                            ) -> TypeBool
  (Fun (SFun "!="   )    , _                            ) -> TypeBool
  (Fun (SFun "<"    )    , _                            ) -> TypeBool
  (Fun (SFun ">"    )    , _                            ) -> TypeBool

  (Fun (SFun "delta")    , [TypeInteger, TypeInteger, t]) -> t

  (Fun (SelFun i _  )    , [TypeTuple tys]              ) -> tys !! (i - 1)
  (GradFun (SelFun i _) _, [TypeTuple tys]) ->
    TypeLM (TypeTuple tys) (tys !! (i - 1))

  (Fun (SelFun{})      , [TypeVec t]) -> t
  (GradFun (SelFun{}) _, [TypeVec t]) -> TypeLM (TypeVec t) t
  (GradFun (SelFun{}) _, _          ) -> TypeUnknown

  (Fun (SFun f)        , _          ) -> case Map.lookup (Simple f) env of
    Just a  -> a
    Nothing -> trace
      (  "Failed to type fun ["
      ++ show f
      ++ "], types ["
      ++ show ttys
      ++ "], env"
      ++ show env
      )
      TypeUnknown

  (LMFun f, tys) -> typeofLMFun f tys
  (GradFun (SFun f) _, [tfrom]) ->
    let ty = stLookup "GradFun" (Simple f) env in TypeLM tfrom ty
  (GradFun (SFun f) _, t : tys) ->
    let tfrom = TypeTuple (t : tys)
    in  let ty = stLookup "GradFun2" (Simple f) env in TypeLM tfrom ty
  _ ->
    let emsg =
          "EFailed to type ("
            ++ show tf
            ++ ", "
            ++ show ttys
            ++ "), env"
            ++ show env
    in  trace (error emsg) TypeUnknown

typeofLMFun :: String -> [Type] -> Type
typeofLMFun f tys = case (f, tys) of
  ("lmOne"  , [t]   ) -> TypeLM t t
  ("lmZero" , [a, b]) -> TypeLM a b
  ("lmScale", [t]   ) -> TypeLM t t
  ("lmBuild", [TypeInteger, TypeLambda TypeInteger (TypeLM a b)]) ->
    TypeLM a (TypeVec b)
  ("lmBuildT", [TypeInteger, TypeLambda TypeInteger (TypeLM a b)]) ->
    TypeLM (TypeVec a) b
  ("lmVCat", [TypeLM a b, TypeLM a1 c]) ->
    assertEqualThen "lmVCat" a a1 $ TypeLM a (TypeTuple [b, c])
  ("lmHCat", [TypeLM a c, TypeLM b c1]) ->
    assertEqualThen "lmHCat" c c1 $ TypeLM (TypeTuple [a, b]) c
  ("lmCompose", [TypeLM b c, TypeLM a b1]) ->
    assertEqualThen "lmCompose" b b1 $ TypeLM a c
  ("lmApply", [TypeLM a b, c]) -> assertEqualThen
    ("lmApply LM " ++ show a ++ " -o " ++ show b ++ " * " ++ show c)
    a
    c
    b
  _ ->
    flip trace TypeUnknown
      $  "Failed to type LMfun ("
      ++ show f
      ++ ", "
      ++ show tys
      ++ ") -> ?"

