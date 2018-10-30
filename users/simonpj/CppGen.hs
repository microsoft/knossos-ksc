{-# LANGUAGE LambdaCase #-}

module CppGen where

import Data.List ( intercalate )
import qualified Lang as L
import qualified Data.HashMap.Strict as Map
import Parse

genDefs :: [L.Def] -> [String]
genDefs defs = 
    map genDef defs

genDef :: L.Def -> String
genDef (L.Def f [] expr) = -- Empty argument list is "non-templated" function
    (typeof expr) ++ " " ++ genFun f ++ "() \n" ++
    "{\n" ++ "  return " ++ genExpr (expr) ++ ";\n}\n"
genDef (L.Def f vars expr) = 
    let vs = map genVar vars in
      "template <" ++ intercalate "," (map ((++) "typename ty$") vs) ++ ">\n" ++
      "auto " ++ genFun f ++ "(" ++ intercalate ", " (map (\v -> "ty$" ++ v ++ " " ++ v) vs) ++ ") -> auto\n" ++
        "{\n" ++ "  return " ++ genExpr (expr) ++ ";\n}\n" ++
      "template <" ++ intercalate "," (map ((++) "typename ty$") vs) ++ ">\n" ++
      "auto " ++ genFun f ++ "(std::tuple<" ++ intercalate ", " (map (\v -> "ty$" ++ v ) vs) ++ "> t) -> auto\n" ++
        "{\n" ++ "  return " ++ genFun f ++ "(" ++ genGets (length vs) ++ ");\n}\n"
      where 
        genGets 1 = "std::get<0>(t)"
        genGets n = let nm1 = n-1 in genGets nm1 ++ ", std::get<" ++ show nm1 ++ ">(t)"
  


genExpr :: L.Expr -> String
genExpr (L.Konst k) = genKonst k
genExpr (L.Var v) = genVar v
genExpr (L.Call f e) = genFun f ++ "(" ++ genExpr e ++ ")" 
genExpr (L.Let v e1 e2) = "[&](auto " ++ genVar v ++ ") { return " ++ genExpr e2 ++ "; }\n" ++
                           "(" ++ genExpr e1 ++ ")"
genExpr (L.Tuple es) = "make_tuple(" ++ intercalate ", " (map genExpr es) ++ ")"
genExpr (L.Lam v2 e) =  -- TODO: int lambdas only for now
                        "std::function<"++ typeof e++"(int)> {[&](int " ++ genVar v2 ++ ")" ++ 
                          "{ return " ++ genExpr e ++ "; }}"
genExpr (L.App f a) = error "App"
genExpr (L.If c t f) = "((" ++ genExpr c ++ ")?(" ++ genExpr t ++ "):(" ++ genExpr f ++ "))"
genExpr (L.Assert e1 e2) = "(ASSERT("++genExpr e1++"), "++genExpr e2++")"

genKonst :: L.Konst -> String
genKonst = \case
  L.KZero      -> "0"
  L.KInteger i -> show i
  L.KFloat   f -> show f
  L.KBool    b -> if b then "TRUE" else "FALSE"

genVar :: L.Var -> String
genVar = \case
  L.Simple s -> s
  L.Delta  d -> "d$" ++ d
  L.Grad g m ->
    "g"
      ++ (case m of
           L.Fwd -> "f"
           L.Rev -> "r"
         )
      ++ "$"
      ++ g

genFun :: L.Fun -> String
genFun = \case
  L.Fun funId -> case funId of
    L.SFun _ fun -> case fun of
      "*" -> "mul"
      "+" -> "add"
      "/" -> "div"
      "-" -> "sub"
      "<" -> "ks_less"
      "==" -> "ks_equals"
      s   -> s
    L.SelFun i n -> "get<(" ++ show i ++ ")>" 

{- TODO typeof properly -}
typeof :: L.Expr -> String
typeof (L.Konst k) = case k of
                        L.KZero      -> "R"
                        L.KInteger i -> "int"
                        L.KFloat   f -> "R"
                        L.KBool    b -> "bool"
typeof (L.Call (L.Fun (L.SFun ty _)) _ = typeof ty 
typeof (L.Var v) = "typeof(" ++ genVar v ++ ")"
typeof (L.Tuple es) = "tuple<" ++ intercalate ", " (map typeof es) ++ ">"
{-
typeof (L.Lam v2 e) =  "std::function<"++ typeof e++"(int)>"
typeof (L.App f a) = error "App"
typeof (L.If c t f) = typeof t
typeof (L.Assert e1 e2) = typeof e2
typeof e = "typeof (" ++ genExpr e ++ ")"
-}
typeof e = "typeof(" ++ genExpr e ++ ")"

