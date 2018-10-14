{-# LANGUAGE LambdaCase #-}

module CppGen where

import           Data.List                      ( intercalate )
import qualified Lang                          as L
import qualified Main

genDef (L.Def f vars expr) = 
          let vs = map genVar vars in
          "template <" ++ intercalate "," (map ((++) "typename ty$") vs) ++ ">\n" ++
          "auto " ++ genFun f ++ "(" ++ intercalate ", " (map (\v -> "ty$" ++ v ++ " " ++ v) vs) ++ ") -> auto\n" ++
            "{\n" ++ "  return " ++ genExpr (expr) ++ ";\n}\n"

genExpr :: L.Expr -> String
genExpr (L.Konst k) = genKonst k
genExpr (L.Var v) = genVar v
genExpr (L.Call f e) = genFun f ++ "(" ++ genExpr e ++ ")" 
genExpr (L.Let v e1 e2) = "[&](auto " ++ genVar v ++ ") { return " ++ genExpr e2 ++ "; }\n" ++
                           "(" ++ genExpr e1 ++ ")"
genExpr (L.Tuple es) = "make_tuple(" ++ intercalate ", " (map genExpr es) ++ ")"
genExpr (L.Lam v2 e) = error "Lam"
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
    L.SFun fun -> case fun of
      "*" -> "mul"
      "+" -> "add"
      "/" -> "div"
      s   -> s
    L.SelFun i n -> "get<(" ++ show i ++ ")>" 


example = do
    let lines = [ 
                  "#include <stdio.h>"
                  , "#include <tuple>"
                  , "using std::make_tuple;"
                  , "using std::tuple;"
                  , "using std::get;"
                  , "double mul(tuple<double,double> arg) { return get<0>(arg) * get<1>(arg); }"
                  , "double add(tuple<double,double> arg) { return get<0>(arg) + get<1>(arg); }"
                  , "double div(tuple<double,double> arg) { return get<0>(arg) / get<1>(arg); }"
                  ,  (genDef Main.ex1)
                  ,  (genDef Main.ex2)
                  ,  (genDef Main.ex2a) -- the definition of y seems to be missing
                  ,  (genDef Main.ex4)
                  ,  (genDef Main.ex5)
                  , "int main(void) {\n"
                  ++ printFloat "f1(2)"
                  ++ printFloat "f5(2.0,3.0)"
                  ++ "}"
                ]
    writeFile "tmp1.cpp" (intercalate "\n" lines)
    readFile "tmp1.cpp" >>= putStrLn

printFloat s = "printf(\"%f\\n\", " ++ s ++ ");\n"
