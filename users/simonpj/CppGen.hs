{-# LANGUAGE LambdaCase #-}

module CppGen where

import Data.List ( intercalate )
import qualified Lang as L
import qualified Main
import Parse

cppF :: String -> IO ()
-- String is the file name
cppF file
  = do  
        cts <- readFile file
        ;
        let lls = case runParser pDefs cts of
                    Left err   -> error ("Failed parse: " ++ show err)
                    Right defs -> map genDef defs
        
        let lines = [ 
                      "#include <stdio.h>"
                      , "#include \"knossos.h\""
                    ]
        writeFile "tmp1.cpp" (intercalate "\n" (lines ++ lls))
        readFile "tmp1.cpp" >>= putStrLn;
        putStrLn "^^^^^^---- Written to tmp1.cpp ----^^^^^^^^^"

        


genDef :: L.Def -> String
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
  L.StopGrad s -> s
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
      "<" -> "ks_less"
      "==" -> "ks_equals"
      s   -> s
    L.SelFun i n -> "get<(" ++ show i ++ ")>" 

{- TODO typeof properly -}
typeof :: L.Expr -> String
typeof (L.Konst k) = case k of
                        L.KZero      -> "double"
                        L.KInteger i -> "int"
                        L.KFloat   f -> "double"
                        L.KBool    b -> "bool"
typeof (L.Call (L.Fun (L.SFun "build")) (L.Tuple [n, L.Lam i b])) = "vec<" ++ typeof b ++ ">" 
typeof (L.Var v) = "typeof(" ++ genVar v ++ ")"
{-
typeof (L.Tuple es) = "tuple<" ++ intercalate ", " (map typeof es) ++ ">"
typeof (L.Lam v2 e) =  "std::function<"++ typeof e++"(int)>"
typeof (L.App f a) = error "App"
typeof (L.If c t f) = typeof t
typeof (L.Assert e1 e2) = typeof e2
typeof e = "typeof (" ++ genExpr e ++ ")"
-}
typeof _ = "double"

example = do
    let lines = [ 
                  "#include <stdio.h>"
                  , "#include \"knossos.h\""
                  , "double v_data [] = {1.1, 2.2, 3.3};"
                  , "vec<double> v { 3, v_data };"
                  ,  (genDef Main.ex1)
                  ,  (genDef Main.ex2)
                  ,  (genDef Main.ex2a) -- the definition of y seems to be missing
                  ,  (genDef Main.ex4)
                  ,  (genDef Main.ex8)
                  , "int main(void) {\n"
                  ++ printFloat "f1(2)"
                  ++ printFloat "f8(v)"
                  ++ "}"
                ]
    writeFile "tmp1.cpp" (intercalate "\n" lines)
    readFile "tmp1.cpp" >>= putStrLn
    putStrLn "^^^^^^---- Written to tmp1.cpp ----^^^^^^^^^"

printFloat s = "printf(\"%f\\n\", " ++ s ++ ");\n"
