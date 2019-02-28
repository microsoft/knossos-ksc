-- Clean documentation of the Knossos IR

-- Top-level function definition
-- f x = e
-- All functions are unary, multiple arguments passed as tuples.
-- The generating language can sugar up the tupling/detupling
data Def   
  = Def Id Var Expr 

data Expr
  = Konst              -- Various constants, e.g. Real/Int/String literals
  | Var Id             -- Variable reference
  | Tuple [Expr]       -- Built-in tuples, with all functions unary 
  | Lam Var Expr
  | Call Id Expr       -- Top-level functions only, like C.  Closures constructed manually.
  | Let Var Expr Expr  -- let x = e1 in e2  (non-recursive)
  | If Expr Expr Expr  

