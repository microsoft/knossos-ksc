{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Lang where

import Prelude hiding( (<>) )

import Data.List ( intercalate )
import qualified Data.Map as Map
import Text.PrettyPrint   as PP
import KMonad

import Data.Functor.Identity
import Data.Maybe
import Data.Functor
import Data.Either( partitionEithers )

import System.Console.Haskeline

import qualified Data.Map as M
import Debug.Trace( trace )
import Test.Hspec

-----------------------------------------------
--  The main data types
-----------------------------------------------

data DeclX f b = RuleDecl (RuleX f b)
               | DefDecl  (DefX f b)

type Decl  = DeclX Fun  Var
type TDecl = DeclX TFun TVar

data DefX f b = DefX f [TVar] (ExprX f b)  -- f x = e
              deriving( Show )

type Def  = DefX Fun Var
type TDef = DefX TFun TVar

data ExprX f b
  = Konst Konst
  | Var b
  | Call f (ExprX f b)      -- f e
  | Tuple [ExprX f b]       -- (e1, ..., en)
  | Lam TVar (ExprX f b)    -- Lambda-bound variable is typed from birth
  | App (ExprX f b) (ExprX f b)
  | Let b (ExprX f b) (ExprX f b)    -- let x = e1 in e2  (non-recursive)
  | If (ExprX f b) (ExprX f b) (ExprX f b)  -- FIXME make cond ExprX?
  | Assert (ExprX f b) (ExprX f b)
  deriving( Show )

type Expr  = ExprX Fun Var
type TExpr = ExprX TFun TVar

data Type = TypeZero Type
          | TypeBool
          | TypeInteger
          | TypeFloat
          | TypeTuple [Type]
          | TypeVec Type
          | TypeLambda Type Type   -- Domain -> Range
          | TypeLM Type Type       -- Linear map  Src -o Target
          | TypeUnknown
          deriving (Show, Eq, Ord)

type PrimFun = String

data FunId = UserFun String
           | PrimFun PrimFun
           | SelFun
                Int      -- Index; 1-indexed, so (SelFun 1 2) is fst
                Int      -- Arity
           deriving( Eq, Ord )

data Fun = Fun     FunId         -- The function              f(x)
         | GradFun FunId ADMode  -- Full Jacobian Df(x)
                                 --   Rev <=> transposed  Rf(x)
         | DrvFun  FunId ADMode  -- Derivative derivative f'(x,dx)
                                 --   Rev <=> reverse mode f`(x,dr)
         deriving( Eq, Ord )

data ADMode = Fwd | Rev
            deriving( Eq, Ord, Show )

data TFun = TFun Type Fun  -- Typed functions.  These are used at
  deriving (Eq, Ord)       -- /occurrence/ sites only, not binding site

data Var
  = Dummy
  | Simple   String         -- x
  | Delta    String         -- The 'dx' or 'dr' argument to fwd
                            -- or backward versions of f
  | Grad     String ADMode  -- \nabla x
                           --   True <=> transposed \bowtie x
  deriving( Eq, Ord )

data TVar = TVar Type Var
  deriving( Show, Eq, Ord )

data Konst = KZero Type
           | KInteger Integer
           | KFloat   Double
           | KBool    Bool
           deriving( Eq, Ord, Show )

data RuleX f b = Rule { ru_name  :: String   -- Just for logging
                      , ru_qvars :: [TVar]
                      , ru_lhs   :: ExprX f b
                      , ru_rhs   :: ExprX f b }
  -- When matching may bind any of the ru_qvars, which are typed,
  -- just like any other lambda-bound variable

type Rule  = RuleX Fun Var
type TRule = RuleX TFun TVar

-----------------------------------------------
--  Simple functions over these types
-----------------------------------------------

flipMode :: ADMode -> ADMode
flipMode Fwd = Rev
flipMode Rev = Fwd

isZero :: Type -> Bool
isZero (TypeZero _) = True
isZero _ = False

isKZero :: TExpr -> Bool
isKZero = \case
    Konst (KZero _) -> True
    Konst (KInteger 0) -> True
    Konst (KFloat 0.0) -> True
    e -> isZero (typeof e)

partitionDecls :: [DeclX f b] -> ([RuleX f b], [DefX f b])
-- Separate the Rules from the Defs
partitionDecls decls
  = partitionEithers $
    map (\case { RuleDecl r -> Left r; DefDecl d -> Right d }) $
    decls

-----------------------------------------------
--       Building values
-----------------------------------------------

mkPrimFun :: String -> Fun
mkPrimFun fname = Fun (PrimFun fname)

mkPrimTFun :: Type -> String -> TFun
mkPrimTFun ty fname = TFun ty $ mkPrimFun fname

mkVar :: String -> Var  -- Just a Simple var
mkVar s = Simple s

mkTVar :: Type -> String -> TVar
mkTVar ty s = TVar ty (mkVar s)

mkDummy :: Type -> TVar
mkDummy ty = TVar ty Dummy

mkLet :: HasCallStack => TVar -> TExpr -> TExpr -> TExpr
mkLet (TVar ty v) rhs body
  = assertEqualThen ("mkLet " ++ show v ++ " = " ++ pps rhs) ty (typeof rhs) $
    Let (TVar ty v) rhs body

mkLets :: HasCallStack => [(TVar,TExpr)] -> TExpr -> TExpr
mkLets [] e = e
mkLets ((v,rhs):bs) e = mkLet v rhs (mkLets bs e)

kInt :: Integer -> ExprX f b
kInt i = Konst (KInteger i)

kTInt :: Integer -> TExpr
kTInt i = Konst (KInteger i)

kFloat :: Double -> Expr
kFloat f = Konst (KFloat f)

kTFloat :: Double -> TExpr
kTFloat f = Konst (KFloat f)

mkTuple :: [ExprX f b] -> ExprX f b
mkTuple [e] = e
mkTuple es = Tuple es

-----------------------------------------------
--     Building types
-----------------------------------------------

mkTypeTuple :: [Type] -> Type
mkTypeTuple [ty] = ty
mkTypeTuple tys = TypeTuple tys


-----------------------------------------------
--  Finding the type of an expression
-----------------------------------------------

class HasType b where
  typeof :: b -> Type

instance HasType TVar where
  typeof (TVar ty _) = ty

instance HasType TFun where
  typeof (TFun ty _) = ty

instance (HasType b, HasType f,
          HasInfix f, Pretty f, Pretty b)
      =>  HasType (ExprX f b) where
  typeof (Konst k)     = typeofKonst k
  typeof (Var b)       = typeof b
  typeof (Call f e)    = typeof f
  typeof e@(App f _)   = case typeof f of
                            TypeLambda _ res -> res
                            _ -> pprPanic "typeof:app " (ppr f $$ ppr (typeof f))
  typeof (Tuple es)    = TypeTuple $ map typeof es
  typeof (Lam b e)     = TypeLambda (typeof b) (typeof e)
  typeof (Let b e1 e2) = typeof e2
  typeof (Assert c e)  = typeof e
  typeof (If c t f)    = makeIfType (typeof t) (typeof f)

-- ToDo:
-- The type of an If statement is a sort of union of the types on the branches
-- This occurs because of types like TypeZero which intersect both Float and Integer
makeIfType :: HasCallStack => Type -> Type -> Type
makeIfType (TypeZero ty1) ty2 = makeIfType ty1 ty2
makeIfType ty1 (TypeZero ty2) = makeIfType ty1 ty2
makeIfType ty1 ty2 = assertEqualThen "makeIfType" ty1 ty2 $
                     ty2

getLM :: HasCallStack => Type -> Type
getLM (TypeLM s t) = TypeLM s t
getLM t = error $ "Wanted TypeLM, got " ++ pps t

unzipLMTypes :: HasCallStack => [Type] -> ([Type], [Type])
unzipLMTypes [] = ([], [])
unzipLMTypes (TypeLM s t : lmts) = case unzipLMTypes lmts of
                                     (ss, ts) -> (s:ss, t:ts)
unzipLMTypes lmts = pprPanic "unzipLMTypes" (ppr lmts)

typeofKonst :: Konst -> Type
typeofKonst (KZero t) = TypeZero t
typeofKonst (KInteger _) = TypeInteger
typeofKonst (KFloat _) = TypeFloat
typeofKonst (KBool _) = TypeBool

-----------------------------------------------
--     Debugging utilities
-----------------------------------------------

assert :: HasCallStack => Doc -> Bool -> b -> b
assert doc True  x = x
assert doc False x = error (show doc)

assertBool :: Bool -> Bool
assertBool x = x    -- To remove check, return True always

assertEqual msg t1 t2 =
  assertEqualThen msg t1 t2 ()

assertTypesEqualThen :: HasCallStack => String -> Type -> Type -> b -> b
assertTypesEqualThen msg t1 (TypeZero t2) e = assertTypesEqualThen msg t1 t2 e
assertTypesEqualThen msg (TypeZero t1) t2 e = assertTypesEqualThen msg t1 t2 e
assertTypesEqualThen msg t1 t2 e =
  if t1 == t2 then
    e
  else
    error ("Asserts unequal ["++msg++"] \n T1 = " ++ show t1 ++ "\n T2 = " ++ show t2 ++ "\n") $
    e

assertEqualThen :: (HasCallStack, Eq a, Show a) => String -> a -> a -> b -> b
assertEqualThen msg t1 t2 e =
  if t1 == t2 then e else error ("Asserts unequal ["++msg++"] \n T1 = " ++ show t1 ++ "\n T2 = " ++ show t2 ++ "\n") $ e

assertAllEqualThen :: (HasCallStack, Eq a, Show a) => String -> [a] -> b -> b
assertAllEqualThen msg es e =
  if allEq es then e else
     flip trace e $ ("Assert failed: ["++msg++"] not all equal  \n " ++ show es ++ "\n")
  where
    allEq [] = True
    allEq (a:as) = allEqa a as

    allEqa a0 [] = True
    allEqa a0 [a] = a0 == a
    allEqa a0 (a:as) = a0 == a && allEqa a0 as

assertAllEqualRet :: (HasCallStack, Eq a, Show a) => String -> [a] -> a
assertAllEqualRet msg (e:es) = assertAllEqualThen msg (e:es) e



-----------------------------------------------
--       Show instances
-----------------------------------------------

instance Show FunId where
  show (UserFun s)  = s
  show (PrimFun p)  = p
  show (SelFun i n) = "selfun<" ++ show i ++ "," ++ show n ++ ">"

instance Show Fun where
  show (Fun s) = show s
  show (GradFun s Fwd) = "D$" ++ show s
  show (GradFun s Rev) = "R$" ++ show s
  show (DrvFun  s Fwd) = "fwd$" ++ show s
  show (DrvFun  s Rev) = "rev$" ++ show s

instance Show TFun where
  show (TFun ty f) = "(TFun " ++ show f ++ " : " ++ show ty ++ ")"

instance Show Var where
  show v = case v of
    Dummy -> "/*dummy*/"
    Simple s -> "s$" ++ s
    Delta  d -> "d$" ++ d
    Grad g m ->
      "g"
      ++ (case m of
            Fwd -> "f"
            Rev -> "r")
      ++ "$"
      ++ g


-----------------------------------------------
--     Pretty printer for the KS langauge
-----------------------------------------------

class Pretty p where
  ppr     :: p -> Doc
  ppr = pprPrec precZero

  pprPrec :: Prec -> p -> Doc
  pprPrec _ = ppr

instance Pretty Char where
  ppr s = char s

instance Pretty Bool where
  ppr True  = text "True"
  ppr False = text "False"

instance Pretty Var where
  ppr v   = PP.text $ show v

instance Pretty FunId where
  ppr f = text (show f)

instance Pretty Fun where
  ppr f = text $ show f -- need show and ppr consistent for debugging

instance Pretty TVar where
  pprPrec p (TVar ty Dummy) = ppr ty -- For dummy vars, print the type
  pprPrec p (TVar ty v) = ppr v

instance Pretty TFun where
  ppr (TFun ty f) = ppr f

instance Pretty Konst where
  ppr (KInteger i) = PP.integer i
  ppr (KFloat f)   = PP.double f
  ppr (KZero t)    = text "(KZero " <> ppr t <> char ')'

instance Pretty Type where
  ppr (TypeVec ty) = PP.text "(Vec " PP.<> ppr ty PP.<> PP.text ")"
  ppr (TypeTuple tys) = PP.text "(Tuple (" PP.<> pprWithCommas ppr tys PP.<> PP.text "))"
  ppr (TypeLambda from to) = PP.text "(Lambda " PP.<> ppr from PP.<> PP.text " -> " PP.<> ppr to PP.<> PP.text ")"
  ppr (TypeLM s t) = PP.text "(LM " PP.<> ppr s PP.<> PP.char ' ' PP.<> ppr t PP.<> PP.text ")"
  ppr (TypeZero t) = text "zero_t@" <> ppr t
  ppr TypeFloat = PP.text "Float"
  ppr TypeInteger = PP.text "Integer"
  ppr TypeBool = PP.text "Bool"
  ppr TypeUnknown = PP.text "UNKNOWN"


type Prec = Int
 -- 0 => no need for parens
 -- high => parenthesise everything

precZero  = 0  -- Base
precOne   = 1  -- ==
precTwo   = 2  -- +
precThree = 3  -- *

instance (HasInfix f, Pretty f, Pretty b) => Pretty (ExprX f b) where
  ppr expr = pprExpr 0 expr

pprParendExpr :: (HasInfix f, Pretty f, Pretty b) =>
                ExprX f b -> Doc
pprParendExpr = pprExpr precTwo

pprTVar :: TVar -> Doc
pprTVar (TVar ty v) = ppr v <> PP.text ":" <> ppr ty

pprExpr :: (HasInfix f, Pretty f, Pretty b) => Prec -> ExprX f b -> Doc
pprExpr _  (Var v)   = ppr v
pprExpr _ (Konst k)  = ppr k
pprExpr p (Call f e) = pprCall p f e
pprExpr _ (Tuple es) = parens (pprWithCommas ppr es)
pprExpr p (Lam v e)
  = parensIf p precZero $
    PP.text "lam" <+> parens (pprTVar v) <+> ppr e
pprExpr p (Let v e1 e2)
  = parensIf p precZero $
    PP.vcat [ PP.text "let" PP.<+>
                (bracesSp $ PP.sep [ ppr v
                                   , PP.nest 2 (PP.text "=" PP.<+> ppr e1) ])
           , ppr e2 ]
pprExpr p (If e1 e2 e3)
  = parensIf p precZero $
    PP.sep [ PP.text "if" PP.<+> ppr e1
           , PP.text "then" PP.<+> ppr e2
           , PP.text "else" PP.<+> ppr e3 ]
pprExpr p (Assert e1 e2)
  = parensIf p precZero $
    PP.sep [ PP.text "assert" PP.<+> pprParendExpr e1
           , ppr e2 ]

pprExpr _ (App e1 e2)
  = parens (text "App" <+> sep [pprParendExpr e1, pprParendExpr e2])
    -- We aren't expecting Apps, so I'm making them very visible

pprCall :: (Pretty f, Pretty b, HasInfix f) => Prec -> f -> ExprX f b -> Doc
pprCall prec f (Tuple [e1,e2])
  | Just prec' <- isInfix f
  = parensIf prec prec' $
    sep [ pprExpr prec' e1, ppr f <+> pprExpr prec' e2 ]

pprCall _ f e = PP.cat [ppr f, nest 2 (parensSp pp_args)]
  where
    pp_args = case e of
                (Tuple es) -> pprWithCommas ppr es
                _        -> ppr e

class HasInfix f where
  isInfix :: f -> Maybe Prec

instance HasInfix TFun where
  isInfix (TFun _ f) = isInfix f

instance HasInfix Fun where
  isInfix (Fun (PrimFun s))
    | s == "==" = Just precOne
    | s == "+"  = Just precTwo
    | s == "-"  = Just precTwo
    | s == "*"  = Just precThree
    | s == "/"  = Just precThree
  isInfix _ = Nothing

parensIf :: Prec -> Prec -> Doc -> Doc
parensIf ctxt inner doc
  | ctxt == precZero = doc
  | ctxt >= inner    = parens doc
  | otherwise        = doc

--  ppr p (If a b c)
--      = sep [ PP.text "if"   PP.<+> ppr p a
--            , PP.text "then" PP.<+> ppr p b
--            , PP.text "else" PP.<+> ppr p c ]

instance (Pretty f, Pretty b, HasInfix f) => Pretty (DeclX f b) where
  ppr (DefDecl d)  = ppr d
  ppr (RuleDecl r) = ppr r

instance (Pretty f, Pretty b, HasInfix f) => Pretty (DefX f b) where
  ppr (DefX f vs rhs)
    = PP.sep [ PP.text "def" PP.<+> ppr f
                 PP.<> parens (pprWithCommas pprTVar vs)
             , PP.nest 2 (PP.text "=" PP.<+> ppr rhs) ]

instance (Pretty f, Pretty b, HasInfix f) => Pretty (RuleX f b) where
  ppr (Rule { ru_name = name, ru_qvars = qvars
            , ru_lhs = lhs, ru_rhs = rhs })
    = PP.sep [ PP.text "rule" PP.<+> PP.doubleQuotes (text name)
                 PP.<+> parens (pprWithCommas pprTVar qvars)
             , PP.nest 2 (PP.sep [ ppr lhs, PP.nest 2 (PP.text "=" PP.<+> ppr rhs)]) ]

display :: Pretty p => p -> KM ()
display p = liftIO $ putStrLn (PP.render (ppr p))

displayN :: Pretty p => [p] -> KM ()
displayN ps = liftIO $ putStrLn (PP.render (go ps))
  where
    go []     = PP.empty
    go [p]    = ppr p
    go (p:ps) = vcat [ ppr p, text "", go ps ]

bracesSp :: Doc -> Doc
bracesSp d = PP.char '{' PP.<+> d PP.<+> PP.char '}'

parensSp :: Doc -> Doc
parensSp d = PP.char '(' PP.<+> d PP.<+> PP.char ')'

pprWithCommas :: (p -> Doc) -> [p] -> Doc
pprWithCommas ppr ps = PP.sep (add_commas ps)
  where
     add_commas []     = []
     add_commas [p]    = [ppr p]
     add_commas (p:ps) = ppr p PP.<> PP.comma : add_commas ps

instance Pretty a => Pretty [a] where
  ppr xs = PP.char '[' <> pprWithCommas ppr xs <> PP.char ']'

pprTrace :: String -> Doc -> a -> a
pprTrace str doc v
  = trace (take 100 $ PP.render (PP.sep [PP.text str, PP.nest 2 doc])) v

pprPanic :: HasCallStack => String -> Doc -> a
pprPanic str doc
  = error (take 1000 $ PP.render (PP.sep [PP.text str, PP.nest 2 doc]))

pps :: Pretty a => a -> String
pps a = show $ ppr a

test_Pretty =
  hspec $ do
    let test e s = it s $ pps e `shouldBe` s

    let var s = Var (Simple s)
    let e  = Call (Fun (UserFun "g")) (var "i")
    let e2 = Call (Fun (UserFun "f")) (Tuple [e, var "_t1", kInt 5])

    describe "Pretty" $ do
      test e "g( s$i )"
      test e2 "f( g( s$i ), s$_t1, 5 )"
