{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Lang where

import Prelude hiding( (<>) )

import qualified Text.PrettyPrint   as PP
import Text.PrettyPrint (Doc)
import KMonad

import Data.Either( partitionEithers )

import Debug.Trace( trace )
import Test.Hspec

-----------------------------------------------
--  The main data types
-----------------------------------------------

data DeclX f b = RuleDecl (RuleX f b)
               | DefDecl  (DefX f b)

type Decl  = DeclX Fun  Var
type TDecl = DeclX TFun TVar

data DefX f b  -- f x = e
  = DefX { def_fun  :: f
         , def_args :: [TVar]  -- See Note [Function arity]
         , def_rhs  :: ExprX f b }
  deriving( Show )

{- Note [Function arity]
~~~~~~~~~~~~~~~~~~~~~~~~
Top level functions with exactly one argument expect a call
    Call f e
and bind the argument to e

Top level functions with zero, or two or more arguments expect a call
   Call f (Tuple [e1, .. en])
and bind the arugments to e1.. en respectively.

That is, arity-1 is treated specially. We do not have 1-tuples.
-}

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

data Type = TypeZero Type  -- What is this and why do we need it?
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

data FunId = UserFun String   -- UserFuns have a Def
           | PrimFun PrimFun  -- PrimFuns do not have a Def
           | SelFun
                Int      -- Index; 1-indexed, so (SelFun 1 2) is fst
                Int      -- Arity
           deriving( Eq, Ord, Show )

data Fun = Fun     FunId         -- The function              f(x)
         | GradFun FunId ADMode  -- Full Jacobian Df(x)
                                 --   Rev <=> transposed  Rf(x)
         | DrvFun  FunId ADMode  -- Derivative derivative f'(x,dx)
                                 --   Rev <=> reverse mode f`(x,dr)
         deriving( Eq, Ord, Show )

data ADMode = Fwd | Rev
            deriving( Eq, Ord, Show )

data TFun = TFun Type Fun   -- Typed functions.  These are used at
  deriving (Eq, Ord, Show)  -- /occurrence/ sites only, not binding site

data Var
  = Dummy                   -- Used for type arguments
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

isDummy :: Var -> Bool
isDummy Dummy = True
isDummy _     = False

mkDummy :: Type -> TExpr
mkDummy ty = Var (TVar ty Dummy)

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
          HasInfix f, PrettyVar f, PrettyVar b)
      =>  HasType (ExprX f b) where
  typeof (Konst k)     = typeofKonst k
  typeof (Var b)       = typeof b
  typeof (Call f e)    = typeof f
  typeof e@(App f _)   = case typeof f of
                            TypeLambda _ res -> res
                            _ -> pprPanic "typeof:app " (vcat [ppr f, ppr (typeof f)])
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

unzipLMTypes :: HasCallStack => [Type] -> Maybe ([Type], [Type])
unzipLMTypes [] = Just ([], [])
unzipLMTypes (TypeLM s t : lmts) = case unzipLMTypes lmts of
                                     Just (ss, ts) -> Just (s:ss, t:ts)
                                     Nothing       -> Nothing
unzipLMTypes lmts = Nothing

typeofKonst :: Konst -> Type
typeofKonst (KZero t)    = t   -- Was: TypeZero t
typeofKonst (KInteger _) = TypeInteger
typeofKonst (KFloat _)   = TypeFloat
typeofKonst (KBool _)    = TypeBool

-----------------------------------------------
--     Debugging utilities
-----------------------------------------------

assert :: HasCallStack => SDoc -> Bool -> b -> b
assert doc True  x = x
assert doc False x = error (show doc)

assertBool :: Bool -> Bool
assertBool x = x    -- To remove check, return True always

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

instance Show Var where
  show v = case v of
    Dummy -> "/*dummy*/"
    Simple s -> "s$" ++ s
    Delta  d -> "d$" ++ d
    Grad g m -> "g" ++ (case m of
                          Fwd -> "f"
                          Rev -> "r")
                    ++ "$"
                    ++ g


-----------------------------------------------
--     SDoc abstraction over expression display style
-----------------------------------------------

type SDoc = Bool -> Doc -- True = S-expressions, False = infix style

(<>) :: SDoc -> SDoc -> SDoc
d1 <> d2 = \s -> (d1 s) PP.<> (d2 s)

(<+>) :: SDoc -> SDoc -> SDoc
d1 <+> d2 = \s -> (d1 s) PP.<+> (d2 s)

text :: String -> SDoc
text s _ = PP.text s

char :: Char -> SDoc
char c _ = PP.char c

int :: Int -> SDoc
int i _ = PP.int i

integer :: Integer -> SDoc
integer i _ = PP.integer i

double :: Double -> SDoc
double d _ = PP.double d

parens :: SDoc -> SDoc
parens d m = PP.parens (d m)

cat :: [SDoc] -> SDoc
cat ss m = PP.cat $ map (\s -> s m) ss

sep :: [SDoc] -> SDoc
sep ss m = PP.sep $ map (\s -> s m) ss

mode :: SDoc -> SDoc -> SDoc
mode se inf m = if m then se m else inf m

nest :: Int -> SDoc -> SDoc
nest i d m = PP.nest i (d m)

vcat :: [SDoc] -> SDoc
vcat ss m = PP.vcat $ map (\s -> s m) ss

hang :: SDoc -> Int -> SDoc -> SDoc
hang d1 i d2 m = PP.hang (d1 m) i (d2 m)

braces :: SDoc -> SDoc
braces d m = PP.braces (d m)

brackets :: SDoc -> SDoc
brackets d m = PP.brackets (d m)

doubleQuotes :: SDoc -> SDoc
doubleQuotes d m = PP.doubleQuotes (d m)

fsep :: [SDoc] -> SDoc
fsep ss m = PP.fsep $ map (\s -> s m) ss

punctuate :: SDoc -> [SDoc] -> [SDoc]
punctuate p ss = let
    ts = PP.punctuate (p True) $ map (\s -> s True) ss
    fs = PP.punctuate (p False) $ map (\s -> s False) ss
    in map (\(t,f) -> \m -> if m then t else f) (zip ts fs)

comma :: SDoc
comma = text ","

empty :: SDoc
empty m = PP.empty

default_display_style :: Bool
default_display_style = False

render :: SDoc -> String
render s = PP.render (s default_display_style)

instance Show SDoc where
  show s = show (s default_display_style)

-----------------------------------------------
--     Pretty printer for the KS language
-----------------------------------------------

class PrettyVar v where
  pprVar  :: v -> SDoc  -- Just print it
  pprBndr :: v -> SDoc  -- Print with its type

class Pretty p where
  ppr     :: p -> SDoc
  ppr = pprPrec precZero

  pprPrec :: Prec -> p -> SDoc
  pprPrec _ = ppr

instance Pretty Char where
  ppr s = char s

instance Pretty Bool where
  ppr True  = text "True"
  ppr False = text "False"

instance Pretty a => Pretty (Maybe a) where
  ppr Nothing  = text "Nothing"
  ppr (Just x) = text "Just" <+> ppr x

instance Pretty Var where
  ppr v  = text $ show v

instance Pretty FunId where
  ppr f = pprFunId f

instance Pretty Fun where
  ppr f = pprFun f

pprFunId :: FunId -> SDoc
pprFunId (UserFun s)  = text s
pprFunId (PrimFun p)  = text p
pprFunId (SelFun i n) = text "selfun<" <> int i <> comma <> int n <> char '>'

pprFun :: Fun -> SDoc
pprFun (Fun s)         = ppr s
pprFun (GradFun s Fwd) = text "D$"   <> ppr s
pprFun (GradFun s Rev) = text "R$"   <> ppr s
pprFun (DrvFun  s Fwd) = text "fwd$" <> ppr s
pprFun (DrvFun  s Rev) = text "rev$" <> ppr s

instance Pretty TVar where
  pprPrec p (TVar ty Dummy) = text "_:" <> ppr ty
  pprPrec p (TVar ty v)     = ppr v

instance PrettyVar TVar where
  pprVar  v = ppr v
  pprBndr v = pprTVar v

instance PrettyVar Var where
  pprVar v  = ppr v
  pprBndr v = ppr v

instance Pretty TFun where
  ppr (TFun ty f) = ppr f

instance PrettyVar Fun where
  pprVar  f = ppr f
  pprBndr f = ppr f

instance PrettyVar TFun where
  pprVar  f = ppr f
  pprBndr (TFun ty f) = ppr f <> text ":" <> ppr ty

instance Pretty Konst where
  ppr (KInteger i) = integer i
  ppr (KFloat f)   = double f
  ppr (KZero t)    = text "(KZero " <> ppr t <> char ')'

instance Pretty Type where
  ppr (TypeVec ty)         = text "(Vec " <> ppr ty <> text ")"
  ppr (TypeTuple tys)      = text "(Tuple (" <> pprList ppr tys <> text "))"
  ppr (TypeLambda from to) = text "(Lambda" <+> ppr from <+> text "->" <+> ppr to <> text ")"
  ppr (TypeLM s t)         = text "(LM" <+> ppr s <+> ppr t <> text ")"
  ppr (TypeZero t)         = text "zero_t@" <> ppr t
  ppr TypeFloat            = text "Float"
  ppr TypeInteger          = text "Integer"
  ppr TypeBool             = text "Bool"
  ppr TypeUnknown          = text "UNKNOWN"


type Prec = Int
 -- 0 => no need for parens
 -- high => parenthesise everything

precZero, precOne, precTwo, precThree :: Int
precZero  = 0  -- Base
precOne   = 1  -- ==
precTwo   = 2  -- +
precThree = 3  -- *

instance (HasInfix f, PrettyVar f, PrettyVar b)
      => Pretty (ExprX f b) where
  ppr expr = pprExpr 0 expr

pprParendExpr :: (HasInfix f, PrettyVar f, PrettyVar b)
              => ExprX f b -> SDoc
pprParendExpr = pprExpr precTwo

pprTVar :: TVar -> SDoc
pprTVar (TVar ty v) = ppr v <> text ":" <> ppr ty

pprExpr :: (HasInfix f, PrettyVar f, PrettyVar b)
        => Prec -> ExprX f b -> SDoc
pprExpr _  (Var v)   = pprVar v
pprExpr _ (Konst k)  = ppr k
pprExpr p (Call f e) = pprCall p f e
pprExpr _ (Tuple es) = parens (mode (text "tuple" <+> rest) rest)
    where rest = pprList ppr es
pprExpr p (Lam v e)
  = parensIf p precZero $
    text "lam" <+> parens (pprTVar v) <+> ppr e
pprExpr p (Let v e1 e2)
  = mode (parens $ sep [text "let", pprBndr v, ppr e1, ppr e2])
         (parensIf p precZero (vcat
            [ text "let" <+> (bracesSp $ sep [ pprBndr v, nest 2 (text "=" <+> ppr e1)])
            , ppr e2]))
pprExpr p (If e1 e2 e3) = mode
    (parens (sep [text "if", ppr e1, ppr e2, ppr e3]))
    (parensIf p precZero (sep [ text "if" <+> ppr e1
                        , text "then" <+> ppr e2
                        , text "else" <+> ppr e3 ]))
pprExpr p (Assert e1 e2)
  = parensIf p precZero $
    sep [ text "assert" <+> pprParendExpr e1, ppr e2 ]

pprExpr _ (App e1 e2)
  = parens (text "App" <+> sep [pprParendExpr e1, pprParendExpr e2])
    -- We aren't expecting Apps, so I'm making them very visible

pprCall :: (PrettyVar f, PrettyVar b, HasInfix f)
        => Prec -> f -> ExprX f b -> SDoc
pprCall prec f e = mode (parens $ (pprVar f) <+> pp_args)
  (case (e, isInfix f) of
      (Tuple [e1, e2], Just prec') -> parensIf prec prec' $
          sep [ pprExpr prec' e1, pprVar f <+> pprExpr prec' e2 ]
      _                            -> cat [pprVar f, nest 2 (parensSp pp_args)])
  where
    pp_args = case e of
                (Tuple es) -> pprList ppr es
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

parensIf :: Prec -> Prec -> SDoc -> SDoc
parensIf ctxt inner doc
  | ctxt == precZero = doc
  | ctxt >= inner    = parens doc
  | otherwise        = doc

instance (Show f, PrettyVar f, PrettyVar b, HasInfix f)
      => Pretty (DeclX f b) where
  ppr (DefDecl d)  = ppr d
  ppr (RuleDecl r) = ppr r

instance (Show f, PrettyVar f, PrettyVar b, HasInfix f) => Pretty (DefX f b) where
  ppr (DefX f vs rhs) = mode
      (parens $ sep [ text "def", bndr, parens (sep (map pprTVar vs)), ppr rhs])
      (sep [ hang (text "def" <+> bndr)
             2 (parens (pprList pprTVar vs))
           , nest 2 (text "=" <+> ppr rhs) ])
        where
          bndr = pprBndr f <+> (brackets (text (show f)))

instance (PrettyVar f, PrettyVar b, HasInfix f) => Pretty (RuleX f b) where
  ppr (Rule { ru_name = name, ru_qvars = qvars
            , ru_lhs = lhs, ru_rhs = rhs })
    = sep [ text "rule" <+> doubleQuotes (text name)
                 <+> parens (pprList pprTVar qvars)
             , nest 2 (sep [ ppr lhs, nest 2 (text "=" <+> ppr rhs)]) ]

display :: Pretty p => p -> KM ()
display p = liftIO $ putStrLn (render (ppr p))

displayN :: Pretty p => [p] -> KM ()
displayN ps = liftIO $ putStrLn (render (go ps))
  where
    go []     = empty
    go [p]    = ppr p
    go (p:ps) = vcat [ ppr p, text "", go ps ]

bracesSp :: SDoc -> SDoc
bracesSp d = char '{' <+> d <+> char '}'

parensSp :: SDoc -> SDoc
parensSp d = char '(' <+> d <+> char ')'

pprList :: (p -> SDoc) -> [p] -> SDoc
pprList ppr ps = let pps = map ppr ps in
    mode (sep pps) (sep $ punctuate comma pps)

instance Pretty a => Pretty [a] where
  ppr xs = char '[' <> pprList ppr xs <> char ']'

pprTrace :: String -> SDoc -> a -> a
pprTrace str doc v
  = trace (render (sep [text str, nest 2 doc])) v

pprPanic :: HasCallStack => String -> SDoc -> a
pprPanic str doc
  = error (take 1000 $ render (sep [text str, nest 2 doc]))

pps :: Pretty a => a -> String
pps a = show $ ppr a

hspec :: Spec
hspec = do
    let test e s = it s $ pps e `shouldBe` s

    let var s = Var (Simple s)
    let e  = Call (Fun (UserFun "g")) (var "i")
    let e2 = Call (Fun (UserFun "f")) (Tuple [e, var "_t1", kInt 5])

    describe "Pretty" $ do
      test e "g( s$i )"
      test e2 "f( g( s$i ), s$_t1, 5 )"

test_Pretty :: IO ()
test_Pretty = Test.Hspec.hspec Lang.hspec
