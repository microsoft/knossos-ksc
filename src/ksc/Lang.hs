{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Lang where

import           Prelude                 hiding ( (<>) )

import qualified Text.PrettyPrint              as PP
import           Text.PrettyPrint               ( Doc )
import           Data.List                      ( intersperse )
import           KMonad

import           Data.Either                    ( partitionEithers )

import           Debug.Trace                    ( trace )
import           Test.Hspec

-----------------------------------------------
--  The main data types
-----------------------------------------------

data DeclX f b = RuleDecl (RuleX f b)
               | DefDecl  (DefX f b)

type Decl  = DeclX Fun  Var
type TDecl = DeclX TFun TVar

data DefX f b  -- f x = e
  = DefX { def_fun  :: TFun
         , def_args :: [TVar]  -- See Note [Function arity]
         , def_rhs  :: ExprX f b }
  deriving( Show )
  -- Definitions are user-annotated with argument types
  -- (via TVar) and result types (via TFun)

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
  | Call f [ExprX f b]      -- f e
  | Tuple [ExprX f b]       -- (e1, ..., en)
  | Lam TVar (ExprX f b)    -- Lambda-bound variable is typed from birth
  | App (ExprX f b) (ExprX f b)
  | Let b (ExprX f b) (ExprX f b)    -- let x = e1 in e2  (non-recursive)
  | If (ExprX f b) (ExprX f b) (ExprX f b)  -- FIXME make cond ExprX?
  | Assert (ExprX f b) (ExprX f b)
  deriving( Show )

type Expr  = ExprX Fun Var
type TExpr = ExprX TFun TVar

data Type = TypeBool
          | TypeInteger
          | TypeFloat
          | TypeString
          | TypeTuple [Type]
          | TypeVec Type
          | TypeLambda Type Type   -- Domain -> Range
          | TypeLM Type Type       -- Linear map  Src -o Target
          | TypeUnknown
          deriving (Show, Eq, Ord)

isScalar :: Type -> Bool
isScalar = \case
  TypeBool       -> True
  TypeInteger    -> True
  TypeFloat      -> True
  TypeString     -> True
  TypeTuple ts   -> all isScalar ts
  TypeVec   _    -> False
  TypeLambda _ _ -> False
  TypeLM     _ _ -> error "Shouldn't see TypeLM at this stage of codegen"
  TypeUnknown    -> error "Shouldn't see TypeUnknown at this stage of codegen"

----------------------------------
--- Tangent space

tangentType :: Type -> Type
-- We can't differentiate Integer, Bool etc.
tangentType TypeFloat      = TypeFloat
tangentType (TypeVec   t ) = TypeVec (tangentType t)
tangentType (TypeTuple ts) = TypeTuple (map tangentType ts)
tangentType TypeInteger    = TypeTuple []
tangentType TypeBool       = TypeTuple []
tangentType TypeString     = TypeTuple []
tangentType TypeUnknown    = TypeUnknown
tangentType t              = pprPanic "tangentType" (ppr t)
                               -- TypeLM, TypeLambda

eqType :: Type -> Type -> Bool
eqType t1 t2 = t1 == t2

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

data TFun = TFun Type Fun   -- Typed functions.  The type is the /return/
  deriving (Eq, Ord, Show)  -- type of the function.

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

data Konst = KInteger Integer
           | KFloat   Double
           | KBool    Bool
           | KString  String
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

isKZero :: TExpr -> Bool
isKZero = \case
  Konst (KInteger 0  ) -> True
  Konst (KFloat   0.0) -> True
  Call (TFun _ (Fun (UserFun "zero"))) _ -> True
  _                    -> False

partitionDecls :: [DeclX f b] -> ([RuleX f b], [DefX f b])
-- Separate the Rules from the Defs
partitionDecls decls =
  partitionEithers
    $ map
        (\case
          RuleDecl r -> Left r
          DefDecl  d -> Right d
        )
    $ decls

tVarVar :: TVar -> Var
tVarVar (TVar _ v) = v

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
mkLet (TVar ty v) rhs body =
  assertEqualThen ("mkLet " ++ show v ++ " = " ++ pps rhs) ty (typeof rhs)
    $ Let (TVar ty v) rhs body

mkLets :: HasCallStack => [(TVar, TExpr)] -> TExpr -> TExpr
mkLets xs e = foldr (uncurry mkLet) e xs

kInt :: Integer -> ExprX f b
kInt i = Konst (KInteger i)

kTInt :: Integer -> TExpr
kTInt i = Konst (KInteger i)

kFloat :: Double -> Expr
kFloat f = Konst (KFloat f)

kTFloat :: Double -> TExpr
kTFloat f = Konst (KFloat f)

zeroInt :: TExpr
zeroInt = Konst (KInteger 0)

zeroFloat :: TExpr
zeroFloat = Konst (KFloat 0.0)

mkTuple :: [ExprX f b] -> ExprX f b
mkTuple [e] = e
mkTuple es  = Tuple es

mkTupleTy :: [Type] -> Type
mkTupleTy [t] = t
mkTupleTy ts  = TypeTuple ts

dropLast :: [a] -> [a]
-- Drop the last element of a list.
-- No-op for empty list
dropLast xs = take (length xs - 1) xs

-----------------------------------------------
--     Building types
-----------------------------------------------

mkTypeTuple :: [Type] -> Type
mkTypeTuple [ty] = ty
mkTypeTuple tys  = TypeTuple tys


-----------------------------------------------
--  Finding the type of an expression
-----------------------------------------------

class HasType b where
  typeof :: HasCallStack => b -> Type

typeofArgs :: HasType b => [b] -> Type
typeofArgs args = mkTupleTy (map typeof args)

instance HasType TVar where
  typeof (TVar ty _) = ty

instance HasType TFun where
  typeof (TFun ty _) = ty

instance (HasType b, HasType f,
          HasInfix f, PrettyVar f, PrettyVar b)
      =>  HasType (ExprX f b) where
  typeof (Konst k)     = typeofKonst k
  typeof (Var b)       = typeof b
  typeof (Call f _)    = typeof f
  typeof (App f _)     = case typeof f of
                            TypeLambda _ res -> res
                            _ -> pprPanic "typeof:app " (vcat [ppr f, ppr (typeof f)])
  typeof (Tuple es)    = TypeTuple $ map typeof es
  typeof (Lam b e)     = TypeLambda (typeof b) (typeof e)
  typeof (Let _ _ e2)  = typeof e2
  typeof (Assert _ e)  = typeof e
  typeof (If _ t f)    = makeIfType (typeof t) (typeof f)

-- ToDo: delete this if no longer needed
makeIfType :: HasCallStack => Type -> Type -> Type
makeIfType ty1 ty2 = assertEqualThen "makeIfType" ty1 ty2 $ ty2

getLM :: HasCallStack => Type -> Type
getLM (TypeLM s t) = TypeLM s t
getLM t            = error $ "Wanted TypeLM, got " ++ pps t

unzipLMTypes :: HasCallStack => [Type] -> Maybe ([Type], [Type])
unzipLMTypes []                  = Just ([], [])
unzipLMTypes (TypeLM s t : lmts) = case unzipLMTypes lmts of
  Just (ss, ts) -> Just (s : ss, t : ts)
  Nothing       -> Nothing
unzipLMTypes _ = Nothing

typeofKonst :: Konst -> Type
typeofKonst (KInteger _) = TypeInteger
typeofKonst (KFloat   _) = TypeFloat
typeofKonst (KBool    _) = TypeBool
typeofKonst (KString  _) = TypeString

-----------------------------------------------
--     Debugging utilities
-----------------------------------------------

assert :: HasCallStack => SDoc -> Bool -> b -> b
assert _   True  x = x
assert doc False _ = error (show doc)

assertBool :: Bool -> Bool
assertBool x = x    -- To remove check, return True always

assertEqualThen :: (HasCallStack, Eq a, Show a) => String -> a -> a -> b -> b
assertEqualThen msg t1 t2 e = if t1 == t2
  then e
  else
    error
        (  "Asserts unequal ["
        ++ msg
        ++ "] \n T1 = "
        ++ show t1
        ++ "\n T2 = "
        ++ show t2
        ++ "\n"
        )
      $ e

assertTypesEqualThen :: HasCallStack => String -> Type -> Type -> b -> b
assertTypesEqualThen = assertEqualThen

assertAllEqualThen :: (HasCallStack, Eq a, Show a) => String -> [a] -> b -> b
assertAllEqualThen msg es e = if allEq es
  then e
  else
    flip trace e
      $ ("Assert failed: [" ++ msg ++ "] not all equal  \n " ++ show es ++ "\n")
 where
  allEq []       = True
  allEq (a : as) = allEqa a as

  allEqa a0 = all (a0 ==)

assertAllEqualRet :: (HasCallStack, Eq a, Show a) => String -> [a] -> a
assertAllEqualRet msg [] = error ("assertAllEqualRet: Empty list: " ++ msg)
assertAllEqualRet msg (e : es) = assertAllEqualThen msg (e : es) e



-----------------------------------------------
--       Show instances
-----------------------------------------------

instance Show Var where
  show v = case v of
    Dummy -> "/*dummy*/"
    Simple s -> s
    Delta  d -> "d$" ++ d
    Grad g m -> "g" ++ (case m of
                          Fwd -> "f"
                          Rev -> "r")
                    ++ "$"
                    ++ g


-----------------------------------------------
--     SDoc abstraction over expression display style
-----------------------------------------------

newtype SDoc = SDoc(Bool -> Doc) -- True = S-expressions, False = infix style

(<>) :: SDoc -> SDoc -> SDoc
(SDoc d1) <> (SDoc d2) = SDoc (\s -> (d1 s) PP.<> (d2 s))

(<+>) :: SDoc -> SDoc -> SDoc
(SDoc d1) <+> (SDoc d2) = SDoc (\s -> (d1 s) PP.<+> (d2 s))

text :: String -> SDoc
text s = SDoc (\_ -> PP.text s)

char :: Char -> SDoc
char c = SDoc (\_ -> PP.char c)

int :: Int -> SDoc
int i = SDoc (\_ -> PP.int i)

integer :: Integer -> SDoc
integer i = SDoc (\_ -> PP.integer i)

double :: Double -> SDoc
double d = SDoc (\_ -> PP.double d)

parens :: SDoc -> SDoc
parens (SDoc d) = SDoc (\m -> PP.parens $ d m)

cat :: [SDoc] -> SDoc
cat ss = SDoc
  (\m -> PP.cat $ map
    (\case
      SDoc s -> s m
    )
    ss
  )

sep :: [SDoc] -> SDoc
sep ss = SDoc
  (\m -> PP.sep $ map
    (\case
      SDoc s -> s m
    )
    ss
  )

mode :: SDoc -> SDoc -> SDoc
mode (SDoc se) (SDoc inf) = SDoc (\m -> if m then se m else inf m)

nest :: Int -> SDoc -> SDoc
nest i (SDoc d) = SDoc (\m -> PP.nest i (d m))

vcat :: [SDoc] -> SDoc
vcat ss = SDoc
  (\m -> PP.vcat $ map
    (\case
      SDoc s -> s m
    )
    ss
  )

hang :: SDoc -> Int -> SDoc -> SDoc
hang (SDoc d1) i (SDoc d2) = SDoc (\m -> PP.hang (d1 m) i (d2 m))

braces :: SDoc -> SDoc
braces (SDoc d) = SDoc (\m -> PP.braces $ d m)

brackets :: SDoc -> SDoc
brackets (SDoc d) = SDoc (\m -> PP.brackets $ d m)

doubleQuotes :: SDoc -> SDoc
doubleQuotes (SDoc d) = SDoc (\m -> PP.doubleQuotes $ d m)

fsep :: [SDoc] -> SDoc
fsep ss = SDoc
  (\m -> PP.fsep $ map
    (\case
      SDoc s -> s m
    )
    ss
  )

punctuate :: SDoc -> [SDoc] -> [SDoc]
punctuate (SDoc p) ss =
  let ts = PP.punctuate (p True) $ map
        (\case
          SDoc s -> s True
        )
        ss
      fs = PP.punctuate (p False) $ map
        (\case
          SDoc s -> s False
        )
        ss
  in  map (\(t, f) -> SDoc (\m -> if m then t else f)) (zip ts fs)

comma :: SDoc
comma = text ","

empty :: SDoc
empty = SDoc (\_ -> PP.empty)

default_display_style :: Bool
default_display_style = False

render :: SDoc -> String
render (SDoc s) = PP.render (s default_display_style)

renderSexp :: SDoc -> String
renderSexp (SDoc s) = PP.render (s True)

instance Show SDoc where
  show (SDoc s) = show (s default_display_style)

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
pprFunId (UserFun s ) = text s
pprFunId (PrimFun p ) = text p
pprFunId (SelFun i n) = text "get$" <> int i <> char '$' <> int n

pprFun :: Fun -> SDoc
pprFun (Fun s        ) = ppr s
pprFun (GradFun s Fwd) = text "D$" <> ppr s
pprFun (GradFun s Rev) = text "R$" <> ppr s
pprFun (DrvFun  s Fwd) = text "fwd$" <> ppr s
pprFun (DrvFun  s Rev) = text "rev$" <> ppr s

instance Pretty TVar where
  pprPrec _ (TVar ty Dummy) = parens $ text "_ : " <> ppr ty
  pprPrec _ (TVar _ v)     = ppr v

instance PrettyVar TVar where
  pprVar  v = ppr v
  pprBndr v = pprTVar v

instance PrettyVar Var where
  pprVar v  = ppr v
  pprBndr v = ppr v

instance Pretty TFun where
  ppr (TFun _ f) = ppr f

instance PrettyVar Fun where
  pprVar  f = ppr f
  pprBndr f = ppr f

instance PrettyVar TFun where
  pprVar  f = ppr f
  pprBndr (TFun ty f) = ppr f <+> text ":" <+> ppr ty

instance Pretty Konst where
  pprPrec _ (KInteger i) = integer i
  pprPrec _ (KFloat f)   = double f
  pprPrec _ (KString s)  = text (show s)
  pprPrec _ (KBool b)    = text (case b of { True -> "true"; False -> "false" })

instance Pretty Type where
  pprPrec p (TypeVec ty)         = parensIf p precZero $
                                   text "Vec" <+> pprParendType ty
  pprPrec p (TypeTuple tys)      = parensIf p precZero $
                                   text "Tuple" <+> parens (pprList ppr tys)
  pprPrec p (TypeLambda from to) = parensIf p precZero $
                                   text "Lambda" <+> ppr from <+> text "->" <+> ppr to
  pprPrec p (TypeLM s t)         = parensIf p precZero $ text "LM" <+> pprParendType s <+> pprParendType t
  pprPrec _ TypeFloat            = text "Float"
  pprPrec _ TypeInteger          = text "Integer"
  pprPrec _ TypeString           = text "String"
  pprPrec _ TypeBool             = text "Bool"
  pprPrec _ TypeUnknown          = text "UNKNOWN"

pprParendType :: Type -> SDoc
pprParendType = pprPrec  precTwo

type Prec = Int
 -- 0 => no need for parens
 -- high => parenthesise everything

precZero, precOne, precTwo, precThree :: Int
precZero = 0  -- Base
precOne = 1  -- ==
precTwo = 2  -- +
precThree = 3  -- *

instance (HasInfix f, PrettyVar f, PrettyVar b)
      => Pretty (ExprX f b) where
  pprPrec = pprExpr

pprParendExpr :: (HasInfix f, PrettyVar f, PrettyVar b) => ExprX f b -> SDoc
pprParendExpr = pprExpr precTwo

pprTVar :: TVar -> SDoc
pprTVar (TVar ty v) = ppr v <+> text ":" <+> ppr ty

pprExpr :: (HasInfix f, PrettyVar f, PrettyVar b) => Prec -> ExprX f b -> SDoc
pprExpr _ (Var   v ) = pprVar v
pprExpr p (Konst k ) = pprPrec p k
pprExpr p (Call f e) = pprCall p f e
pprExpr _ (Tuple es) = mode (parens $ text "tuple" <+> rest) (parens rest)
  where rest = pprList ppr es
pprExpr p (Lam v e) =  mode (parensIf p precZero $ text "lam" <+> parens (pprTVar v) <+> ppr e)
                            (parens $ text "lam" <+> (vcat [parens (pprTVar v), ppr e]))
pprExpr p (Let v e1 e2) = mode
  (pprLetSexp v e1 e2)
  (parensIf
    p
    precZero
    (vcat
      [ text "let"
        <+> (bracesSp $ sep [pprBndr v, nest 2 (text "=" <+> ppr e1)])
      , ppr e2
      ]
    )
  )
pprExpr p (If e1 e2 e3) = mode
  (parens (sep [text "if", ppr e1, ppr e2, ppr e3]))
  (parensIf
    p
    precZero
    (sep [text "if" <+> ppr e1, text "then" <+> ppr e2, text "else" <+> ppr e3])
  )
pprExpr p (Assert e1 e2) =
  parensIf p precZero $ sep [text "assert" <+> pprParendExpr e1, ppr e2]

pprExpr _ (App e1 e2) =
  parens (text "App" <+> sep [pprParendExpr e1, pprParendExpr e2])
    -- We aren't expecting Apps, so I'm making them very visible

pprCall
  :: (PrettyVar f, PrettyVar b, HasInfix f) => Prec -> f -> [ExprX f b] -> SDoc
pprCall prec f e = mode
  (parens $ (pprVar f) <+> pp_args)
  (case (e, isInfix f) of
    ([e1, e2], Just prec') -> parensIf prec prec'
      $ sep [pprExpr prec' e1, pprVar f <+> pprExpr prec' e2]
    _ -> cat [pprVar f, nest 2 (parensSp pp_args)]
  )
 where
  pp_args = pprList ppr e

pprLetSexp :: (PrettyVar f, PrettyVar b, HasInfix f) =>
           b -> ExprX f b -> ExprX f b -> SDoc
pprLetSexp v e body =
      go [(v,e)] body
    where
      go binds (Let v1 e1 body) = go ((v1,e1):binds) body
      go binds body =
            parens $ sep [text "let", (parens $ vcat (map parenBind $ reverse binds)),
                        ppr body]
      parenBind (v,e) = parens $ pprVar v <+> ppr e


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
parensIf ctxt inner doc | ctxt == precZero = doc
                        | ctxt >= inner    = parens doc
                        | otherwise        = doc

instance (Show f, PrettyVar f, PrettyVar b, HasInfix f)
      => Pretty (DeclX f b) where
  ppr (DefDecl d)  = ppr d
  ppr (RuleDecl r) = ppr r

instance (Show f, PrettyVar f, PrettyVar b, HasInfix f) => Pretty (DefX f b) where
  ppr (DefX f vs rhs) = mode
      (parens $ sep [ text "def", pprBndr f, parens (sep (map (parens . pprTVar) vs)), ppr rhs])
      (sep [ hang (text "def" <+> pprBndr f)
             2 (parens (pprList pprTVar vs))
           , nest 2 (text "=" <+> ppr rhs) ])

instance (PrettyVar f, PrettyVar b, HasInfix f) => Pretty (RuleX f b) where
  ppr (Rule { ru_name = name, ru_qvars = qvars
            , ru_lhs = lhs, ru_rhs = rhs })
    = sep [ text "rule" <+> doubleQuotes (text name)
                 <+> parens (pprList pprTVar qvars)
             , nest 2 (sep [ ppr lhs, nest 2 (text "=" <+> ppr rhs)]) ]

printK :: SDoc -> KM ()
printK d = liftIO (putStrLn (render d))

display :: Pretty p => p -> KM ()
display p = printK (ppr p)

displayN :: Pretty p => [p] -> KM ()
displayN ps = printK (vcat $ intersperse (text "") $ map ppr ps)

bracesSp :: SDoc -> SDoc
bracesSp d = char '{' <+> d <+> char '}'

parensSp :: SDoc -> SDoc
parensSp d = char '(' <+> d <+> char ')'

pprList :: (p -> SDoc) -> [p] -> SDoc
pprList ppr ps =
  let pps = map ppr ps in mode (sep pps) (sep $ punctuate comma pps)

instance Pretty a => Pretty [a] where
  ppr xs = char '[' <> pprList ppr xs <> char ']'

pprTrace :: String -> SDoc -> a -> a
pprTrace str doc v = trace (render (sep [text str, nest 2 doc])) v

pprPanic :: HasCallStack => String -> SDoc -> a
pprPanic str doc = error (take 1000 $ render (sep [text str, nest 2 doc]))

pps :: Pretty a => a -> String
pps a = show $ ppr a

hspec :: Spec
hspec = do
  let test e s = it s $ pps e `shouldBe` s

  let var s = Var (Simple s)
  let e  = Call (Fun (UserFun "g")) [var "i"]
  let e2 = Call (Fun (UserFun "f")) [e, var "_t1", kInt 5]

  describe "Pretty" $ do
    test e  "g( i )"
    test e2 "f( g( i ), _t1, 5 )"

test_Pretty :: IO ()
test_Pretty = Test.Hspec.hspec Lang.hspec
