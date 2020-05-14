-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances, LambdaCase,
             PatternSynonyms, StandaloneDeriving, AllowAmbiguousTypes,
             ScopedTypeVariables, TypeApplications #-}

module Lang where

import           Prelude                 hiding ( (<>) )

import qualified Text.PrettyPrint              as PP
import           Text.PrettyPrint               ( Doc )
import           Data.List                      ( intersperse )
import           KMonad

import qualified Data.Map as M
import           Debug.Trace                    ( trace )
import           Test.Hspec

-----------------------------------------------
--  The main data types
-----------------------------------------------

mkGradType :: ADPlan -> Type -> Type -> Type
mkGradType BasicAD s ty = TypeLM s ty
mkGradType TupleAD s ty = TypeTuple [ty, TypeLM s ty]
  -- For TupleAD, mkGradType s t = (t, s -o t)

mkGradTuple :: ADPlan -> TExpr -> TExpr -> TExpr
mkGradTuple BasicAD _ lm = lm
mkGradTuple TupleAD p lm = Tuple [p, lm]

data Phase = Parsed | Typed | OccAnald

data DeclX p = RuleDecl (RuleX p)
             | DefDecl  (DefX p)

type Decl  = DeclX Parsed
type TDecl = DeclX Typed

data DefX p  -- f x = e
  = Def { def_fun    :: Fun
        , def_args   :: TVarX  -- See Note [Function arity]
        , def_res_ty :: TypeX     -- Result type
        , def_rhs    :: RhsX p }
  -- Definitions are user-annotated with argument types
  -- (via TVar) and result types (via TFun)

type Def  = DefX Parsed
type TDef = DefX Typed

data RhsX p
  = UserRhs (ExprX p)   -- An ordinary definition with a right hand side

  | StubRhs             -- Used during recursion, or after a type error
                        --   to allow a Def to be added to the environment
                        --   without having a RHS

  | EDefRhs             -- An external definition: no RHS

type TRhs  = RhsX Typed

isUserDef :: DefX p -> Bool
isUserDef (Def { def_rhs = UserRhs {} }) = True
isUserDef _ = False

data TVarX = TVar TypeX Var
type TVar = TVarX

deriving instance Ord TVar
instance Show TVar where
  show e = pps e

tVarVar :: TVarX -> Var
tVarVar (TVar _ v) = v

tVarType :: TVarX -> TypeX
tVarType (TVar ty _) = ty

-- MTypeX pm
--   pm controls whether the type is there at all
type family MTypeX p where
  MTypeX Parsed   = ()
  MTypeX Typed    = Type
  MTypeX OccAnald = Type

{- Note [Function arity]
~~~~~~~~~~~~~~~~~~~~~~~~

All functions are functions of a single argument, regardless of whether
they were given multiple arguments in the surface syntax.  If the
surface syntax def takes multiple arguments then we generate code to
unpack the arguments from a single argument tuple.  For consistency we
therefore also treat calls specially in the surface syntax.
Multiple arguments are implicitly wrapped in a tuple.  That is, the syntax

    (def f T ((x1 : S1) ... (xn : Sn)) ...)

means the same thing as

    (def f T (x : (Tuple S1 ... Sn))
        (let ((x1 (get$1$n x))
              ...
              (xn (get$n$n x)))
       ...))

and the surface syntax

    (f e1 ... en)

means the same thing as

    (f (tuple e1 ... en))

if n /= 2.  Nested unpacking is not supported, yet.

-}

type family VarX p where
  VarX Parsed   = Var
  VarX Typed    = TVar
  VarX OccAnald = TVar

type family LetBndrX p where
  LetBndrX Parsed   = Var
  LetBndrX Typed    = TVarX
  LetBndrX OccAnald = (Int,TVarX)

type family FunX p where
  FunX Parsed   = Fun
  FunX Typed    = TFun
  FunX OccAnald = TFun

data ExprX p
  = Konst Konst
  | Var  (VarX p)
  | Call (FunX p) (ExprX p)  -- f e
  | Tuple [ExprX p]          -- (e1, ..., en)
  | Lam TVarX (ExprX p)      -- Lambda-bound variable is typed from birth
  | App (ExprX p) (ExprX p)
  | Let (LetBndrX p) (ExprX p) (ExprX p)    -- let x = e1 in e2  (non-recursive)
  | If (ExprX p) (ExprX p) (ExprX p)
  | Assert (ExprX p) (ExprX p)
  | Dummy (MTypeX p)

instance InPhase p => Show (ExprX p) where
  show e = pps e

type Expr  = ExprX Parsed

type TExpr = ExprX Typed
type Type  = TypeX

data TypedExpr = TE TExpr Type   -- A pair of an expression and its type

exprOf :: TypedExpr -> TExpr
exprOf (TE e _) = e

unzipTEs :: [TypedExpr] -> ([TExpr], [Type])
unzipTEs []             = ([],[])
unzipTEs (TE e t : tes) = (e:es, t:ts)
  where
    (es, ts) = unzipTEs tes

data TypeX
  = TypeBool
  | TypeInteger
  | TypeFloat
  | TypeString
  | TypeTuple [TypeX]

  | TypeVec TypeX

  | TypeLam TypeX TypeX  -- Domain -> Range
  | TypeLM  TypeX TypeX   -- Linear map  Src -o Target

  | TypeUnknown

deriving instance Eq  Type
deriving instance Ord Type

instance Show TypeX where
  show e = pps e

-- Round trip testing requires that two different DeclXs be checked
-- for equality.  If the equality check fails we'd like to print them
-- to see the differences.  Thus we add Eq and Show instances for
-- DeclX and other types that transitively need them.
deriving instance Eq (RhsX Typed)
deriving instance Eq (DefX Typed)
deriving instance Eq (DeclX Typed)
deriving instance Eq (RuleX Typed)

deriving instance Show (RhsX Typed)
deriving instance Show (DefX Typed)
deriving instance Show (DeclX Typed)
deriving instance Show (RuleX Typed)

deriving instance Eq TVarX
deriving instance Eq (ExprX Parsed)
deriving instance Eq (RhsX Parsed)
deriving instance Eq (DefX Parsed)
deriving instance Eq (DeclX Parsed)
deriving instance Eq (RuleX Parsed)

deriving instance Show (RhsX Parsed)
deriving instance Show (DefX Parsed)
deriving instance Show (DeclX Parsed)
deriving instance Show (RuleX Parsed)

-- TypeSize is used to document when an integer represents a Size.
-- It's too viral to use a separate Integer type because most integer operations
-- need to be supported, e.g. the size of a lower-triangular matrix is d*(d+1)/2
pattern TypeSize :: TypeX
pattern TypeSize = TypeInteger

isScalar :: Type -> Bool
isScalar = \case
  TypeBool      -> True
  TypeInteger   -> True
  TypeFloat     -> True
  TypeString    -> True
  TypeTuple ts  -> all isScalar ts
  TypeVec    {} -> False
  TypeLam {}    -> False
  TypeLM     {} -> error "Shouldn't see TypeLM at this stage of codegen"
  TypeUnknown   -> error "Shouldn't see TypeUnknown at this stage of codegen"

----------------------------------
--- Tangent space

tangentType :: HasCallStack => Type -> Type
-- We can't differentiate Integer, Bool etc.
tangentType TypeFloat      = TypeFloat
tangentType (TypeVec t)    = TypeVec (tangentType t)
tangentType (TypeTuple ts) = TypeTuple (map tangentType ts)
tangentType TypeInteger    = TypeTuple []
tangentType TypeBool       = TypeTuple []
tangentType TypeString     = TypeTuple []
tangentType TypeUnknown    = TypeUnknown
tangentType t              = pprPanic "tangentType" (ppr t)
                               -- TypeLM, TypeLam

-- eqType is currently == but coud be more complicated if we do size
-- types
eqType :: Type -> Type -> Bool
eqType = (==)

eqTypes :: Type -> [Type] -> Maybe Type
eqTypes x xs = if all (eqType x) xs
               then Just x
               else Nothing

eqSize :: TExpr -> TExpr -> Bool
eqSize (Konst k1) (Konst k2) = traceWhenUnequal "eqSize" k1 k2 $ k1 == k2
-- eqSize (Var v1) (Var v2) = traceWhenUnequal "eqSize" v1 v2 $ v1 == v2
-- Punt on all other size equality checks
eqSize _e1 _e2 = -- trace ("[Punting eqSize " ++ pps _e1 ++ " == " ++ pps _e2 ++ "]")
                 True

type PrimFun = String

data FunId = UserFun String   -- UserFuns have a Def
           | PrimFun PrimFun  -- PrimFuns do not have a Def
           | SelFun
                Int      -- Index; 1-indexed, so (SelFun 1 2) is fst
                Int      -- Arity
           deriving( Eq, Ord, Show )

data Fun = Fun      FunId         -- The function f(x)
         | GradFun  FunId ADPlan  -- Full Jacobian Df(x)
         | DrvFun   FunId ADMode  -- Derivative derivative f'(x,dx)
                                  --   Rev <=> reverse mode f`(x,dr)
         deriving( Eq, Ord, Show )

isUserFun :: FunId -> Bool
isUserFun = \case
  UserFun{} -> True
  PrimFun{} -> False
  SelFun{}  -> False

isSelFun :: FunId -> Bool
isSelFun = \case
  UserFun{} -> False
  PrimFun{} -> False
  SelFun{}  -> True

funIdOfFun :: Fun -> FunId
funIdOfFun = \case
  Fun f       -> f
  GradFun f _ -> f
  DrvFun f _  -> f

data ADMode = AD { adPlan :: ADPlan, adDir :: ADDir }
  deriving( Eq, Ord, Show )

data ADPlan = BasicAD | TupleAD
  deriving( Eq, Ord, Show )

data ADDir = Fwd | Rev
  deriving( Eq, Ord, Show )

data TFun = TFun Type Fun   -- Typed functions.  The type is the /return/
  deriving (Eq, Ord)  -- type of the function.


data Var
  = Simple String         -- x
  | Delta  String         -- The 'dx' or 'dr' argument to
                          -- forward or backward versions of f
  | Grad   String ADPlan  -- Derivative of x
  deriving( Eq, Ord, Show )

varName :: Var -> String
varName = \case
  Simple s -> s
  Delta s  -> s
  Grad s _ -> s

tVarName :: TVarX -> String
tVarName tv = varName (tVarVar tv)

data Konst = KInteger Integer   -- :: TypeInteger
           | KFloat   Double    -- :: TypeFloat
           | KBool    Bool      -- :: TypeBool
           | KString  String    -- :: TypeString
           deriving( Eq, Ord, Show )

data RuleX p = Rule { ru_name  :: String   -- Just for logging
                    , ru_qvars :: [TVarX]
                    , ru_lhs   :: ExprX p
                    , ru_rhs   :: ExprX p }
  -- When matching may bind any of the ru_qvars, which are typed,
  -- just like any other lambda-bound variable

type Rule  = RuleX Parsed
type TRule = RuleX Typed

-----------------------------------------------
--  Simple functions over these types
-----------------------------------------------

partitionDecls :: [DeclX p] -> ([RuleX p], [DefX p])
-- Separate the Rules, Defs
--
-- See https://www.stackage.org/haddock/lts-12.1/base-4.11.1.0/src/Data-Either.html#partitionEithers
partitionDecls = foldr declX ([], [])
  where
    declX (RuleDecl r) = rule r
    declX (DefDecl  d) = def  d
    rule a ~(r, d) = (a:r, d)
    def  a ~(r, d) = (r, a:d)

-----------------------------------------------
--       Building values
-----------------------------------------------

mkPrimFun :: String -> Fun
mkPrimFun fname = Fun (PrimFun fname)

mkPrimTFun :: Type -> String -> TFun
mkPrimTFun ty fname = TFun ty $ mkPrimFun fname

mkVar :: String -> Var  -- Just a Simple var
mkVar = Simple

mkTVar :: TypeX -> String -> TVarX
mkTVar ty = TVar ty . mkVar

resVar :: Var
resVar = Simple "ksc$resVar"

argVar :: Var
argVar = Simple "ksc$argVar"

indexTVar :: TVar
indexTVar = TVar TypeSize (Simple "ksc$indexTVar")

mkArgVar :: Int -> Var
mkArgVar n = Simple ("ksc$argVar" ++ show n)

mkDummy :: Type -> TExpr
mkDummy ty = Dummy ty

mkLet :: LetBndrX p -> ExprX p -> ExprX p -> ExprX p
mkLet = Let

mkLets :: HasCallStack => [(LetBndrX p, ExprX p)] -> ExprX p -> ExprX p
mkLets xs e = foldr (uncurry mkLet) e xs

kInt :: Integer -> ExprX p
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

mkTuple :: [ExprX p] -> ExprX p
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
--  Finding the type of an expression
-----------------------------------------------

class HasType b where
  typeof :: HasCallStack => b -> Type

instance HasType TVar where
  typeof (TVar ty _) = ty

instance HasType TypedExpr where
  typeof (TE _ ty) = ty

instance HasType TFun where
  typeof (TFun ty _) = ty

instance HasType TExpr where
  typeof (Dummy ty)    = ty
  typeof (Konst k)     = typeofKonst k
  typeof (Var b)       = typeof b
  typeof (Call f _)    = typeof f
  typeof (App f _)     = case typeof f of
    TypeLam _ res -> res
    _ -> pprPanic "typeof:app " (vcat [ppr f, ppr (typeof f)])
  typeof (Tuple es)    = TypeTuple $ map typeof es
  typeof (Lam b e)     = TypeLam (typeof b) (typeof e)
  typeof (Let _ _ e2)  = typeof e2
  typeof (Assert _ e)  = typeof e
  typeof (If _ t f)    = makeIfType (typeof t) (typeof f)

instance HasType Type where
  typeof t = t

-- ToDo: delete this if no longer needed
makeIfType :: HasCallStack => Type -> Type -> Type
makeIfType ty1 ty2 = traceWhenUnequal "makeIfType" ty1 ty2 ty2

unzipLMType :: Type -> Maybe (Type, Type)
unzipLMType = \case
  TypeLM s t -> Just (s, t)
  _          -> Nothing

unzipLMTypes :: HasCallStack => [Type] -> Maybe ([Type], [Type])
unzipLMTypes [] = Just ([], [])
unzipLMTypes (lmt : lmts) = do
  (s, t) <- unzipLMType lmt
  (ss, ts) <- unzipLMTypes lmts
  return (s : ss, t : ts)

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

traceWhenUnequal :: (HasCallStack, Eq a, Pretty a) => String -> a -> a -> b -> b
traceWhenUnequal msg t1 t2
  | t1 == t2 = id
  | otherwise = trace (  "Note: unequal ["
                      ++ msg
                      ++ "] \n T1 = "
                      ++ pps t1
                      ++ "\n T2 = "
                      ++ pps t2
                      ++ "\n"
                      )

traceWhenTypesUnequal :: HasCallStack => String -> Type -> Type -> b -> b
traceWhenTypesUnequal = traceWhenUnequal


-----------------------------------------------
--     SDoc abstraction over expression display style
-----------------------------------------------

newtype SDoc = SDoc(Bool -> Doc) -- True = S-expressions, False = infix style

(<>) :: SDoc -> SDoc -> SDoc
SDoc d1 <> SDoc d2 = SDoc (\s -> d1 s PP.<> d2 s)

(<+>) :: SDoc -> SDoc -> SDoc
SDoc d1 <+> SDoc d2 = SDoc (\s -> d1 s PP.<+> d2 s)

($$) :: SDoc -> SDoc -> SDoc
SDoc d1 $$ SDoc d2 = SDoc (\s -> d1 s PP.$$ d2 s)

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
parens (SDoc d) = SDoc (PP.parens . d)

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

mode :: SDoc  -- How to print in s-expression style
     -> SDoc  -- How to print in "user" style
     -> SDoc
mode (SDoc se) (SDoc inf) = SDoc (\m -> if m then se m else inf m)

nest :: Int -> SDoc -> SDoc
nest i (SDoc d) = SDoc (PP.nest i . d)

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
braces (SDoc d) = SDoc (PP.braces . d)

brackets :: SDoc -> SDoc
brackets (SDoc d) = SDoc (PP.brackets . d)

doubleQuotes :: SDoc -> SDoc
doubleQuotes (SDoc d) = SDoc (PP.doubleQuotes . d)

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

class InPhase p where
  pprVar     :: VarX p -> SDoc      -- Just print it
  pprLetBndr :: LetBndrX p -> SDoc  -- Print with its type
  pprFunOcc  :: FunX p -> SDoc      -- Just print it

  getMType   :: MTypeX p   -> Maybe Type
  getVar     :: VarX p     -> (Var, Maybe Type)
  getFun     :: FunX p     -> (Fun, Maybe Type)
  getLetBndr :: LetBndrX p -> (Var, Maybe Type)

instance InPhase Parsed where
  pprVar     = ppr
  pprLetBndr = ppr
  pprFunOcc  = ppr

  getMType _      = Nothing
  getVar     var = (var, Nothing)
  getFun     fun = (fun, Nothing)
  getLetBndr var = (var, Nothing)

instance InPhase Typed where
  pprVar  = ppr
  pprLetBndr = pprTVar
  pprFunOcc  = ppr

  getMType ty              = Just ty
  getVar     (TVar ty var) = (var, Just ty)
  getFun     (TFun ty fun) = (fun, Just ty)
  getLetBndr (TVar ty var) = (var, Just ty)

instance InPhase OccAnald where
  pprVar  = ppr
  pprLetBndr (n,tv) = pprTVar tv <> braces (int n)
  pprFunOcc = ppr

  getMType   ty                 = Just ty
  getVar     (TVar ty var)      = (var, Just ty)
  getFun     (TFun ty fun)      = (fun, Just ty)
  getLetBndr (_, tv)            = (tVarVar tv,   Nothing)
  -- This last case is awkward. _ty :: TypeX OccAnald
  -- and we could convert it to a Type, but it does not
  -- see worth the bother.  Nothing is fine, actually

pprMTypeX :: forall p. InPhase p => MTypeX p -> SDoc
pprMTypeX mty = case getMType @p mty of
                 Just ty -> ppr ty
                 Nothing -> empty

pprTFun :: TFun -> SDoc
pprTFun (TFun ty f) = ppr f <+> text ":" <+> ppr ty


class Pretty a where
  ppr     :: a -> SDoc
  ppr = pprPrec precZero

  pprPrec :: Prec -> a -> SDoc
  pprPrec _ = ppr

instance Pretty Char where
  ppr = char

instance Pretty Int where
  ppr = int

instance Pretty Bool where
  ppr True  = text "True"
  ppr False = text "False"

instance Pretty a => Pretty (Maybe a) where
  ppr Nothing  = text "Nothing"
  ppr (Just x) = text "Just" <+> ppr x

instance (Pretty a, Pretty b) => Pretty (a,b) where
  ppr (x,y) = parens (sep [ ppr x <> comma, ppr y])

instance Pretty ADMode where
  ppr (AD p d) = ppr p <> ppr d

instance Pretty ADDir where
  ppr Fwd = char 'f'
  ppr Rev = char 'r'

instance Pretty ADPlan where
  ppr BasicAD = empty
  ppr TupleAD = char 't'

instance Pretty Var where
  ppr (Simple s) = text s
  ppr (Delta  d) = text "d$" <> text d
  ppr (Grad g m) = char 'g' <> ppr m <> char '$' <> text g

instance Pretty FunId where
  ppr = pprFunId

instance Pretty Fun where
  ppr = pprFun

pprFunId :: FunId -> SDoc
pprFunId (UserFun s ) = text s
pprFunId (PrimFun p ) = text p
pprFunId (SelFun i n) = text "get$" <> int i <> char '$' <> int n

pprFun :: Fun -> SDoc
pprFun (Fun s)                   = ppr s
pprFun (GradFun  s adp)          = char 'D'   <> ppr adp <> char '$' <> ppr s
pprFun (DrvFun   s (AD adp Fwd)) = text "fwd" <> ppr adp <> char '$' <> ppr s
pprFun (DrvFun   s (AD adp Rev)) = text "rev" <> ppr adp <> char '$' <> ppr s

instance Pretty TVar where
  pprPrec _ (TVar _ v) = ppr v

instance Pretty TFun where
  ppr (TFun _ f) = ppr f

instance Pretty Konst where
  pprPrec _ (KInteger i) = integer i
  pprPrec _ (KFloat f)   = double f
  pprPrec _ (KString s)  = text (show s)
  pprPrec _ (KBool b)    = text (case b of { True -> "true"; False -> "false" })

instance Pretty TypeX where
  pprPrec p (TypeVec ty)      = parensIf p precTyApp $
                                text "Vec" <+> pprParendType ty
  pprPrec _ (TypeTuple tys)   = mode (parens (text "Tuple" <+> pprList pprParendType tys))
                                     (parens (pprList pprParendType tys))
  pprPrec p (TypeLam from to) = parensIf p precZero $
                                text "Lam" <+> ppr from <+> ppr to
  pprPrec p (TypeLM s t)      = parensIf p precTyApp $ text "LM" <+> pprParendType s <+> pprParendType t
  pprPrec _ TypeFloat         = text "Float"
  pprPrec _ TypeInteger       = text "Integer"
  pprPrec _ TypeSize          = text "Size"
  pprPrec _ TypeString        = text "String"
  pprPrec _ TypeBool          = text "Bool"
  pprPrec _ TypeUnknown       = text "UNKNOWN"

pprParendType :: TypeX -> SDoc
pprParendType = pprPrec precTop

type Prec = Int
 -- 0 => no need for parens
 -- high => parenthesise everything

precZero, precOne, precTwo, precThree, precTyApp, precCall, precTop :: Int
precZero  = 0  -- Base
precOne   = 1  -- ==
precTwo   = 2  -- +
precThree = 3  -- *
precTyApp = 4
precCall  = 4
precTop   = 4

instance InPhase p => Pretty (ExprX p) where
  pprPrec = pprExpr

pprParendExpr :: InPhase p => ExprX p -> SDoc
pprParendExpr = pprExpr precTop

pprTVar :: TVarX -> SDoc
pprTVar (TVar ty v) = ppr v <+> text ":" <+> ppr ty

pprExpr :: forall phase. InPhase phase => Prec -> ExprX phase -> SDoc
pprExpr _ (Var   v ) = pprVar @phase v
pprExpr _ (Dummy ty) = char '<' <> pprMTypeX @phase ty <> char '>'
pprExpr p (Konst k ) = pprPrec p k
pprExpr p (Call f e) = pprCall p f e
pprExpr _ (Tuple es) = mode (parens $ text "tuple" <+> rest) (parens rest)
  where rest = pprList ppr es
pprExpr _ (Lam v e) =  mode (parens $ text "lam" <+> parens (pprTVar v) <+> ppr e)
                            (parens $ text "lam" <+> vcat [parens (pprTVar v), ppr e])
pprExpr p (Let v e1 e2) = mode
  (pprLetSexp v e1 e2)
  (parensIf
    p
    precZero
    (vcat
      [ text "let"
        <+> (bracesSp $ sep [pprLetBndr @phase v, nest 2 (text "=" <+> ppr e1)])
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

pprCall :: forall p. InPhase p => Prec -> FunX p -> ExprX p -> SDoc
pprCall prec f e = mode
  (parens $ pprFunOcc @p f <+> pp_args_tuple)
  (case (e, isInfix @p f) of
    (Tuple [e1, e2], Just prec')
      -> parensIf prec prec' $
         sep [pprExpr prec' e1, pprFunOcc @p f <+> pprExpr prec' e2]
    _ -> parensIf prec precCall $
         cat [pprFunOcc @p f, nest 2 (case e of { Tuple {} -> ppr e; _ -> parens (ppr e) })]
  )
 where
  pp_args = ppr e

  -- pp_args_tuple unpacks a literal tuple of arguments to multiple
  -- surface syntax arguments.  See Note [Function arity].
  pp_args_tuple = case e of
    Tuple [_] -> pp_args
    Tuple es  -> sep (map ppr es)
    _         -> pp_args

pprLetSexp :: forall p. InPhase p => LetBndrX p -> ExprX p -> ExprX p -> SDoc
pprLetSexp v e =
      go [(v,e)]
    where
      go binds (Let v1 e1 body) = go ((v1,e1):binds) body
      go binds body =
            parens $ sep [text "let", parens $ vcat (map parenBind $ reverse binds),
                        ppr body]
      parenBind (v,e) = parens $ (ppr . fst . getLetBndr @p) v <+> ppr e


isInfix :: forall p. InPhase p => FunX p ->  Maybe Prec
isInfix f = isInfixFun (fst (getFun @p f))

-- TODO: this is rather out of date, and it's not clear we need to keep it...
isInfixFun :: Fun -> Maybe Prec
isInfixFun (Fun (PrimFun s))
    | s == "eq"     = Just precOne
    | s == "ts_add" = Just precTwo
isInfixFun _ = Nothing

parensIf :: Prec -> Prec -> SDoc -> SDoc
parensIf ctxt inner doc | ctxt >= inner    = parens doc
                        | otherwise        = doc

instance InPhase p => Pretty (DeclX p) where
  ppr (DefDecl d)  = ppr d
  ppr (RuleDecl r) = ppr r

instance InPhase p => Pretty (DefX p) where
  ppr def = pprDef def

pprDef :: InPhase p => DefX p -> SDoc
pprDef (Def { def_fun = f, def_args = vs, def_res_ty = res_ty, def_rhs = rhs })
  = case rhs of
      EDefRhs -> parens $
                 sep [ text "edef", ppr f
                     , pprParendType res_ty
                     , parens ((pprParendType . tVarType) vs) ]

      UserRhs rhs -> mode
          (parens $ sep [ text "def", pprFun f <+> pprParendType res_ty
                        , parens ((parens . pprTVar) vs)
                        , ppr rhs])
          (sep [ hang (text "def" <+> pprFun f <+> pprParendType res_ty)
                    2 (parens (pprTVar vs))
               , nest 2 (text "=" <+> ppr rhs) ])

      StubRhs -> text "<<StubRhs>>"

instance InPhase p => Pretty (RuleX p) where
  ppr (Rule { ru_name = name, ru_qvars = qvars
            , ru_lhs = lhs, ru_rhs = rhs })
    = parens $ sep [ text "rule" <+> doubleQuotes (text name)
                 <+> parens (pprList pprTVar qvars)
             , nest 2 (sep [ ppr lhs, nest 2 (ppr rhs)]) ]

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
pprList ppr ps = mode (sep pps) (sep $ punctuate comma pps)
  where
   pps = map ppr ps

instance Pretty a => Pretty [a] where
  ppr xs = char '[' <> pprList ppr xs <> char ']'

pprTrace :: String -> SDoc -> a -> a
pprTrace str doc = trace (render (sep [text str, nest 2 doc]))

pprPanic :: HasCallStack => String -> SDoc -> a
pprPanic str doc = error (take 1000 $ render (sep [text str, nest 2 doc]))

pps :: Pretty a => a -> String
pps = show . ppr

hspec :: Spec
hspec = do
  let test e s = it s $ pps e `shouldBe` s

  let var s = Var (Simple s)
  let e,e2 :: Expr
      e  = Call (Fun (UserFun "g")) (var "i")
      e2 = Call (Fun (UserFun "f")) (Tuple [e, var "_t1", kInt 5])

  describe "Pretty" $ do
    test e  "g(i)"
    test e2 "f(g(i), _t1, 5)"

  describe "eqType" $
    it "doesn't truncate" (eqType (TypeTuple []) (TypeTuple [TypeFloat]) `shouldBe` False)

test_Pretty :: IO ()
test_Pretty = Test.Hspec.hspec Lang.hspec

-----------------------------------------------
--     Equality modulo alpha
-----------------------------------------------

instance Eq TExpr where
  e1 == e2 = case e1 `cmpExpr` e2 of
               EQ -> True
               _  -> False

instance Ord TExpr where
  compare = cmpExpr

thenCmp :: Ordering -> Ordering -> Ordering
EQ `thenCmp` o = o
o  `thenCmp` _ = o

cmpExpr :: TExpr -> TExpr -> Ordering
cmpExpr e1
 = go e1 M.empty
 where
   go :: TExpr -> M.Map Var TVar -> TExpr -> Ordering
   go (Dummy t1) _ e2
     = case e2 of
         Dummy t2 -> t1 `compare` t2
         _        -> LT

   go (Konst k1) _ e2
     = case e2 of
         Dummy {} -> GT
         Konst k2 -> k1 `compare` k2
         _        -> LT

   go (Var v1) subst e2
     = case e2 of
         Dummy {} -> GT
         Konst {} -> GT
         Var v2   -> v1 `compare` M.findWithDefault v2 (tVarVar v2) subst
         _        -> LT

   go (Call f1 e1) subst e2
     = case e2 of
         Dummy {} -> GT
         Konst {} -> GT
         Var {}   -> GT
         Call f2 e2 -> (f1 `compare` f2) `thenCmp` (go e1 subst e2)
         _ -> LT

   go (Tuple es1) subst e2
     = case e2 of
         Dummy {} -> GT
         Konst {} -> GT
         Var {}  -> GT
         Call {} -> GT
         Tuple es2 -> gos es1 subst es2
         _        -> LT

   go (Lam b1 e1) subst e2
      = case e2 of
         Dummy {} -> GT
         Konst {}  -> GT
         Var {}    -> GT
         Call {}   -> GT
         Tuple {}  -> GT
         Lam b2 e2 -> (typeof b1 `compare` typeof b2) `thenCmp`
                      go e1 (M.insert (tVarVar b2) b1 subst) e2
         _         -> LT

   go (App e1a e1b) subst e2
     = case e2 of
         Dummy {} -> GT
         Konst {} -> GT
         Var {}   -> GT
         Call {}  -> GT
         Tuple {} -> GT
         Lam {}   -> GT
         App e2a e2b -> go e1a subst e2a `thenCmp` go e1b subst e2b
         _           -> LT

   go (Let b1 r1 e1) subst e2
     = case e2 of
         If {}     -> LT
         Assert {} -> LT
         Let b2 r2 e2
           -> go r1 subst r2 `thenCmp`
              (typeof b1 `compare` typeof b2) `thenCmp`
              go e1 (M.insert (tVarVar b2) b1 subst) e2
         _ -> GT

   go (If e1c e1t e1f) subst e2
      = case e2 of
          Assert {} -> LT
          If e2c e2t e2f -> go e1c subst e2c `thenCmp`
                            go e1t subst e2t `thenCmp`
                            go e1f subst e2f
          _ -> GT

   go (Assert e1a e1b) subst e2
      = case e2 of
          Assert e2a e2b -> go e1a subst e2a `thenCmp` go e1b subst e2b
          _              -> GT

   gos :: [TExpr] -> M.Map Var TVar -> [TExpr] -> Ordering
   gos []    _ []    = EQ
   gos []    _ (_:_) = LT
   gos (_:_) _ []    = GT
   gos (e1:es1) subst (e2:es2) = go e1 subst e2 `thenCmp` gos es1 subst es2
