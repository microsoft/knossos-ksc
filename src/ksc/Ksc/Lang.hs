-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleInstances, LambdaCase,
             PatternSynonyms, StandaloneDeriving, AllowAmbiguousTypes,
             ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
-- The instances we derive need UndecidableInstances to be enabled
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}
-- Pattern match exhaustiveness checker seems to be broken on synonyms

module Ksc.Lang where

import           Prelude                 hiding ( (<>) )

import qualified Ksc.Traversal                  as T

import qualified Text.PrettyPrint              as PP
import           Text.PrettyPrint               ( Doc )
import           Data.List                      ( intersperse )
import           Ksc.KMonad

import           Data.Either                    ( partitionEithers )
import qualified Data.Map as M
import           Data.Maybe                     ( isJust, mapMaybe )
import           Debug.Trace                    ( trace )
import           Test.Hspec

-----------------------------------------------
--  The main data types
-----------------------------------------------

mkGradType :: Type -> Type -> Type
mkGradType s ty = TypeLM s ty
  -- For TupleAD, mkGradType s t = (t, s -o t)

mkGradTuple :: TExpr -> TExpr -> TExpr
mkGradTuple _ lm = lm

data Phase = Parsed | Typed | OccAnald

data DeclX p = RuleDecl (RuleX p)
             | DefDecl  (DefX p)
             | GDefDecl GDefX

deriving instance (Eq (RuleX p), Eq (DefX p)) => Eq (DeclX p)
deriving instance (Show (RuleX p), Show (DefX p)) => Show (DeclX p)

type Decl  = DeclX Parsed
type TDecl = DeclX Typed

data DefX p  -- f x = e
  = Def { def_fun    :: UserFun p
        , def_pat    :: Pat       -- See Note [Function arity]
        , def_res_ty :: TypeX     -- Result type
        , def_rhs    :: RhsX p }
  -- Definitions are user-annotated with argument types
  -- (via TVar) and result types (via TFun)

deriving instance (Eq (BaseArgTy p), Eq (RhsX p)) => Eq (DefX p)
deriving instance (Show (BaseArgTy p), Show (RhsX p)) => Show (DefX p)

type Def  = DefX Parsed
type TDef = DefX Typed

data RhsX p
  = UserRhs (ExprX p)   -- An ordinary definition with a right hand side

  | StubRhs             -- Used during recursion, or after a type error
                        --   to allow a Def to be added to the environment
                        --   without having a RHS

  | EDefRhs             -- An external definition: no RHS

deriving instance Eq (ExprX p) => Eq (RhsX p)
deriving instance Show (ExprX p) => Show (RhsX p)

type TRhs  = RhsX Typed

isUserDef :: DefX p -> Bool
isUserDef (Def { def_rhs = UserRhs {} }) = True
isUserDef _ = False

data Derivation = DerivationDrvFun ADMode
                | DerivationCLFun
                | DerivationShapeFun
                | DerivationSUFFwdPass
                | DerivationSUFRevPass
                | DerivationSUFRev
  deriving ( Show, Eq, Ord )

data GDefX = GDef Derivation (UserFun Typed)
  deriving ( Show, Eq, Ord )

data PatG v = VarPat v     -- A single variable
            | TupPat [v]   -- A tuple of variables
            deriving( Eq, Functor, Foldable, Traversable )

type Pat = PatG TVar

patTypeG :: forall p. InPhase p => PatG (LetBndrX p) -> Maybe Type
patTypeG (VarPat v)  = snd (getLetBndr @p v)
patTypeG (TupPat vs) = TypeTuple <$> mapM (snd . getLetBndr @p) vs

patType :: Pat -> Type
patType (VarPat v) = typeof v
patType (TupPat vs) = mkTupleTy (map typeof vs)

patVars :: PatG v -> [v]
patVars (VarPat v) = [v]
patVars (TupPat vs) = vs

patToExpr :: Pat -> TExpr
patToExpr (VarPat v)  = Var v
patToExpr (TupPat vs) = Tuple (map Var vs)

instance Show Pat where
  show p = pps p

data TVarX = TVar TypeX Var deriving (Eq, Ord)
type TVar = TVarX

instance Show TVar where
  show e = pps e

tVarVar :: TVarX -> Var
tVarVar (TVar _ v) = v

tVarType :: TVarX -> TypeX
tVarType (TVar ty _) = ty

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
        (let ((x1 ... xn) x) ...))

and the surface syntax

    (f e1 ... en)

means the same thing as

    (f (tuple e1 ... en))

if n /= 1.  Nested unpacking is not supported, yet.

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
  FunX Parsed   = Fun Parsed
  FunX Typed    = TFun Typed
  FunX OccAnald = TFun OccAnald

data ExprX p
  = Konst Konst
  | Var  (VarX p)
  | Call (FunX p) (ExprX p)  -- f e
  | Tuple [ExprX p]          -- (e1, ..., en)
  | Lam TVarX (ExprX p)      -- Lambda-bound variable is typed from birth
  | App (ExprX p) (ExprX p)
  | Let (PatG (LetBndrX p)) (ExprX p) (ExprX p)
  -- ^ let x = e1 in e2
  --   or let (x1, ..., x2) = e1 in e2
  --   (both non-recursive)
  | If (ExprX p) (ExprX p) (ExprX p)
  | Assert (ExprX p) (ExprX p)
  | Dummy Type

deriving instance Eq (ExprX Parsed)
deriving instance Eq (ExprX OccAnald)

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

  | TypeTensor Int TypeX

  | TypeLam TypeX TypeX  -- Domain -> Range
  | TypeLM  TypeX TypeX   -- Linear map  Src -o Target

  | TypeUnknown

deriving instance Eq  Type
deriving instance Ord Type

instance Show TypeX where
  show e = pps e

----------------------------------
--- Tensor properties
----------------------------------

{- Note [Size and index types of tensors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally, tensor elements are indexed by tuples of integers,
with the size of the tuple being the number of dimensions of
the tensor. The same type (a tuple of integers) is used to
represent the size of the tensor.

The 1-dimensional case is special because we avoid the
use of 1-tuples. In this case, the index/size type is a
plain integer.
-}

tensorIndexType :: Int -> Type
tensorIndexType 1 = TypeInteger
tensorIndexType d = TypeTuple (replicate d TypeInteger)

tensorDimensionFromIndexType_maybe :: Type -> Maybe Int
tensorDimensionFromIndexType_maybe TypeInteger = Just 1
tensorDimensionFromIndexType_maybe (TypeTuple ts)
  | all (`eqType` TypeInteger) ts
  = Just (length ts)
tensorDimensionFromIndexType_maybe _ = Nothing

isTensorIndexType :: Type -> Bool
isTensorIndexType = isJust . tensorDimensionFromIndexType_maybe

tensorTypeFromIndexType_maybe :: Type -> Type -> Maybe Type
tensorTypeFromIndexType_maybe indexType elementType =
  fmap (\d -> TypeTensor d elementType) (tensorDimensionFromIndexType_maybe indexType)

zeroIndexForDimension :: Int -> TExpr
zeroIndexForDimension 1 = kInt 0
zeroIndexForDimension d = mkTuple (replicate d (kInt 0))


----------------------------------
--- Tangent space

tangentType :: HasCallStack => Type -> Type
-- We can't differentiate Integer, Bool etc.
tangentType TypeFloat      = TypeFloat
tangentType (TypeTensor d t) = TypeTensor d (tangentType t)
tangentType (TypeTuple ts) = TypeTuple (map tangentType ts)
tangentType TypeInteger    = TypeTuple []
tangentType TypeBool       = TypeTuple []
tangentType TypeString     = TypeTuple []
tangentType TypeUnknown    = TypeUnknown
tangentType t              = pprPanic "tangentType" (ppr t)
                               -- TypeLM, TypeLam

-----------------------------------
--- Shapes
-----------------------------------

uncompressedShapeType :: Type -> Type
uncompressedShapeType TypeBool = TypeTuple []
uncompressedShapeType TypeInteger = TypeTuple []
uncompressedShapeType TypeFloat = TypeTuple []
uncompressedShapeType TypeString = TypeTuple []
uncompressedShapeType (TypeTuple ts) = TypeTuple (map uncompressedShapeType ts)
uncompressedShapeType (TypeTensor d vt) = TypeTensor d (uncompressedShapeType vt)
uncompressedShapeType (TypeLam _ _) = TypeUnknown
uncompressedShapeType (TypeLM _ _) = TypeUnknown  -- TBD
uncompressedShapeType TypeUnknown = TypeUnknown

{- Note [Uncompressed shapes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here Sh(T) is the uncompressed shape-type of T (defined analogously to
the tangent-type of T), and is implemented by uncompressedShapeType ::
Type -> Type.  We intend to replace uncompressed shape-types with
compressed shape-types.

For example
  --------------------------
  T           Sh(T)
  --------------------------
  Float       ()
  Tensor N T  Tensor N Sh(T)
  (T1 T2)     (Sh(T1), Sh(T2))
  --------------------------

-}


-- eqType is currently == but coud be more complicated if we do size
-- types
eqType :: Type -> Type -> Bool
eqType = (==)

eqTypes :: Type -> [Type] -> Maybe Type
eqTypes x xs = if all (eqType x) xs
               then Just x
               else Nothing

-----------------------------------
--- Functions
-----------------------------------

{- Note [Global function ad-hoc overloading]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Whenever we use the name of a global function in a call, what we refer
to internally is a 'Fun'.  Indeed each global function is uniquely
identified by a 'Fun'.  The internal structure of a 'Fun' is:

* Each 'Fun' is a derivation from a base function ('BaseFun')

  See definition of 'type Fun'

* A base function is either a primitive ('PrimFun') or a base user
  function ('BaseUserFun')

* A base user function is a pair of name (e.g. 'mul') and argument type
  (e.g. (Float, Float))

  See definition of 'data BaseUserFun'.  The reason we need a (name, type)
  pair is that distinct global functions can have the same name but
  different argument type. See below.

* A derivation is either trivial (i.e. no derivation) or one of a
  collection of hardcoded derivations, for example 'rev' or 'shape'.

To write the name of a (non-trivial) derived function we supply the
name of the derivation before the name of the function in square
brackets, for example '[rev f]' or '[shape f]'.

Many global functions can be called 'mul' or '[rev mul]', provided the
base argument types differ.  Indeed you can see this in
src/runtime/prelude.ks.  Functions with the same name, same base
argument type but differing return types are *not* allowed.

To be explicit about the identity of a function we must provide the
type of the base function.  We do so by writing the type of the base
function after the name of the base function, in square brackets .
For example, if there are two functions called 'f', one of which has
Integer argument type and one of which has Float argument type, we can
disambiguate by writing '[f Integer]' and '[f Float]' respectively.
Likewise we can disambiguate '[rev f]' by writing '[rev [f Integer]]'
or '[rev [f Float]]' as appropriate.

The global symbol table is keyed by 'UserFun's, that is the subset of
'Fun's which are neither primitive nor tuple selector functions.

All of this would go out of the window if we had polymorphism, because
then the argument type could be a type variable.  But we don't!

Moreover, for type inference, we can work out the type of the
arguments before looking up the function to find its result type.
That would fail if were were inferring the type of a recursive
function -- but fortunately all functions have explicitly-declared
types.

-}

data BaseFunId name (p :: Phase) = BaseFunId name (BaseArgTy p)

type family BaseArgTy p where
  BaseArgTy Parsed   = Maybe Type
  BaseArgTy OccAnald = Type
  BaseArgTy Typed    = Type

data BaseName = BaseUserFunName String   -- BaseUserFuns have a Def
              | BasePrimFunName PrimFun  -- PrimFuns do not have a Def
              deriving (Eq, Ord, Show)

type BaseFun p     = BaseFunId BaseName p
type BaseUserFun p = BaseFunId String p
type BasePrimFun p = BaseFunId PrimFun p

deriving instance (Eq   name, Eq   (BaseArgTy p)) => Eq   (BaseFunId name p)
deriving instance (Ord  name, Ord  (BaseArgTy p)) => Ord  (BaseFunId name p)
deriving instance (Show name, Show (BaseArgTy p)) => Show (BaseFunId name p)

-- The purposes of this pattern synonym is to avoid churn.  We can
-- retain the functionality of the old "PrimFun" constructor by using
-- "PrimFunT" instead.
pattern PrimFunT :: forall (p :: Phase). PrimFun -> BaseFun p
pattern PrimFunT p <- BaseFunId (BasePrimFunName p) _

data Derivations
  = JustFun        -- The function              f(x)
  | GradFun        -- Full Jacobian Df(x)
  | DrvFun ADMode  -- Derivative derivative f'(x,dx)
                   --   Rev <=> reverse mode f`(x,dr)
  | CLFun          -- f(x), roundtripped through CatLang
  | ShapeFun Derivations
  | SUFFwdPass
  | SUFRevPass
  | SUFRev
  deriving (Eq, Ord, Show)

data DerivedFun funname p = Fun Derivations (BaseFunId funname p)

deriving instance Eq (BaseFunId funname p)   => Eq   (DerivedFun funname p)
deriving instance Ord (BaseFunId funname p)  => Ord  (DerivedFun funname p)
deriving instance Show (BaseFunId funname p) => Show (DerivedFun funname p)

-- DerivedFun has just two instantiations
--
-- Fun p: these you can call hence are in the function field of a Call (in
-- the guise of a TFun)
--
-- UserFun p: These you can def/edef and hence are the domain of the
-- GblSymTab and CST and appear in the def_fun field of DefX.
type UserFun p = DerivedFun String p
type Fun     p = DerivedFun BaseName p

baseFunT :: T.Lens (BaseFunId a p) (BaseFunId a q)
                   (BaseArgTy p) (BaseArgTy q)
baseFunT g (BaseFunId n ty) = BaseFunId n <$> g ty

baseFunFun :: T.Lens (DerivedFun funname p) (DerivedFun funname' p')
                     (BaseFunId funname p) (BaseFunId funname' p')
baseFunFun f (Fun ds fi) = fmap (Fun ds) (f fi)

baseFunName :: T.Lens (BaseFunId n p) (BaseFunId n' p) n n'
baseFunName f (BaseFunId n ty) = flip BaseFunId ty <$> f n

funBaseType :: forall a p. InPhase p
            => T.Lens (DerivedFun a p) (DerivedFun a Typed)
                      (Maybe Type) Type
funBaseType = funType . baseUserFunArgTy @p

funType :: T.Lens (DerivedFun a p) (DerivedFun a q) (BaseArgTy p) (BaseArgTy q)
funType = baseFunFun . baseFunT

-- In the Parsed phase, if the user didn't supply a type, add it;
-- otherwise (and in other phases, where the type is there) check that
-- the type matches.  If mis-match return (Left
-- type-that-was-in-DerivedFun)
addBaseTypeToFun :: InPhase p
                 => DerivedFun name p
                 -> Type
                 -> Either Type (DerivedFun name Typed)
addBaseTypeToFun userfun expectedBaseTy = T.traverseOf funBaseType checkBaseType userfun
  where checkBaseType :: Maybe Type -> Either Type Type
        checkBaseType maybeAppliedType
          | Just appliedTy <- maybeAppliedType
          , not (eqType appliedTy expectedBaseTy)
          = Left appliedTy
          | otherwise
          = Right expectedBaseTy


userFunToFun :: UserFun p -> Fun p
userFunToFun = T.over (baseFunFun . baseFunName) BaseUserFunName

-- A 'Fun p' is Either:
--
-- Right: a 'UserFun p', or
-- Left:  a 'PrimFun'
perhapsUserFun :: Fun p -> Either (DerivedFun PrimFun p) (UserFun p)
perhapsUserFun (Fun ds baseFun) = case baseFun of
  BaseFunId (BaseUserFunName u) ty -> Right (Fun ds (BaseFunId u ty))
  BaseFunId (BasePrimFunName p) ty -> Left  (Fun ds (BaseFunId p ty))

maybeUserFun :: Fun p -> Maybe (UserFun p)
maybeUserFun f = case perhapsUserFun f of
  Right f -> Just f
  Left _ -> Nothing

isSelFun :: BaseFun p -> Bool
isSelFun = \case
  PrimFunT (P_SelFun{}) -> True
  _ -> False

baseFunOfFun :: Fun p -> BaseFun p
baseFunOfFun (Fun _ baseFun) = baseFun

data ADMode = AD { adDir :: ADDir }
  deriving( Eq, Ord, Show )

data ADDir = Fwd | Rev
  deriving( Eq, Ord, Show )

data TFun p = TFun Type (Fun p)   -- Typed functions.  The type is the /return/

deriving instance Eq (Fun p) => Eq (TFun p)
deriving instance Ord (Fun p) => Ord (TFun p)

-- Morally this is just 'coerce' but I don't know how to persuade
-- GHC's machinery to allow that.
coerceTFun :: BaseArgTy p ~ BaseArgTy q
           => TFun p -> TFun q
coerceTFun (TFun t f) = TFun t (T.over funType id f)


data Var
  = Simple String         -- x
  | Delta  String         -- The 'dx' or 'dr' argument to
                          -- forward or backward versions of f
  | Grad   String         -- Derivative of x
  deriving( Eq, Ord, Show )

nameOfVar :: Var -> String
nameOfVar = \case
  Simple s -> s
  Delta s  -> s
  Grad s   -> s

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

deriving instance Eq (ExprX p) => Eq (RuleX p)
deriving instance Show (ExprX p) => Show (RuleX p)

type Rule  = RuleX Parsed
type TRule = RuleX Typed

-----------------------------------------------
--  Simple functions over these types
-----------------------------------------------

partitionDeclsE :: [DeclX p] -> ([RuleX p], [Either (DefX p) GDefX])
partitionDeclsE = partitionEithers . map f
  where
    f (RuleDecl r) = Left r
    f (DefDecl  d) = Right (Left d)
    f (GDefDecl g) = Right (Right g)

partitionDecls :: [DeclX p] -> ([RuleX p], [DefX p])
-- Separate the Rules, Defs
partitionDecls decls = (r, mapMaybe keepDef dg)
  where (r, dg) = partitionDeclsE decls
        keepDef = \case
          Left d  -> Just d
          Right _ -> Nothing

-----------------------------------------------
--       Building values
-----------------------------------------------

mkVar :: String -> Var  -- Just a Simple var
mkVar = Simple

mkTVar :: TypeX -> String -> TVarX
mkTVar ty = TVar ty . mkVar

resVar :: Var
resVar = Simple "ksc$resVar"

argVar :: Var
argVar = Simple "ksc$argVar"

indexTVar :: Int -> TVar
indexTVar d = TVar (tensorIndexType d) (Simple "ksc$indexTVar")

mkArgVar :: Int -> Var
mkArgVar n = Simple ("ksc$argVar" ++ show n)

mkDummy :: Type -> TExpr
mkDummy ty = Dummy ty

mkLet :: LetBndrX p -> ExprX p -> ExprX p -> ExprX p
mkLet = Let . VarPat

mkLets :: HasCallStack => [(LetBndrX p, ExprX p)] -> ExprX p -> ExprX p
mkLets xs e = foldr (uncurry mkLet) e xs

kInt :: Integer -> ExprX p
kInt i = Konst (KInteger i)

kFloat :: Double -> ExprX p
kFloat f = Konst (KFloat f)

zeroFloat :: TExpr
zeroFloat = kFloat 0

mkTuple :: [ExprX p] -> ExprX p
mkTuple [e] = e
mkTuple es  = Tuple es

mkTupleTy :: [Type] -> Type
mkTupleTy [t] = t
mkTupleTy ts  = TypeTuple ts

mkPat :: [VarX p] -> PatG (VarX p)
mkPat [v] = VarPat v
mkPat vs  = TupPat vs

dropLast :: [a] -> [a]
-- Drop the last element of a list.
-- No-op for empty list
dropLast xs = take (length xs - 1) xs

pSel :: Int -> Int -> TExpr -> TExpr
pSel i n e = Call (TFun el_ty
                        (Fun JustFun (BaseFunId (BasePrimFunName (P_SelFun i n)) (typeof e)))) e
           where
             el_ty = case typeof e of
                        TypeTuple ts -> ts !! (i-1)
                        _ -> TypeUnknown  -- Better error from Lint

pFst,pSnd :: TExpr -> TExpr
pFst   = pSel 1 2
pSnd   = pSel 2 2

-----------------------------------------------
--  Finding the type of an expression
-----------------------------------------------

class HasType b where
  typeof :: HasCallStack => b -> Type

instance HasType TVar where
  typeof tv = tVarType tv

instance HasType Pat where
  typeof pat = patType pat

instance HasType TypedExpr where
  typeof (TE _ ty) = ty

instance HasType (TFun p) where
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
  pprNameAndBaseArgTy :: SDoc -> BaseArgTy p -> SDoc

  getVar     :: VarX p     -> (Var, Maybe Type)
  getFun     :: FunX p     -> (Fun Parsed, Maybe Type)
  getLetBndr :: LetBndrX p -> (Var, Maybe Type)

  baseUserFunArgTy :: T.Lens (BaseArgTy p) (BaseArgTy Typed)
                             (Maybe Type) Type

instance InPhase Parsed where
  pprVar     = ppr
  pprLetBndr = ppr
  pprFunOcc  = ppr
  pprNameAndBaseArgTy name mty = case mty of
    Nothing -> name
    Just ty -> brackets (name <+> pprParendType ty)

  getVar     var = (var, Nothing)
  getFun     fun = (fun, Nothing)
  getLetBndr var = (var, Nothing)

  baseUserFunArgTy = id

instance InPhase Typed where
  pprVar  = ppr
  pprLetBndr = pprTVar
  pprFunOcc  = ppr
  pprNameAndBaseArgTy name ty = brackets (name <+> pprParendType ty)

  getVar     (TVar ty var) = (var, Just ty)
  getFun     (TFun ty fun) = (fun', Just ty)
    where fun' = T.over funType Just fun
  getLetBndr (TVar ty var) = (var, Just ty)

  baseUserFunArgTy g t = g (Just t)

instance InPhase OccAnald where
  pprVar  = ppr
  pprLetBndr (n,tv) = pprTVar tv <> braces (int n)
  pprFunOcc = ppr
  pprNameAndBaseArgTy name ty = brackets (name <+> pprParendType ty)

  getVar     (TVar ty var)      = (var, Just ty)
  getFun     (TFun ty fun)      = (fun', Just ty)
    where fun' = T.over funType Just fun
  getLetBndr (_, TVar ty var)   = (var, Just ty)

  baseUserFunArgTy g t = g (Just t)

pprTFun :: InPhase p => TFun p -> SDoc
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
  ppr (AD p) = ppr p

instance Pretty ADDir where
  ppr Fwd = char 'f'
  ppr Rev = char 'r'

instance Pretty Var where
  ppr (Simple s) = text s
  ppr (Delta  d) = text "d$" <> text d
  ppr (Grad   g) = char 'g' <> char '$' <> text g

instance InPhase p => Pretty (BaseFun p) where
  ppr = pprBaseFun

instance Pretty (BaseFunId funid p) => Pretty (DerivedFun funid p) where
  ppr = pprDerivedFun ppr

instance Pretty PrimFun where
  ppr = pprPrimFun

instance Pretty BaseName where
  ppr = \case
    BaseUserFunName s -> text s
    BasePrimFunName p -> ppr p

pprBaseFun :: forall p. InPhase p => BaseFun p -> SDoc
pprBaseFun (BaseFunId (BaseUserFunName s) ty) = pprBaseUserFun @p (BaseFunId s ty)
pprBaseFun (BaseFunId (BasePrimFunName p) ty) = pprBasePrimFun @p (BaseFunId p ty)

pprBaseUserFun :: forall p. InPhase p => BaseUserFun p -> SDoc
pprBaseUserFun (BaseFunId name ty) = pprNameAndBaseArgTy @p (text name) ty

pprBasePrimFun :: BasePrimFun p -> SDoc
pprBasePrimFun (BaseFunId name _) = pprPrimFun name

pprPrimFun :: PrimFun -> SDoc
pprPrimFun = \case
  P_inline -> text "$inline"
  P_copydown -> text "$copydown"
  P_check -> text "$check"
  P_trace -> text "$trace"
  P_print -> text "print"
  P_Vec_init -> text "Vec_init"
  P_build -> text "build"
  P_sumbuild -> text "sumbuild"
  P_buildFromSparse -> text "buildFromSparse"
  P_buildFromSparseTupled -> text "buildFromSparseTupled"
  P_fold -> text "fold"
  P_map -> text "map"
  P_map2 -> text "map2"
  P_index -> text "index"
  P_shape -> text "shape"
  P_size -> text "size"
  P_sum -> text "sum"
  P_unzip -> text "unzip"
  P_ts_neg -> text "ts_neg"
  P_ts_add -> text "ts_add"
  P_ts_scale -> text "ts_scale"
  P_ts_dot -> text "ts_dot"
  P_eq -> text "eq"
  P_ne -> text "ne"
  P_delta -> text "delta"
  P_deltaVec -> text "deltaVec"
  P_diag -> text "diag"
  P_constVec -> text "constVec"
  P_lmApply -> text "lmApply"
  P_lmApplyR -> text "lmApplyR"
  P_lmApplyT -> text "lmApplyT"
  P_lmVCat -> text "lmVCat"
  P_lmHCat -> text "lmHCat"
  P_lmVCatV -> text "lmVCatV"
  P_lmHCatV -> text "lmHCatV"
  P_lmCompose -> text "lmCompose"
  P_lmAdd -> text "lmAdd"
  P_lmScale -> text "lmScale"
  P_lmScaleR -> text "lmScaleR"
  P_lmDot -> text "lmDot"
  P_lmZero -> text "lmZero"
  P_lmOne -> text "lmOne"
  P_lmApplyTR -> text "lmApplyTR"
  P_lmDummyFold -> text "P_lmDummyFold"
  P_lmFold -> text "P_lmFold"
  P_FFold -> text "FFold"
  P_RFold -> text "RFold"
  P_lmVariant -> text "P_lmVariant"

  P_SelFun i n -> text "get$" <> int i <> char '$' <> int n
  P_dup n -> text "dup$" <> int n
  P_elim -> text "elim"

  P_suffwdpass_map -> text "suffwdpass_map"
  P_sufrevpass_map -> text "sufrevpass_map"

pprUserFun :: forall p. InPhase p => UserFun p -> SDoc
pprUserFun = pprDerivedFun (pprBaseUserFun @p)

pprDerivedFun :: (BaseFunId funid p -> SDoc) -> DerivedFun funid p -> SDoc
pprDerivedFun f (Fun JustFun s)               = f s
pprDerivedFun f (Fun GradFun   s)             = brackets (char 'D'   <+> f s)
pprDerivedFun f (Fun (DrvFun   (AD Fwd)) s)   = brackets (text "fwd" <+> f s)
pprDerivedFun f (Fun (DrvFun   (AD Rev)) s)   = brackets (text "rev" <+> f s)
pprDerivedFun f (Fun (ShapeFun ds) sf)             = brackets (text "shape" <+> pprDerivedFun f (Fun ds sf))
pprDerivedFun f (Fun CLFun    s)              = brackets (text "CL" <+> f s)
pprDerivedFun f (Fun SUFFwdPass s)            = brackets (text "suffwdpass" <+> f s)
pprDerivedFun f (Fun SUFRevPass s)            = brackets (text "sufrevpass" <+> f s)
pprDerivedFun f (Fun SUFRev s)                = brackets (text "sufrev" <+> f s)

instance Pretty Pat where
  pprPrec _ p = pprPat True p

instance Pretty TVar where
  pprPrec _ (TVar _ v) = ppr v

instance InPhase p => Pretty (TFun p) where
  ppr (TFun _ f) = ppr f

instance Pretty Konst where
  pprPrec _ (KInteger i) = integer i
  pprPrec _ (KFloat f)   = double f
  pprPrec _ (KString s)  = text (show s)
  pprPrec _ (KBool b)    = text (case b of { True -> "true"; False -> "false" })

instance Pretty TypeX where
  pprPrec p (TypeTensor 1 ty) = parensIf p precTyApp $    -- TODO: remove this special case once other Knossos components understand Tensor
                                text "Vec" <+> pprParendType ty
  pprPrec p (TypeTensor d ty) = parensIf p precTyApp $
                                text "Tensor" <+> int d <+> pprParendType ty
  pprPrec _ (TypeTuple tys)   = mode (parens (text "Tuple" <+> pprList pprParendType tys))
                                     (parens (pprList pprParendType tys))
  pprPrec p (TypeLam from to) = parensIf p precZero $
                                text "Lam" <+> pprParendType from <+> pprParendType to
  pprPrec p (TypeLM s t)      = parensIf p precTyApp $ text "LM" <+> pprParendType s <+> pprParendType t
  pprPrec _ TypeFloat         = text "Float"
  pprPrec _ TypeInteger       = text "Integer"
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
pprExpr _ (Dummy ty) = parens $ text "$dummy" <+> pprParendType ty
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
        <+> (bracesSp $ sep [pprPatLetBndr @phase v, nest 2 (text "=" <+> ppr e1)])
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
  (parensIf prec precCall $
         cat [pprFunOcc @p f, nest 2 (parensSp pp_args)]
  )
 where
  pp_args = ppr e

  -- pp_args_tuple unpacks a literal tuple of arguments to multiple
  -- surface syntax arguments.  See Note [Function arity].
  pp_args_tuple = case e of
    Tuple [_] -> pp_args
    Tuple es  -> sep (map ppr es)
    _         -> pp_args

pprLetSexp :: forall p. InPhase p
           => PatG (LetBndrX p) -> ExprX p -> ExprX p -> SDoc
pprLetSexp v e body =
      parens $ vcat [ text "let" <+> (parens $ pprPatLetBndr @p v <+> ppr e),
                      ppr body]


parensIf :: Prec -> Prec -> SDoc -> SDoc
parensIf ctxt inner doc | ctxt >= inner    = parens doc
                        | otherwise        = doc

instance InPhase p => Pretty (DeclX p) where
  ppr (DefDecl d)  = ppr d
  ppr (RuleDecl r) = ppr r
  ppr (GDefDecl g) = ppr g

instance InPhase p => Pretty (DefX p) where
  ppr def = pprDef def

instance Pretty GDefX where
  ppr (GDef d f) = parens (text "gdef" <+> ppr d <+> ppr f)

instance Pretty Derivation where
  ppr = \case
    DerivationDrvFun (AD Fwd) -> text "fwd"
    DerivationDrvFun (AD Rev) -> text "rev"
    DerivationCLFun    -> text "CL"
    DerivationShapeFun -> text "shape"
    DerivationSUFFwdPass -> text "suffwdpass"
    DerivationSUFRevPass -> text "sufrevpass"
    DerivationSUFRev     -> text "sufrev"

pprDef :: forall p. InPhase p => DefX p -> SDoc
pprDef (Def { def_fun = f, def_pat = vs, def_res_ty = res_ty, def_rhs = rhs })
  = case rhs of
      EDefRhs -> parens $
                 sep [ text "edef", ppr f
                     , pprParendType res_ty
                     , parens (pprParendType (typeof vs)) ]

      UserRhs rhs -> mode
          (parens $ sep [ text "def", pprFun_f <+> pprParendType res_ty
                        , parens (pprPat False vs)
                        , ppr rhs])
          (sep [ hang (text "def" <+> pprFun_f <+> pprParendType res_ty)
                    2 (parens (pprPat False vs))
               , nest 2 (text "=" <+> ppr rhs) ])

      StubRhs -> text "<<StubRhs>>"

    where pprFun_f = pprUserFun @p f

instance InPhase p => Pretty (BaseUserFun p) where
  ppr = pprBaseUserFun

instance InPhase p => Pretty (BasePrimFun p) where
  ppr = pprBasePrimFun

pprPat :: Bool -> Pat -> SDoc
          -- True <=> wrap tuple pattern in parens
pprPat _          (VarPat v)  = pprTVar v
pprPat tup_parens (TupPat vs) = mb_parens $ pprList (parens . pprTVar) vs
  where
    mb_parens d | tup_parens = parens d
                | otherwise  = d

pprPatLetBndr :: forall phase. InPhase phase => PatG (LetBndrX phase) -> SDoc
pprPatLetBndr (VarPat v)  = (ppr . fst . getLetBndr @phase) v
pprPatLetBndr (TupPat vs) = parens (pprList (ppr . fst . getLetBndr @phase) vs)

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
pprPanic str doc = error (render (sep [text str, nest 2 doc]))

pps :: Pretty a => a -> String
pps = show . ppr

hspec :: Spec
hspec = do
  let test e s = it s $ pps e `shouldBe` s

  let var s = Var (Simple s)
  let e,e2 :: Expr
      e  = Call (Fun JustFun (BaseFunId (BaseUserFunName "g") Nothing)) (var "i")
      e2 = Call (Fun JustFun (BaseFunId (BaseUserFunName "f") Nothing)) (Tuple [e, var "_t1", kInt 5])

  describe "Pretty" $ do
    test e  "g( i )"
    test e2 "f( (g( i ), _t1, 5) )"

  describe "eqType" $
    it "doesn't truncate" (eqType (TypeTuple []) (TypeTuple [TypeFloat]) `shouldBe` False)

test_Pretty :: IO ()
test_Pretty = Test.Hspec.hspec Ksc.Lang.hspec

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
         Let b2 r2 e2 -> case (b1, b2) of
           (VarPat _,  TupPat _)  -> LT
           (TupPat _,  VarPat _)  -> GT
           (VarPat v1, VarPat v2) ->
             go r1 subst r2 `thenCmp`
             (typeof v1 `compare` typeof v2) `thenCmp`
             go e1 (M.insert (tVarVar v1) v2 subst) e2
           (TupPat t1, TupPat t2) ->
             -- These annotations "@[]" are optional and serve to
             -- inform us that we're taking the length of genuine
             -- lists here.  There's no funny business going on via
             -- the Foldable type class.
             case compare (length @[] t1) (length @[] t2) of
               LT -> LT
               GT -> GT
               EQ ->
                 go r1 subst r2 `thenCmp`
                 (map typeof t1 `compare` map typeof t2) `thenCmp`
                 go e1 (M.fromList
                         (zipWith (\tv1 tv2 -> (tVarVar tv1, tv2)) t1 t2)
                         `M.union` subst) e2

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

{- Note [PrimFun]
~~~~~~~~~~~~~~~~~

A PrimFun is an entity that can be called as a function but (unlike a
UserFun) it has its own type checking rule rather than having a type
per se (See Prim.hs).  This means that, unlike UserFuns which are
monomorphic, PrimFuns can be "polymorphic".  That is, in fact, the
only reason we have them.  If we were to implement polymorphism in ksc
then we would no longer need PrimFuns at all.

-}

data PrimFun = P_inline
             | P_copydown
             | P_check
             | P_trace
             | P_print
             | P_Vec_init
             | P_build
             | P_sumbuild
             | P_buildFromSparse
             | P_buildFromSparseTupled
             | P_map
             | P_map2
             | P_fold
             | P_index
             | P_shape
             | P_size
             | P_sum
             | P_unzip
             | P_ts_neg
             | P_ts_add
             | P_ts_scale
             | P_ts_dot
             | P_eq
             | P_ne
             | P_delta
             | P_deltaVec
             | P_diag
             | P_constVec
             | P_lmApply
             | P_lmApplyR
             | P_lmApplyT
             | P_lmVCat
             | P_lmHCat
             | P_lmVCatV
             | P_lmHCatV
             | P_lmCompose
             | P_lmAdd
             | P_lmScale
             | P_lmScaleR
             | P_lmDot
             | P_lmZero
             | P_lmOne
             | P_SelFun Int Int -- P_SelFun index arity.  1-indexed, so (SelFun 1 2) is fst
             | P_dup Int
             | P_elim
             | P_lmApplyTR
             | P_lmFold
             | P_FFold
             | P_RFold
             | P_lmDummyFold
             | P_lmVariant
             | P_suffwdpass_map
             | P_sufrevpass_map
  deriving (Show, Ord, Eq)

toPrimFun :: String -> Maybe PrimFun
toPrimFun = \case
  "$inline" -> Just P_inline
  "$copydown"-> Just P_copydown
  "$check" -> Just P_check
  "$trace" -> Just P_trace
  "print" -> Just P_print
  "Vec_init" -> Just P_Vec_init
  "build" -> Just P_build
  "sumbuild" -> Just P_sumbuild
  "buildFromSparse" -> Just P_buildFromSparse
  "buildFromSparseTupled" -> Just P_buildFromSparseTupled
  "fold" -> Just P_fold
  "map" -> Just P_map
  "map2" -> Just P_map2
  "index" -> Just P_index
  "shape" -> Just P_shape
  "size" -> Just P_size
  "sum" -> Just P_sum
  "unzip" -> Just P_unzip
  "ts_neg" -> Just P_ts_neg
  "ts_add" -> Just P_ts_add
  "ts_scale" -> Just P_ts_scale
  "ts_dot" -> Just P_ts_dot
  "eq" -> Just P_eq
  "ne" -> Just P_ne
  "delta" -> Just P_delta
  "deltaVec" -> Just P_deltaVec
  "diag" -> Just P_diag
  "constVec" -> Just P_constVec
  "lmApply" -> Just P_lmApply
  "lmApplyR" -> Just P_lmApplyR
  "lmApplyT" -> Just P_lmApplyT
  "lmVCat" -> Just P_lmVCat
  "lmHCat" -> Just P_lmHCat
  "lmVCatV"-> Just P_lmVCatV
  "lmHCatV" -> Just P_lmHCatV
  "lmCompose" -> Just P_lmCompose
  "lmAdd" -> Just P_lmAdd
  "lmScale" -> Just P_lmScale
  "lmScaleR" -> Just P_lmScaleR
  "lmDot" -> Just P_lmDot
  "lmZero" -> Just P_lmZero
  "lmOne" -> Just P_lmOne
  _ -> Nothing
