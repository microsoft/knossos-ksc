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

data Type = TypeZero               -- Polyamorous zero
          | TypeBool
          | TypeInteger
          | TypeFloat
          | TypeTuple [Type]
          | TypeVec Type
          | TypeLambda Type Type   -- Domain -> Range
          | TypeLM Type Type       -- Linear map  Src -o Target
          | TypeUnknown
          deriving (Show, Eq, Ord)

data FunId = SelFun     -- Selector function: fst, snd etc
               Int      -- Index; 1-indexed, so (SelFun 1 2) is fst
               Int      -- Arity
           | SFun String  -- For now
           deriving( Eq, Ord )

data Fun = Fun     FunId         -- The function              f(x)
         | GradFun FunId ADMode  -- Full Jacobian Df(x)
                                 --   Rev <=> transposed  Rf(x)
         | DrvFun  FunId ADMode  -- Derivative derivative f'(x,dx)
                                 --   Rev <=> reverse mode f`(x,dr)
         | LMFun   String        -- Linear map
         deriving( Eq, Ord )

data ADMode = Fwd | Rev
            deriving( Eq, Ord, Show )

data TFun = TFun Type Fun  -- Typed functions
  deriving (Eq, Ord)

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

data Konst = KZero  -- Of any type
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

isKZero :: ExprX f b -> Bool
isKZero (Konst KZero) = True
isKZero _             = False

partitionDecls :: [DeclX f b] -> ([RuleX f b], [DefX f b])
partitionDecls decls
  = partitionEithers $
    map (\case { RuleDecl r -> Left r; DefDecl d -> Right d }) $
    decls

-----------------------------------------------
--  Finding the type of an expression
-----------------------------------------------

class CmkVar b where
  mkVar :: Type -> String -> b

instance CmkVar Var where
  mkVar ty s = Simple s

instance CmkVar TVar where
  mkVar ty s = TVar ty $ Simple s

class TypeableFun f where
  typeofFun :: f -> Type -> Type

instance TypeableFun Fun where
  typeofFun v arg = TypeUnknown

instance TypeableFun TFun where
  typeofFun (TFun ty f) argtype = ty

class Typeable b where
  typeof :: b -> Type

instance Typeable Var where
  typeof v = TypeUnknown

instance Typeable TVar where
  typeof (TVar ty _) = ty

instance (Typeable b, TypeableFun f) =>
         Typeable (ExprX f b) where
  typeof (Konst k) = typeofKonst k
  typeof (Var b) = typeof b
  typeof (Call f e) = typeofFun f (typeof e)
  typeof (App f e) = TypeUnknown
  typeof (Tuple es) = TypeTuple $ map typeof es
  typeof (Lam (TVar tyv v) e) = TypeLambda tyv $ typeof e
  typeof (Let b e1 e2) = typeof e2
  typeof (If c t f) = makeIfType (typeof t) (typeof f)
  typeof (Assert c e) = typeof e

-- ToDo:
-- The type of an If statement is a sort of union of the types on the branches
-- This occurs because of types like TypeZero which intersect both Float and Integer
-- We could make our constants more strongly typed, which might be better, but
-- the Zero vector can be a useful optimization concept.
makeIfType :: Type -> Type -> Type
makeIfType TypeZero ty = ty
makeIfType ty TypeZero = ty
makeIfType ty1 ty2 = assertEqualThen "makeIfType" ty1 ty2 $ ty2

getLM :: HasCallStack => Type -> Type
getLM (TypeLM s t) = TypeLM s t
getLM t = error $ "Wanted TypeLM, got " ++ pps t

typeofLMs:: HasCallStack => Type -> Type
typeofLMs (TypeLM s _) = s
typeofLMs t = error $ "Wanted TypeLM, got " ++ pps t
typeofLMt:: HasCallStack =>Type -> Type
typeofLMt (TypeLM _ t) = t

typeofKonst :: Konst -> Type
typeofKonst KZero = TypeZero
typeofKonst (KInteger _) = TypeInteger
typeofKonst (KFloat _) = TypeFloat
typeofKonst (KBool _) = TypeBool

-----------------------------------------------
--       Show instances
-----------------------------------------------

instance Show FunId where
  show (SFun s) = s
  show (SelFun i n) = "selfun<" ++ show i ++ "," ++ show n ++ ">"

instance Show Fun where
  show (Fun s) = show s
  show (GradFun s Fwd) = "D$" ++ show s
  show (GradFun s Rev) = "R$" ++ show s
  show (DrvFun  s Fwd) = "fwd$" ++ show s
  show (DrvFun  s Rev) = "rev$" ++ show s
  show (LMFun s) = "LM$" ++ s

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
--       Building values
-----------------------------------------------

mkFun :: String -> Fun
mkFun fname = Fun (SFun fname)

mkTFun :: Type -> String -> TFun
mkTFun ty fname = TFun ty $ mkFun fname

mkCall :: String->[Expr]->Expr
mkCall fname [e] = Call (mkFun fname) e
mkCall fname es = Call (mkFun fname) (mkTuple es)

mkTCall :: Type -> Fun -> [TExpr] -> TExpr
mkTCall ty f [e] = Call (TFun ty f) e
mkTCall ty f es = Call (TFun ty f) (mkTuple es)

mkTuple :: [ExprX f b] -> ExprX f b
mkTuple [e] = e
mkTuple es = Tuple es

mkSCall1 :: String -> Expr -> Expr
mkSCall1 fname a = mkCall fname [a]

mkSCall2 :: String -> Expr -> Expr -> Expr
mkSCall2 fname a b = mkCall fname [a, b]

mkSCall3 :: String -> Expr -> Expr -> Expr -> Expr
mkSCall3 fname a b c = mkCall fname [a, b, c]

mkTCall1 :: Type -> Fun -> TExpr -> TExpr
mkTCall1 ty f a = mkTCall ty f [a]

mkTCall2 :: Type -> Fun -> TExpr -> TExpr -> TExpr
mkTCall2 ty f a b = mkTCall ty f [a, b]

mkTCall3 :: Type -> Fun -> TExpr -> TExpr -> TExpr -> TExpr
mkTCall3 ty f a b c = mkTCall ty f [a, b, c]

mkLet :: HasCallStack => TVar -> TExpr -> TExpr -> TExpr
mkLet (TVar ty v) rhs body
  = assertEqualThen ("mkLet " ++ show v ++ " = " ++ pps rhs)
                    ty (typeof rhs) $
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

{-
infixr 0 `seqExpr`

seqExprX :: ExprX -> a -> a
seqExprX (Var v) x = v `seq` x
seqExprX (Call fun e) x = fun `seq` e `seqExprX` x
seqExprX (Konst k) x = k `seq` x
seqExprX (Let v r b) x = v `seq` r `seqExprX` b `seqExprX` x
seqExprX (Tuple es) x = Prelude.foldr seqExprX x es

seqExpr:: Expr -> a -> a
seqExpr (Expr ty e) = ty `seq` e
-}


-----------------------------------------------
--     Substitution
-----------------------------------------------

substE :: M.Map TVar TExpr -> TExpr -> TExpr
substE subst (Konst k)      = Konst k
substE subst (Var v)        = case M.lookup v subst of
                               Just e  -> e
                               Nothing -> Var v
substE subst (Call f e)     = Call f (substE subst e)
substE subst (If b t e)     = If (substE subst b) (substE subst t) (substE subst e)
substE subst (Tuple es)     = Tuple (map (substE subst) es)
substE subst (App e1 e2)    = App (substE subst e1) (substE subst e2)
substE subst (Assert e1 e2) = Assert (substE subst e1) (substE subst e2)
substE subst (Lam v e)      = Lam v (substE (v `M.delete` subst) e)
substE subst (Let v r b)    = Let v (substE subst r) $
                              substE (v `M.delete` subst) b

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
cmpExpr e1 e2
 = go e1 M.empty e2
 where
   go :: TExpr -> M.Map TVar TVar -> TExpr -> Ordering
   go (Konst k1) subst e2
     = case e2 of
         Konst k2 -> k1 `compare` k2
         _        -> LT

   go (Var v1) subst e2
     = case e2 of
         Konst {} -> GT
         Var v2   -> v1 `compare` M.findWithDefault v2 v2 subst
         _        -> LT

   go (Call f1 e1) subst e2
     = case e2 of
         Konst {} -> GT
         Var {} -> GT
         Call f2 e2 -> (f1 `compare` f2) `thenCmp` (go e1 subst e2)
         _ -> LT

   go (Tuple es1) subst e2
     = case e2 of
         Konst {} -> GT
         Var {}  -> GT
         Call {} -> GT
         Tuple es2 -> gos es1 subst es2
         _        -> LT

   go (Lam b1 e1) subst e2
      = case e2 of
         Konst {}  -> GT
         Var {}    -> GT
         Call {}   -> GT
         Tuple es  -> GT
         Lam b2 e2 -> go e1 (M.insert b2 b1 subst) e2
         _         -> LT

   go (App e1a e1b) subst e2
     = case e2 of
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
         Let b2 r2 e2 ->
                go r1 subst r2 `thenCmp` go e1 (M.insert b2 b1 subst) e2
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

   gos :: [TExpr] -> M.Map TVar TVar -> [TExpr] -> Ordering
   gos [] subst [] = EQ
   gos [] subst (_:_) = LT
   gos (_:_) subst [] = GT
   gos (e1:es1) subst (e2:es2) = go e1 subst e2 `thenCmp` gos es1 subst es2


-----------------------------------------------
--     Free variables
-----------------------------------------------

notFreeIn :: TVar -> TExpr -> Bool
notFreeIn v e = go v e
 where
   go:: TVar -> TExpr -> Bool
   go v (Var v2) = v /= v2
   go v (Konst _) = True
   go v (Tuple es) = all (go v) es
   go v (If b t e) = go v b && go v t && go v e
   go v (Call _ e) = go v e
   go v (App f a)  = go v f && go v a
   go v (Let v2 r b) = go v r && (v == v2 || go v b)
   go v (Lam v2 e)   = v == v2 || go v e
   go v (Assert e1 e2) = go v e1 && go v e2

-----------------

newVarNotIn :: Type -> TExpr -> TVar
newVarNotIn ty e = go ty e 1 -- FIXME start with hash of e to reduce retries
  where go ty e n =
          let v = mkVar ty ("_t" ++ show n) in
            if v `notFreeIn` e then
              v
            else
              trace ("newVarNotIn: Var " ++ show v ++ " was bound in E, retry") (
              go ty e (n + 1))

test_FreeIn =
  hspec $ do
    let var :: String -> TVar
        var s = TVar TypeFloat (Simple s)
        fun :: String -> TFun
        fun s = TFun TypeFloat (mkFun s)
        e  = Call (fun "f") (Var (var "i"))
        e2 = Call (fun "f") (Tuple [Var (var "_t1"), kInt 5])
    describe "notFreeIn" $ do
      it ("i notFreeIn " ++ show (ppr (e::TExpr))) $
        (var "i" `notFreeIn` e) `shouldBe` False
      it ("x not notFreeIn " ++ show (ppr (e::TExpr))) $
        (var "x" `notFreeIn` e) `shouldBe` True
    describe "newVarNotIn" $ do
      it "not in, so new var is _t1..." $
        newVarNotIn TypeFloat e `shouldBe` (var "_t1")
      it "in, so new var is _t2..." $
        newVarNotIn TypeFloat e2 `shouldBe` (var "_t2")

-----------------------------------------------
--     Pretty printer
-----------------------------------------------

class Pretty p where
  ppr     :: p -> Doc
  ppr = pprPrec precZero

  pprPrec :: Prec -> p -> Doc
  pprPrec _ = ppr

instance Pretty Var where
  ppr v   = PP.text $ show v

instance Pretty FunId where
  ppr (SFun s)     = PP.text s
  ppr (SelFun i n) = PP.text "sel_" PP.<> PP.int i PP.<> PP.char '_' PP.<> PP.int n

instance Pretty Fun where
  ppr (Fun s)           = ppr s
  ppr (GradFun s Fwd) = PP.char 'D' PP.<> ppr s
  ppr (GradFun s Rev) = PP.char 'R' PP.<> ppr s
  ppr (DrvFun s Fwd)  = ppr s PP.<> PP.char '\''
  ppr (DrvFun s Rev)  = ppr s PP.<> PP.char '`'
  ppr (LMFun s)   = PP.text s

instance Pretty TVar where
  pprPrec p (TVar ty Dummy) = ppr ty -- For dummy vars, print the type
  pprPrec p (TVar ty v) = ppr v
  
instance Pretty TFun where
  ppr (TFun ty f) = ppr f

instance Pretty Konst where
  ppr (KInteger i) = PP.integer i
  ppr (KFloat f)   = PP.double f
  ppr KZero        = text "KZero"

instance Pretty Type where
  ppr (TypeVec ty) = PP.text "(Vec " PP.<> ppr ty PP.<> PP.text ")"
  ppr (TypeTuple tys) = PP.text "(Tuple (" PP.<> pprWithCommas ppr tys PP.<> PP.text "))"
  ppr (TypeLambda from to) = PP.text "(Lambda " PP.<> ppr from PP.<> PP.text " -> " PP.<> ppr to PP.<> PP.text ")"
  ppr (TypeLM s t) = PP.text "(LM " PP.<> ppr s PP.<> PP.char ' ' PP.<> ppr t PP.<> PP.text ")"
  ppr TypeZero = PP.text "zero_t"
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
  isInfix (Fun (SFun s))
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
  = trace (PP.render (PP.sep [PP.text str, PP.nest 2 doc])) v

pprPanic :: String -> Doc -> a
pprPanic str doc
  = error (PP.render (PP.sep [PP.text str, PP.nest 2 doc]))

pps :: Pretty a => a -> String
pps a = show $ ppr a

test_Pretty =
  hspec $ do
    let test e s = it s $ pps e `shouldBe` s

    let var s = Var (Simple s)
    let e = mkSCall1 "g" (var "i")
    let e2 = mkSCall3 "f" e (var "_t1") (kInt 5)

    describe "Pretty" $ do
      test e "g( s$i )"
      test e2 "f( g( s$i ), s$_t1, 5 )"

-----------------------------------------------
--     Symbol table, ST, maps variables to types
-----------------------------------------------

type ST = Map.Map Var Type

sttrace :: String -> a -> a
sttrace _ e = e -- trace msg e

emptyST :: ST
emptyST = Map.empty

stBindParams :: ST -> [TVar] -> ST
stBindParams st params
  = foldl add st params
  where
    add :: ST -> TVar -> ST
    add env (TVar ty v) = stInsert v ty env

stInsert :: Var -> Type -> ST -> ST
stInsert v ty env = sttrace
  ("Inserting " ++ show v ++ " = " ++ show ty ++ " in " ++ show env ++ "\n")
  (Map.insert v ty env)

stInsertFun :: Fun -> Type -> ST -> ST
stInsertFun f = stInsert (Simple $ show f)
  -- Simon says: bizarre

stLookup :: HasCallStack => String -> Var -> ST -> Type
stLookup msg v env = case Map.lookup v env of
  Just a  -> a
  Nothing -> error
    ("Couldn't find " ++ show v ++ " in " ++ msg ++ ", env = " ++ show env)
    TypeUnknown

stLookupFun :: HasCallStack => String -> Fun -> ST -> Type
stLookupFun msg f = stLookup msg (Simple $ show f)

------------------------------------------------------------------------------

mkTypeTuple :: [Type] -> Type
mkTypeTuple [ty] = ty
mkTypeTuple tys = TypeTuple tys

-- A single place for "domain knowledge" about polymorphic functions -- to be pruned when we get primdefs
typeofFunTy :: HasCallStack => ST -> Fun -> Type -> Type
typeofFunTy env f (TypeTuple tys) = typeofFunTys env f tys
typeofFunTy env f ty              = typeofFunTys env f [ty]

typeofFunTys :: HasCallStack => ST -> Fun -> [Type] -> Type
typeofFunTys env tf tys =
  case (tf, tys) of
  (GradFun f Fwd, tys) -> TypeLM (mkTypeTuple tys) (typeofFunTys env (Fun f) tys)
  (GradFun f Rev, tys) -> TypeLM (typeofFunTys env (Fun f) tys) (mkTypeTuple tys)
  (LMFun "lmApply",  [TypeLM s t, s1]) -> assertEqualThen "lmApply" s1 s $ t
  (LMFun f, tys) -> error $ "When?"
  (Fun (SFun "pr")       , _                            ) -> TypeInteger
  (Fun (SFun "build")    , [_, TypeLambda TypeInteger t]) -> TypeVec t
  (Fun (SFun "index")    , [_, TypeVec t]               ) -> t
  (Fun (SFun "size" )    , [TypeVec _]                  ) -> TypeInteger
  (Fun (SFun "sum"  )    , [TypeVec t]                  ) -> t
  (Fun (SFun "to_float") , [TypeInteger]                ) -> TypeFloat
  (Fun (SFun "neg"  )    , [t]                          ) -> t
  (Fun (SFun "exp"  )    , [TypeFloat]                  ) -> TypeFloat
  (Fun (SFun "log"  )    , [TypeFloat]                  ) -> TypeFloat
  (Fun (SFun "*"    )    , [t1, TypeFloat]              ) -> t1
  (Fun (SFun "+"    )    , [t1, t2]                     ) -> t1
  (Fun (SFun "/"    )    , [t1, t2]                     ) -> t1
  (Fun (SFun "*"    )    , [t1, t2]                     ) -> t1
  (Fun (SFun "-"    )    , [t1, t2]                     ) -> t1
  (Fun (SFun "square")   , [t1]                         ) -> t1

  (Fun (SFun "=="   )    , _                            ) -> TypeBool
  (Fun (SFun "!="   )    , _                            ) -> TypeBool
  (Fun (SFun "<"    )    , _                            ) -> TypeBool
  (Fun (SFun ">"    )    , _                            ) -> TypeBool

  (Fun (SFun "delta")    , [TypeInteger, TypeInteger, t]) -> t

  (Fun (SelFun i _  )    , [TypeTuple tys]              ) -> tys !! (i - 1)
  (Fun (SelFun{})      , [TypeVec t]) -> t
  (f        , _          ) -> case Map.lookup (Simple $ show f) env of
                                  Just a  -> a
                                  Nothing -> error $ "LOOKUP: " ++ emsg

  where emsg = "Failed to find type for Function\n"
                ++ show tf
                ++ " @ "
                ++ show tys
                ++ ".    Env:\n"
                ++ show env

-----------------------------------------------
--     Debugging utilities
-----------------------------------------------

assertEqual msg t1 t2 =
  assertEqualThen msg t1 t2 ()

assertEqualThen :: HasCallStack => (Eq a, Show a) => String -> a -> a -> b -> b
assertEqualThen msg t1 t2 e =
  if t1 == t2 then e else error ("Asserts unequal ["++msg++"] \n T1 = " ++ show t1 ++ "\n T2 = " ++ show t2 ++ "\n") $ e

assertAllEqualThen :: HasCallStack => Eq a => Show a => String -> [a] -> b -> b
assertAllEqualThen msg es e =
  if allEq es then e else
     flip trace e $ ("Assert failed: ["++msg++"] not all equal  \n " ++ show es ++ "\n")
  where
    allEq [] = True
    allEq (a:as) = allEqa a as

    allEqa a0 [] = True
    allEqa a0 [a] = a0 == a
    allEqa a0 (a:as) = a0 == a && allEqa a0 as

assertAllEqualRet :: HasCallStack => Eq a => Show a => String -> [a] -> a
assertAllEqualRet msg (e:es) = assertAllEqualThen msg (e:es) e
