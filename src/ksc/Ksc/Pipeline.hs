-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Ksc.Pipeline where

import Ksc.Annotate (annotDecls, lintDefs)
import Ksc.AD (gradDef, applyDef)
import qualified Ksc.Cgen
import Ksc.CSE (cseDefs)
import Ksc.KMonad (KM, KMT, runKM,  banner)
import Ksc.CatLang
import Ksc.Traversal (mapAccumLM)
import Ksc.Lang (Decl, DeclX(DefDecl), DerivedFun(Fun), Derivations(JustFun),
             TDef, Pretty,
             def_fun, displayN, partitionDecls,
             ppr, renderSexp, (<+>))
import qualified Ksc.Lang as L
import Ksc.LangUtils (GblSymTab, emptyGblST, extendGblST, stInsertFun)
import qualified Ksc.Prune
import qualified Ksc.Futhark
import Ksc.Parse (parseF)
import Ksc.Rules (mkRuleBase)
import Ksc.Opt (optDefs)
import Ksc.Shapes (shapeDefs)
import Ksc.SUF (sufDef)
import Ksc.SUF.AD (sufFwdRevPassDef, sufRevDef)

import Data.Maybe (maybeToList)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

-------------------------------------
--  The demo driver
-------------------------------------

-- For example, load the cabal repl and try
--
--    demoF ["src/runtime/prelude.ks", "test/ksc/ex1.ks"]
demoF :: [String] -> IO ()
demoF files = do
  decls <- fmap concat (mapM parseF files)
  _ <- pipelineIO Nothing (Just 999) decls
  pure ()

-------------------------------------
-- Main compiler driver
-------------------------------------

displayCppGen :: Roots -> Maybe Int -> [String] -> [String] -> String -> String -> IO (String, String)
displayCppGen roots verbosity cppincludefiles ksFiles ksofile cppfile =
  do {
  ; decls0 <- fmap concat (mapM parseF ksFiles)
  ; putStrLn "ksc: read decls"
  ; defs <- pipelineIO roots verbosity decls0
  ; Ksc.Cgen.cppGenWithFiles ksofile cppfile cppincludefiles defs
  }

displayCppGenAndCompile
  :: HasCallStack
  => Roots
  -> (String -> String -> IO a)
  -> String
  -> Maybe Int
  -> [String]
  -> [String]
  -> String
  -> IO (a, (String, String))
displayCppGenAndCompile
  roots compile ext verbosity cppincludefiles files file = do {
  ; let ksFile = file ++ ".ks"
  ; let ksFiles = map (++ ".ks") files
  ; let compiler = compile
  ; let outfile = "obj/" ++ file
  ; let exefile = "obj/" ++ file ++ ext
  ; let ksofile = outfile ++ ".kso"
  ; let cppfile = outfile ++ ".cpp"
  ; outputFileContents <- displayCppGen roots verbosity cppincludefiles (ksFiles ++ [ksFile]) ksofile cppfile
  ; compilerResult <- compiler cppfile exefile
  ; pure (compilerResult, outputFileContents)
  }

type Roots = Maybe [L.UserFun L.Typed]

displayCppGenCompileAndRun :: HasCallStack
                           => Roots
                           -> String
                           -> Maybe Int
                           -> [String]
                           -> [String]
                           -> String
                           -> IO (String, (String, String))
displayCppGenCompileAndRun roots compilername verbosity cppincludefiles files file = do
  { (exefile, cpp_kso) <- displayCppGenAndCompile
                          roots (Ksc.Cgen.compile compilername) ".exe" verbosity cppincludefiles files file
  ; output <- Ksc.Cgen.runExe exefile
  ; pure (output, cpp_kso)
  }

displayCppGenCompileAndRunWithOutput :: HasCallStack => Roots -> String -> Maybe Int -> [String] -> [String] -> String -> IO ()
displayCppGenCompileAndRunWithOutput roots compilername verbosity cppincludefiles files file = do
  { (output, _) <- displayCppGenCompileAndRun roots compilername verbosity cppincludefiles files file
  ; putStrLn "Done"
  ; putStr output
  }

doall :: HasCallStack => Maybe Int -> [String] -> [String] -> String -> IO ()
doall = displayCppGenCompileAndRunWithOutput Nothing "g++-7"

-------------------------------------
-- Main compiler pipeline
-------------------------------------

pipelineIO :: Roots -> Maybe Int -> [Decl] -> IO [TDef]
pipelineIO mroots v d = runKM (pipeline mroots v d)

pipeline :: Roots -> Maybe Int -> [Decl] -> KM [TDef]
pipeline mroots verbosity decls = do
  { (env, concat->decls) <- mapAccumLM annotAndDeriveDecl emptyGblST decls
  ; displayPassMNoLint verbosity "generated decls" decls
  ; let (rules, defs) = partitionDecls decls
  ; displayPassM verbosity "generated defs" env defs
  ; let rulebase = mkRuleBase rules
  ; (env, defs) <- optDefs rulebase env defs
  ; displayPassM verbosity "opt decls" env defs
  ; (_env, defs) <- cseDefs rulebase env defs
  ; displayPassM verbosity "CSE decls" env defs
  ; defs <- pure $ case mroots of
      Nothing -> defs
      Just roots -> Ksc.Prune.prune defs (Set.fromList roots)
  ; pure defs
  }

annotAndDeriveDecl :: GblSymTab -> L.Decl -> KM (GblSymTab, [L.TDecl])
annotAndDeriveDecl env decl = do
  { (env, tdecls) <- annotDecls env [decl]
  ; (env, concat->tdecls) <- mapAccumLM deriveDecl env tdecls
  ; pure (env, tdecls)
  }

{- Note [GDef]
~~~~~~~~~~~~~~

ksc has various transformations each of which transform a ksc function
definition for [f T] into one or more derived functions.
Differentiated functions and shape functions are examples of the
derived functions that can be generated (e.g. e.g. [fwd [f T]], [shape
[f T]], ... -- see the implementation of deriveDecl for a
comprehensive list). The surface language feature that supports this
functionality is "gdef" ("generated definition").

The key function which expands a gdef into the function(s) it stands
for is deriveDecl.  Whenever the AST contains a GDefDecl, deriveDecl
looks up the implementation of the original function, transforms it,
and inserts one or more new functions into the AST based on the
transformation.  For example

* (gdef rev [f Float]) generates and inserts [rev [f Float]] and [D [f
  Float]] into the AST

* (gdef shape [f Float]) generates and inserts [shape [f Float]] into
  the AST

We do not want to require that the type of a derived function can be
deduced from the type of its input function.  Therefore we must
generate a derived function before we typecheck code that uses it (at
least under the current typechecker implementation) .  Consequently
the pipeline generates and typechecks each function definition
separately (annotAndDeriveDecl) before moving on to the next function
definition.

This restriction precludes mutual recursion between generated
functions.  This is not currently a practical restriction because Cgen
does not yet support mutual recursion either.  If we want to support
mutual recursion in the future then we can do so at the expense of a
more complicated typechecking story.

-}

deriveDecl :: GblSymTab -> L.TDecl -> KM (GblSymTab, [L.TDecl])
deriveDecl = deriveDeclUsing $ \env (L.GDef derivation fun) -> do
    { let tdef = case Map.lookup fun env of
              Nothing -> error $ unwords
                [ "Internal bug. Error when attempting to gdef."
                ,  "TODO: This ought to have been caught by type checking." ]
              Just tdef' -> tdef'

    ; case derivation of
        L.DerivationDrvFun dir -> do
          { let mgradDef = gradDef tdef
          ; let env' = maybe env (flip stInsertFun env) mgradDef
          ; graddedDef <- case mgradDef of
              Nothing -> do
                { L.printK (L.vcat [ L.text "Couldn't grad"
                                   , L.text "In gdef for" <+> ppr fun ])
                ; error "Exiting" }
              Just graddedDef' -> pure graddedDef'
          ; let appliedDef = applyDef dir graddedDef
          ; pure (stInsertFun appliedDef env', [DefDecl appliedDef])
          }
        L.DerivationCLFun
          | Fun JustFun basefun <- fun
          , let tdef' = case toCLDef_maybe tdef of
                  Nothing -> error ("Couldn't derive CL of "
                                    ++ L.render (ppr basefun))
                  Just d  -> (fromCLDef d){ def_fun = Fun L.CLFun basefun }
          -> pure (stInsertFun tdef' env, [DefDecl tdef'])
          | otherwise
          -> error ("Was not base fun: " ++ L.render (ppr fun))

        L.DerivationShapeFun ->
          let shapeDef = shapeDefs [tdef]
              env' = extendGblST env shapeDef
          in pure (env', map DefDecl shapeDef)

        L.DerivationSUFFwdPass ->
          let (sufFwdpassedDef, _, _) = sufFwdRevPassDef env (sufDef tdef)
          in pure (maybe env (flip stInsertFun env) sufFwdpassedDef,
                   DefDecl <$> maybeToList sufFwdpassedDef)
        L.DerivationSUFRevPass ->
          let (_, sufRevpassedDef, _) = sufFwdRevPassDef env (sufDef tdef)
          in pure (maybe env (flip stInsertFun env) sufRevpassedDef,
                   DefDecl <$> maybeToList sufRevpassedDef)
        L.DerivationSUFRev     ->
          let sufRevedDef = sufRevDef env (sufDef tdef)
          in pure (maybe env (flip stInsertFun env) sufRevedDef,
                   DefDecl <$> maybeToList sufRevedDef)
    }

deriveDeclUsing :: Applicative f
                => (env -> L.GDefX -> f (env, [L.TDecl]))
                -> env
                -> L.TDecl
                -> f (env, [L.TDecl])
deriveDeclUsing f env = \case
  -- Pass the rules on through
  r@L.RuleDecl{} -> pure (env, [r])
  -- Existing defs just get passed straight through
  d@DefDecl{} -> pure (env, [d])
  -- gdefs get looked up
  L.GDefDecl g -> f env g

-------------------------------------
-- Displaying passes
-------------------------------------

type DisplayLintT m = String -> GblSymTab -> [TDef] -> KMT m ()
type DisplayLint = DisplayLintT IO

displayPassMNoLint :: Pretty def => Maybe Int -> String -> [def] -> KMT IO ()
displayPassMNoLint mverbosity what decls
  = do flip mapM_ mverbosity $ \verbosity -> do
         banner what
         displayN (take verbosity decls)

displayPassM :: Maybe Int -> DisplayLint
displayPassM mverbosity what env decls
  = do { displayPassMNoLint mverbosity what decls
       ; lintDefs what env decls
    }

-------------------------------------
-- The Futhark driver
-------------------------------------
futharkPipeline :: [FilePath] -> IO [TDef]
futharkPipeline files
  = do
  { decls0 <- fmap concat (mapM (parseF . (++ ".ks")) files)
  ; pipelineIO Nothing Nothing decls0
  }

-- | Read source code from specified input file, optimise,
-- differentiate, optimise, and write result to corresponding @.fut@
-- file.  You will have to run the Futhark compiler yourself
-- afterwards (and probably write a small wrapper program, because
-- currently the generated file will not have any entry points).
--
-- Usage:
--
-- @
-- $ ghci -isrc/ksc -e 'genFuthark "test/ksc/gmm"' src/ksc/Main.hs
-- @
genFuthark :: [FilePath] -> FilePath -> IO ()
genFuthark files file = do
  prelude <- readFile "src/runtime/knossos.fut"
  defs <- futharkPipeline (files ++ [file])
  putStrLn $ "ksc: Writing to " ++ futfile
  Ksc.Cgen.createDirectoryWriteFile futfile $
    intercalate "\n\n" $
    prelude : map (renderSexp . ppr . Ksc.Futhark.toFuthark) defs
  where futfile = "obj/" ++ file ++ ".fut"
