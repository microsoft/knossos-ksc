-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Ksc.Pipeline where

import Annotate (annotDecls, lintDefs)
import AD (gradDef, applyDef)
import qualified Cgen
import CSE (cseDefs)
import KMonad (KM, KMT, runKM,  banner)
import Ksc.CatLang
import Ksc.Traversal (mapAccumLM)
import Lang (Decl, DeclX(DefDecl), DerivedFun(Fun), Derivations(JustFun),
             TDef, Pretty,
             def_fun, displayN, partitionDecls,
             ppr, renderSexp, (<+>))
import qualified Lang as L
import LangUtils (GblSymTab, emptyGblST, extendGblST, stInsertFun, lookupDef)
import qualified Ksc.Futhark
import Parse (parseF)
import Rules (mkRuleBase)
import Opt (optDefs)
import Shapes (shapeDefs)
import Ksc.SUF (sufDef)
import Ksc.SUF.AD (sufFwdRevPassDef, sufRevDef)

import Data.Maybe (maybeToList)
import Data.List (intercalate)
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
  _ <- pipelineIO (Just 999) decls
  pure ()

-------------------------------------
-- Main compiler driver
-------------------------------------

displayCppGen :: Maybe Int -> [String] -> String -> String -> IO (String, String)
displayCppGen verbosity ksFiles ksofile cppfile =
  do {
  ; decls0 <- fmap concat (mapM parseF ksFiles)
  ; putStrLn "ksc: read decls"
  ; defs <- pipelineIO verbosity decls0
  ; Cgen.cppGenWithFiles ksofile cppfile defs
  }

displayCppGenAndCompile
  :: HasCallStack
  => (String -> String -> IO a)
  -> String
  -> Maybe Int
  -> [String]
  -> String
  -> IO (a, (String, String))
displayCppGenAndCompile
  compile ext verbosity files file = do {
  ; let ksFile = file ++ ".ks"
  ; let ksFiles = map (++ ".ks") files
  ; let compiler = compile
  ; let outfile = "obj/" ++ file
  ; let exefile = "obj/" ++ file ++ ext
  ; let ksofile = outfile ++ ".kso"
  ; let cppfile = outfile ++ ".cpp"
  ; outputFileContents <- displayCppGen verbosity (ksFiles ++ [ksFile]) ksofile cppfile
  ; compilerResult <- compiler cppfile exefile
  ; pure (compilerResult, outputFileContents)
  }

displayCppGenCompileAndRun :: HasCallStack
                           => String
                           -> Maybe Int
                           -> [String]
                           -> String
                           -> IO (String, (String, String))
displayCppGenCompileAndRun compilername verbosity file files = do
  { (exefile, cpp_kso) <- displayCppGenAndCompile
                          (Cgen.compile compilername) ".exe" verbosity file files
  ; output <- Cgen.runExe exefile
  ; pure (output, cpp_kso)
  }

displayCppGenCompileAndRunWithOutput :: HasCallStack => String -> Maybe Int -> [String] -> String -> IO ()
displayCppGenCompileAndRunWithOutput compilername verbosity files file = do
  { (output, _) <- displayCppGenCompileAndRun compilername verbosity files file
  ; putStrLn "Done"
  ; putStr output
  }

doall :: HasCallStack => Maybe Int -> [String] -> String -> IO ()
doall = displayCppGenCompileAndRunWithOutput "g++-7"

-------------------------------------
-- Main compiler pipeline
-------------------------------------

pipelineIO :: Maybe Int -> [Decl] -> IO [TDef]
pipelineIO v d = runKM (pipeline v d)

pipeline :: Maybe Int -> [Decl] -> KM [TDef]
pipeline verbosity decls = do
  { (env, concat->decls) <- mapAccumLM annotAndDeriveDecl emptyGblST decls
  ; displayPassMNoLint verbosity "generated decls" decls
  ; let (rules, defs) = partitionDecls decls
  ; displayPassM verbosity "generated defs" env defs
  ; let rulebase = mkRuleBase rules
  ; (env, defs) <- optDefs rulebase env defs
  ; displayPassM verbosity "opt decls" env defs
  ; (_env, defs) <- cseDefs rulebase env defs
  ; displayPassM verbosity "CSE decls" env defs
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
    { -- fun :: UserFun Typed
      let tdef = case lookupDef fun env of
              Nothing -> error $ unwords
                [ "Internal bug. Error when attempting to gdef."
                ,  "TODO: This ought to have been caught by type checking." ]
              Just tdef' -> tdef'

    ; case derivation of
        L.DerivationDrvFun (L.AD plan dir) -> do
          { let mgradDef = gradDef plan tdef
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
  ; pipelineIO Nothing decls0
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
  Cgen.createDirectoryWriteFile futfile $
    intercalate "\n\n" $
    prelude : map (renderSexp . ppr . Ksc.Futhark.toFuthark) defs
  where futfile = "obj/" ++ file ++ ".fut"
