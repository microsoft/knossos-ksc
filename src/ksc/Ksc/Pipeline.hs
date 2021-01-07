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
import KMonad (KM, KMT, runKM,  banner, liftIO)
import Ksc.CatLang
import Ksc.Traversal (mapAccumLM)
import Lang (Decl, DeclX(DefDecl), DefX(Def), DerivedFun(Fun),
             TDef, Pretty, BaseUserFun(BaseUserFunId),
             def_fun, displayN, partitionDecls,
             ppr, renderSexp, (<+>))
import qualified Lang as L
import LangUtils (GblSymTab, emptyGblST, extendGblST, stInsertFun)
import qualified Ksc.Futhark
import Parse (parseF)
import Rules (mkRuleBase)
import Opt (optDefs)
import Shapes (shapeDefs)

import Data.List (partition, intercalate)
import qualified Data.Map as Map
import GHC.Stack (HasCallStack)

-------------------------------------
--  The demo driver
-------------------------------------

-- For example, load the cabal repl and try
--
--    demoF ["src/runtime/prelude.ks", "test/ksc/ex1.ks"]
demoF :: [String] -> IO ()
demoF files = runKM $ do
  decls <- liftIO (fmap concat (mapM parseF files))
  _ <- pipeline (Just 999) decls
  pure ()

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
-- Main compiler driver
-------------------------------------

ignoreMain :: [Decl] -> [Decl]
ignoreMain = snd . moveMain

moveMain :: [Decl]
         -> ( [Decl]    -- Singleton 'main' decl, or empty
            , [Decl])   -- All the rest
moveMain = partition isMain
  where
    isMain (DefDecl (Def { def_fun = Fun (BaseUserFunId "main" _) })) = True
    isMain _ = False

displayCppGen :: Maybe Int -> [String] -> String -> String -> IO (String, String)
displayCppGen verbosity ksFiles ksofile cppfile =
  runKM $
  do {
  ; decls0 <- liftIO (fmap concat (mapM parseF ksFiles))
  ; liftIO $ putStrLn "read decls"
  ; defs <- pipeline verbosity decls0
  ; liftIO (Cgen.cppGenWithFiles ksofile cppfile defs)
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
-- The Futhark driver
-------------------------------------
futharkPipeline :: [FilePath] -> KM [TDef]
futharkPipeline files
  = do
  { decls0 <- liftIO (fmap concat (mapM (parseF . (++ ".ks")) files))
  ; pipeline Nothing decls0
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
  defs <- runKM $ futharkPipeline (files ++ [file])
  putStrLn $ "Writing to " ++ futfile
  Cgen.createDirectoryWriteFile futfile $
    intercalate "\n\n" $
    prelude : map (renderSexp . ppr . Ksc.Futhark.toFuthark) defs
  where futfile = "obj/" ++ file ++ ".fut"

-------------------------------------
-- Main compiler pipeline
-------------------------------------

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

deriveDecl :: GblSymTab -> L.TDecl -> KM (GblSymTab, [L.TDecl])
deriveDecl = deriveDeclUsing $ \env (L.GDef derivation fun) -> do
    { let tdef = case Map.lookup fun env of
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
          | Fun basefun <- fun
          , let tdef' = case toCLDef_maybe tdef of
                  Nothing -> error ("Couldn't derive CL of "
                                    ++ L.render (ppr basefun))
                  Just d  -> (fromCLDef d){ def_fun = L.CLFun basefun }
          -> pure (stInsertFun tdef' env, [DefDecl tdef'])
          | otherwise
          -> error ("Was not base fun: " ++ L.render (ppr fun))

        L.DerivationShapeFun ->
          let shapeDef = shapeDefs [tdef]
              env' = extendGblST env shapeDef
          in pure (env', map DefDecl shapeDef)
    }

annotAndDeriveDecl :: GblSymTab -> L.Decl -> KM (GblSymTab, [L.TDecl])
annotAndDeriveDecl env decl = do
  { (env, tdecls) <- annotDecls env [decl]
  ; (env, concat->tdecls) <- mapAccumLM deriveDecl env tdecls
  ; pure (env, tdecls)
  }
