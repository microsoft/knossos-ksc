-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
module Ksc.Pipeline where

import Annotate (annotDecls, lintDefs)
import AD (gradDefs, applyDefs)
import ANF (anfDefs)
import qualified Cgen
import CSE (cseDefs)
import KMonad (KM, KMT, runKM,  banner, liftIO)
import Lang (ADDir(Rev, Fwd), ADPlan(BasicAD, TupleAD),
             Decl, DeclX(DefDecl), DefX(Def), Fun(Fun),
             FunId(UserFun), TDef,
             def_fun, displayN, partitionDecls,
             pps, ppr, renderSexp)
import LangUtils (GblSymTab, emptyGblST, extendGblST)
import qualified Ksc.Futhark
import Parse (parseF)
import Rules (RuleBase, mkRuleBase)
import Opt (optDefs)

import Data.List (partition, intercalate)
import GHC.Stack (HasCallStack)

-------------------------------------
--  The demo driver
-------------------------------------

demoF :: ADPlan -> String -> IO ()
-- Read source code from specified input file, optimise,
-- differentiate, optimise, and display results of each step
demoF = demoFFilter (Just 999) id

demoFFilter :: Maybe Int -> ([Decl] -> [Decl]) -> ADPlan -> String -> IO ()
demoFFilter verbosity theFilter adp file = do
  defs <- parseF file
  runKM (demoN verbosity adp (theFilter defs))

demo :: Decl -> IO ()
demo d = runKM (demoN (Just 999) BasicAD [d])

demoN :: Maybe Int -> ADPlan -> [Decl] -> KM ()
demoN verbosity adp decls
  = let disp = displayPassM verbosity in
    do { flip mapM_ verbosity $ \v -> do
           banner "Original declarations"
           displayN (take v decls)

       ; (env, tc_decls) <- annotDecls emptyGblST decls
       ; let (rules, defs) = partitionDecls tc_decls
             rulebase     = mkRuleBase rules

       ; disp "Typechecked declarations" env defs

       ; (env1, opt_defs) <- optDefs rulebase env defs
       ; disp "Optimized original definition" env1 opt_defs

       ; anf_defs <- anfDefs opt_defs
       ; disp "Anf-ised original definition" env1 anf_defs

       ; let grad_defs = gradDefs adp anf_defs
             env2      = env1 `extendGblST` grad_defs
       ; disp "The full Jacobian (unoptimised)" env2 grad_defs

       ; (env3, opt_grad_defs) <- optDefs rulebase env2 grad_defs
       ; disp "The full Jacobian (optimised)" env3 opt_grad_defs

       ; let der_fwd = applyDefs Fwd opt_grad_defs
       ; disp "Forward derivative (unoptimised)" env3 der_fwd

       ; (env4, opt_der_fwd) <- optDefs rulebase env3 der_fwd
       ; disp "Forward-mode derivative (optimised)" env4 opt_der_fwd

       ; (env5, cse_fwd) <- cseDefs rulebase env4 opt_der_fwd
       ; disp "Forward-mode derivative (CSE'd)" env5 cse_fwd

       ; let der_rev = applyDefs Rev opt_grad_defs
       ; disp "Reverse-mode derivative (unoptimised)" env3 der_rev

       ; (env7, opt_der_rev) <- optDefs rulebase env3 der_rev
       ; disp "Reverse-mode derivative (optimised)" env7 opt_der_rev

       ; (env8, cse_rev) <- cseDefs rulebase env7 opt_der_rev
       ; disp "Reverse-mode derivative (CSE'd)" env8 cse_rev
       }

type DisplayLintT m a = String -> GblSymTab -> [TDef] -> KMT m a
type DisplayLint a = DisplayLintT IO a

displayPassM :: Maybe Int -> DisplayLint ()
displayPassM mverbosity what env decls
  = do { flip mapM_ mverbosity $ \verbosity -> do
           banner what
           displayN (take verbosity decls)
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
    isMain (DefDecl (Def { def_fun = Fun (UserFun "main") })) = True
    isMain _ = False

theDefs :: DisplayLint a
        -> [Decl] -> KMT IO ([TDef], GblSymTab, RuleBase)
theDefs display decls = do {
  ; (env, ann_decls) <- annotDecls emptyGblST decls
  ; let (rules, defs) = partitionDecls ann_decls
  ; let rulebase      = mkRuleBase rules
  ; display "Typechecked defs" env defs
  ; return (defs, env, rulebase)
  }

theDiffs :: DisplayLint a
         -> [TDef]
         -> GblSymTab
         -> RuleBase
         -> KMT IO (GblSymTab, [TDef], [TDef], RuleBase)
theDiffs display defs env rulebase = do {
  ; let grad_defs = gradDefs BasicAD defs
        env1 = env `extendGblST` grad_defs
  ; display "Grad" env1 grad_defs

  -- We generate grad_defs_tupled even though we do not use it yet.
  -- We do not use it because
  --
  -- 1. lmApplys are not completely removed in forward mode, and
  --
  -- 2. lmApplys are deliberately not removed in reverse mode but we
  -- don't yet implement the necessary LM constructors in C++.
  --
  -- Nonetheless, it's good to generate optgrad_tupled here so that
  -- the tests will run on it and we can be sure it typechecks.
  ; let grad_defs_tupled = gradDefs TupleAD defs
        env15 = env1 `extendGblST` grad_defs_tupled
  ; display "Grad tupled" env15 grad_defs_tupled

  ; (env2, optgrad) <- optDefs rulebase env15 grad_defs
  ; display "Optgrad" env2 optgrad

  ; (env25, optgrad_tupled) <- optDefs rulebase env2 grad_defs_tupled
  ; display "Optgrad tupled" env25 optgrad_tupled

  ; let diffs = applyDefs Fwd optgrad ++ applyDefs Rev optgrad
  ; display "Diffs" env25 diffs

  ; (env3, optdiffs) <- optDefs rulebase env25 diffs
  ; display "OptDiffs" env3 optdiffs

  -- Note optgrad removed from below as we can not currently
  -- codegen the optgrad for recursive functions
  -- [see https://github.com/awf/knossos/issues/281]
  ; return (env3, defs, optdiffs, rulebase)
  }

defsAndDiffs :: DisplayLint a
             -> [Decl]
             -> KM (GblSymTab, [TDef], [TDef], RuleBase)
defsAndDiffs display decls = do {
  ; (defs, env, rulebase) <- theDefs display decls
  ; theDiffs display defs env rulebase
  }
anfOptAndCse :: DisplayLint a
             -> RuleBase -> GblSymTab -> [TDef] -> KM [TDef]
anfOptAndCse display rulebase env4 alldefs =
  do {
  -- We use ANF to expose optimisation opportunities and use optDefs
  -- to take them.  See Note [Inline tuples] for the motiviation for
  -- doing ANF-then-optDefs.
  ; anf_alldefs <- anfDefs alldefs
  ; (env45, opt_alldefs) <- optDefs rulebase env4 anf_alldefs

  ; (env5, cse) <- cseDefs rulebase env45 opt_alldefs
  ; display "CSE" env5 cse

  ; return cse
  }

displayCppGen :: Maybe Int -> String -> String -> String -> IO ()
displayCppGen verbosity ksFile ksofile cppfile =
  let dd defs = mapM_ (liftIO . putStrLn . ("...\n" ++) . pps . flip take defs) verbosity
      display = displayPassM verbosity
  in
  runKM $
  do {
  ; decls0 <- liftIO (parseF ksFile)
  ; liftIO $ putStrLn "read decls"

  ; let (main, decls)    = moveMain decls0
  ; dd main

  ; (defs, env, rulebase) <- theDefs display decls
  ; (env3, defs, optdiffs, rulebase) <- theDiffs display defs env rulebase

  ; (env4, ann_main) <- annotDecls env3 main

  ; let (_rules, main_tdef) = partitionDecls ann_main

  ; let alldefs = defs ++ optdiffs ++ main_tdef

  ; cse <- anfOptAndCse display rulebase env4 alldefs

  ; liftIO (Cgen.cppGenWithFiles ksofile cppfile cse)
  }

displayCppGenAndCompile :: HasCallStack => (String -> String -> IO String) -> String -> Maybe Int -> String -> IO String
displayCppGenAndCompile compile ext verbosity file = do {
  ; let ksFile = file ++ ".ks"
  ; let compiler = compile
  ; let outfile = ("obj/" ++ file)
  ; let exefile = ("obj/" ++ file ++ ext)
  ; let ksofile = outfile ++ ".kso"
  ; let cppfile = outfile ++ ".cpp"
  ; displayCppGen verbosity ksFile ksofile cppfile
  ; compiler cppfile exefile
  }

displayCppGenCompileAndRun :: HasCallStack => String -> Maybe Int -> String -> IO String
displayCppGenCompileAndRun compilername verbosity file = do
  { exefile <- displayCppGenAndCompile (Cgen.compile compilername) ".exe" verbosity file
  ; Cgen.runExe exefile
  }

displayCppGenCompileAndRunWithOutput :: HasCallStack => String -> Maybe Int -> String -> IO ()
displayCppGenCompileAndRunWithOutput compilername verbosity file = do
  { output <- displayCppGenCompileAndRun compilername verbosity file
  ; putStrLn "Done"
  ; putStr output
  }

displayCppGenCompileAndRunWithOutputGpp7 :: HasCallStack => Maybe Int -> String -> IO ()
displayCppGenCompileAndRunWithOutputGpp7 = displayCppGenCompileAndRunWithOutput "g++-7"

doall :: HasCallStack => Int -> String -> IO ()
doall = displayCppGenCompileAndRunWithOutputGpp7 . Just

doallC :: HasCallStack => String -> Int -> String -> IO ()
doallC compilername = displayCppGenCompileAndRunWithOutput compilername . Just

-------------------------------------
-- The Futhark driver
-------------------------------------
futharkPipeline :: FilePath -> KM [TDef]
futharkPipeline file
  = do
  { let display _ _ _ = return ()

  ; decls0 <- liftIO (parseF (file ++ ".ks"))

  ; let decls = ignoreMain decls0

  ; (env3, defs, optdiffs, rulebase) <- defsAndDiffs display decls

  ; let env4 = env3

  ; let alldefs = defs ++ optdiffs

  ; anfOptAndCse display rulebase env4 alldefs
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
genFuthark :: FilePath -> IO ()
genFuthark file = do
  prelude <- readFile "src/runtime/knossos.fut"
  defs <- runKM $ futharkPipeline file
  putStrLn $ "Writing to " ++ futfile
  Cgen.createDirectoryWriteFile futfile $
    intercalate "\n\n" $
    prelude : map (renderSexp . ppr . Ksc.Futhark.toFuthark) defs
  where futfile = "obj/" ++ file ++ ".fut"
