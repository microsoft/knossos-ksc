-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE LambdaCase #-}

module Main where

import GHC.Stack

import Lang
import LangUtils
import Parse (parseF)
import Rules
import Annotate
import AD
import Opt
import CSE

import ANF
import qualified Cgen
import KMonad
import qualified Control.Exception
import qualified Data.Maybe
import Data.List( partition, intercalate )
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath
import qualified System.Process
import System.Process (createProcess, proc, std_out)
import qualified System.IO
import Test.Hspec (Spec)
import Test.Hspec.Runner (runSpec, defaultConfig)


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

{-
       ; let trans_grad_defs = gradDefs (AD adp Rev) anf_defs
             env5      = env1 `extendGblST` trans_grad_defs
       ; disp "Transposed Jacobian" env5 trans_grad_defs

       ; (env6, opt_trans_grad_defs) <- optDefs rulebase env5 trans_grad_defs
       ; disp "Optimised transposed Jacobian" env6 opt_trans_grad_defs
-}

       ; let der_rev = applyDefs Rev opt_grad_defs
       ; disp "Reverse-mode derivative (unoptimised)" env3 der_rev

       ; (env7, opt_der_rev) <- optDefs rulebase env3 der_rev
       ; disp "Reverse-mode derivative (optimised)" env7 opt_der_rev

       ; (env8, cse_rev) <- cseDefs rulebase env7 opt_der_rev
       ; disp "Reverse-mode derivative (CSE'd)" env8 cse_rev
       }

displayPassM :: Maybe Int -> String -> GblSymTab -> [TDef] -> KM ()
displayPassM mverbosity what env decls
  = do { flip mapM_ mverbosity $ \verbosity -> do
           banner what
           displayN (take verbosity decls)
       ; lintDefs what env decls
    }

displayPass :: Int -> String -> GblSymTab -> [TDef] -> KM ()
displayPass = displayPassM . Just

-------------------------------------
-- GMM derivatives
-------------------------------------

moveMain :: [Decl]
         -> ( [Decl]    -- Singleton 'main' decl, or empty
            , [Decl])   -- All the rest
moveMain = partition isMain
  where
    isMain (DefDecl (Def { def_fun = Fun (UserFun "main") })) = True
    isMain _ = False

defsAndDiffs :: (String -> GblSymTab -> [TDef] -> KM a)
             -> [Decl]
             -> KM (GblSymTab, [TDef], [TDef], RuleBase)
defsAndDiffs display decls = do {
  ; (env, ann_decls) <- annotDecls emptyGblST decls
  ; let (rules, defs) = partitionDecls ann_decls
  ; let rulebase      = mkRuleBase rules
  ; display "Typechecked defs" env defs

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

  ; return (env3, defs, optdiffs, rulebase)
  }

anfOptAndCse :: (String -> GblSymTab -> [TDef] -> KM a)
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

displayCppGenAndCompile :: HasCallStack => (String -> String -> IO String) -> Maybe Int -> String -> IO String
displayCppGenAndCompile compile verbosity file =
  let dd defs = mapM_ (liftIO . putStrLn . ("...\n" ++) . pps . flip take defs) verbosity
      display = displayPassM verbosity
  in
  runKM $
  do { decls0 <- liftIO (parseF (file ++ ".ks"))
  ; liftIO $ putStrLn "read decls"

  ; let (main, decls)    = moveMain decls0
  ; dd main

  ; (env3, defs, optdiffs, rulebase) <- defsAndDiffs display decls

  ; (env4, ann_main) <- annotDecls env3 main

  ; let (_rules, main_tdef) = partitionDecls ann_main

  -- Note optgrad removed from below as we can not currently
  -- codegen the optgrad for recursive functions
  -- [see https://github.com/awf/knossos/issues/281]
  ; let alldefs = defs ++ optdiffs ++ main_tdef

  ; cse <- anfOptAndCse display rulebase env4 alldefs

  ; let ann2 =  cse
  ; liftIO (Cgen.cppGenAndCompile compile ("obj/" ++ file) ann2)
  }

displayCppGenAndCompileS :: HasCallStack => String -> Maybe Int -> String -> IO String
displayCppGenAndCompileS compiler = displayCppGenAndCompile (Cgen.compile compiler)

displayCppGenCompileAndRun :: HasCallStack => String -> Maybe Int -> String -> IO String
displayCppGenCompileAndRun compiler verbosity file = do
  { exefile <- displayCppGenAndCompileS compiler verbosity file
  ; Cgen.runExe exefile
  }

displayCppGenCompileAndRunWithOutput :: HasCallStack => String -> Maybe Int -> String -> IO ()
displayCppGenCompileAndRunWithOutput compiler verbosity file = do
  { output <- displayCppGenCompileAndRun compiler verbosity file
  ; putStrLn "Done"
  ; putStr output
  }

displayCppGenCompileAndRunWithOutputGpp7 :: HasCallStack => Maybe Int -> String -> IO ()
displayCppGenCompileAndRunWithOutputGpp7 = displayCppGenCompileAndRunWithOutput "g++-7"

doall :: HasCallStack => Int -> String -> IO ()
doall = displayCppGenCompileAndRunWithOutputGpp7 . Just

doallE :: HasCallStack => (String -> String -> IO String) -> Int -> String -> IO String
doallE compile = displayCppGenAndCompile compile . Just

doallC :: HasCallStack => String -> Int -> String -> IO ()
doallC compiler = displayCppGenCompileAndRunWithOutput compiler . Just

doallG :: HasCallStack => String -> Int -> String -> IO String
doallG compiler = displayCppGenCompileAndRun compiler . Just

gmm :: IO ()
gmm = displayCppGenCompileAndRunWithOutputGpp7 (Just 400) "test/ksc/gmm"

main :: IO ()
main = gmm

hspec :: Spec
hspec = do
    Opt.hspec
    Lang.hspec
    LangUtils.hspec

test :: IO ()
test = do
  let compiler = "g++-7"
  testC compiler
  [fsTestKs] <- System.Environment.getArgs
  compileKscPrograms compiler [fsTestKs]

testWindows :: IO ()
testWindows = do
  let compiler = "g++"
  testC compiler
  [fsTestKs] <- System.Environment.getArgs
  compileKscPrograms compiler [fsTestKs]

ksTestFiles :: String -> IO [String]
ksTestFiles testDir = do
  let last n xs = drop (length xs - n) xs

      naughtyTestsThatDon'tWorkButShouldBeFixedAndRemovedFromThisList
        = ["logsumexp.ks"]

    in fmap (map (testDir ++)
             . filter (not . (`elem` naughtyTestsThatDon'tWorkButShouldBeFixedAndRemovedFromThisList))
             . filter ((== ".ks") . last 3))
            (System.Directory.listDirectory testDir)

compileKscTestPrograms :: String -> IO ()
compileKscTestPrograms compiler = compileKscPrograms compiler =<< ksTestFiles "test/ksc/"

compileKscPrograms :: String -> [String] -> IO ()
compileKscPrograms compiler ksFiles = do
  putStrLn ("Testing " ++ show ksFiles)

  errors <- flip mapM ksFiles $ \ksFile -> do
    let ksTest = System.FilePath.dropExtension ksFile
    putStrLn ""
    putStrLn $ ">>>>> TEST: " ++ ksFile
    fmap (const Nothing) (displayCppGenAndCompile (Cgen.compileWithOpts ["-c"] compiler) Nothing ksTest)
      `Control.Exception.catch` \e -> do
        print (e :: Control.Exception.ErrorCall)
        return (Just ksFile)

  case Data.Maybe.catMaybes errors of
    []     -> return ()
    errors -> error ("Had errors in:\n" ++ unlines errors)

demoFOnTestPrograms :: IO ()
demoFOnTestPrograms = do
  ksTests <- ksTestFiles "test/ksc/"

  putStrLn ("Testing " ++ show ksTests)

  errors <- flip mapM ksTests $ \ksTest -> do
    flip mapM [BasicAD, TupleAD] $ \adp -> do
      putStrLn ""
      putStrLn $ ">>>>> TEST: " ++ ksTest
      fmap (const Nothing) (demoFFilter Nothing (snd . moveMain) adp ksTest)
        `Control.Exception.catch` \e -> do
          print (e :: Control.Exception.ErrorCall)
          return (Just (ksTest, adp))

  case Data.Maybe.catMaybes (concat errors) of
    []     -> return ()
    errors -> error ("Had errors in:\n" ++ unlines (map show errors))

testRunKS :: String -> String -> IO ()
testRunKS compiler ksFile = do
  let ksTest = System.FilePath.dropExtension ksFile
  output <- displayCppGenCompileAndRun compiler Nothing ksTest

  let "TESTS FOLLOW":"----":testResults = dropWhile (/= "TESTS FOLLOW") (lines output)

      groupedTestResults = group testResults
        where group = \case
                testName:"----":testResult:[] ->
                  [(testName, boolOfIntString testResult)]
                testName:"----":testResult:"----":rest ->
                  (testName, boolOfIntString testResult):group rest
                _ -> error "Unexpected test result structure"

              boolOfIntString = \case
                "0" -> False
                "1" -> True
                s   -> error ("boolOfIntString: Unexpected " ++ s)

      failed   = not . snd
      failures = map fst (filter failed groupedTestResults)

  case failures of
    []  -> putStrLn ("All "
                     ++ show (length groupedTestResults)
                     ++ " tests passed: "
                     ++ intercalate ", " (map fst groupedTestResults))
    _:_ -> do
      putStrLn (unlines (reverse (take 30 (reverse (lines output)))))
      error ("These tests failed:\n" ++ unlines failures)

testGMM :: String -> IO ()
testGMM compiler = testRunKS compiler "test/ksc/gmm.ks"

testC :: String -> IO ()
testC compiler = do
  runSpec Main.hspec defaultConfig
  demoFOnTestPrograms
  compileKscTestPrograms compiler
  testGMM compiler

profileArgs :: String -> FilePath -> FilePath -> FilePath -> IO ()
profileArgs source proffile proffunctions proflines = do
  let compiler = "g++-7"

  exe <- displayCppGenAndCompile (Cgen.compileWithProfiling compiler) Nothing source
  Cgen.readProcessEnvPrintStderr exe [] (Just [("CPUPROFILE", proffile)])
  withOutputFileStream proflines $ \std_out -> createProcess
    (proc "google-pprof" ["--text", "--lines", exe, proffile]) { std_out = std_out
                                                               }
  withOutputFileStream proffunctions $ \std_out ->
    createProcess (proc "google-pprof" ["--text", "--functions", exe, proffile])
      { std_out = std_out
      }
  return ()

withOutputFileStream
  :: FilePath -> (System.Process.StdStream -> IO r) -> IO r
withOutputFileStream filename handle =
  System.IO.withBinaryFile
    filename
    System.IO.WriteMode
    (handle . System.Process.UseHandle)

profile :: IO ()
profile = do
  [source, proffile, proffunctions, proflines] <- System.Environment.getArgs
  profileArgs source proffile proffunctions proflines
