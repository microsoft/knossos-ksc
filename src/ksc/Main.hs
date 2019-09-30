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
import qualified Ksc.Futhark

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

  -- Note optgrad removed from below as we can not currently
  -- codegen the optgrad for recursive functions
  -- [see https://github.com/awf/knossos/issues/281]
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

displayCppGenAndCompile :: HasCallStack => (String -> String -> IO String) -> String -> Maybe Int -> String -> IO String
displayCppGenAndCompile compile ext verbosity file =
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

  ; let alldefs = defs ++ optdiffs ++ main_tdef

  ; cse <- anfOptAndCse display rulebase env4 alldefs

  ; let ann2 =  cse
  ; liftIO (Cgen.cppGenAndCompile compile ("obj/" ++ file) ("obj/" ++ file ++ ext) ann2)
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

hspec :: Spec
hspec = do
    Opt.hspec
    Lang.hspec
    LangUtils.hspec

main :: IO ()
main = test

test :: IO ()
test = do
  futharkCompileKscPrograms =<< ksTestFiles "test/ksc/"
  let compiler = "g++-7"
  [fsTestKs] <- System.Environment.getArgs
  testC compiler [fsTestKs]

testWindows :: IO ()
testWindows = do
  let compiler = "g++"
  [fsTestKs] <- System.Environment.getArgs
  testC compiler [fsTestKs]

ksTestFiles :: String -> IO [String]
ksTestFiles testDir = do
  let last n xs = drop (length xs - n) xs

    in fmap (map (testDir ++)
             . filter ((== ".ks") . last 3))
            (System.Directory.listDirectory testDir)

orThrowJust :: IO z -> a -> IO (Maybe a)
orThrowJust body message = fmap (const Nothing) body `Control.Exception.catch` \e -> do
  print (e :: Control.Exception.ErrorCall)
  return (Just message)

gatherErrors :: Show a => [Maybe a] -> Either String ()
gatherErrors errors =
  case Data.Maybe.catMaybes errors of
    []     -> return ()
    errors -> Left ("Had errors in:\n" ++ unlines (map show errors))

compileKscPrograms :: String -> [String] -> IO ()
compileKscPrograms compilername ksFiles = do
  putStrLn ("Testing " ++ show ksFiles)

  errors <- flip mapM ksFiles $ \ksFile -> do
    let ksTest = System.FilePath.dropExtension ksFile
    putStrLn ""
    putStrLn $ ">>>>> TEST: " ++ ksFile
    displayCppGenAndCompile (Cgen.compileWithOpts ["-c"] compilername) ".obj" Nothing ksTest
      `orThrowJust` ksFile

  case gatherErrors errors of
    Right r -> return r
    Left e  -> error e

futharkCompileKscPrograms :: [String] -> IO ()
futharkCompileKscPrograms ksFiles = do
  putStrLn ("Testing " ++ show ksFiles)

  let testsThatDon'tWorkWithFuthark =
        [ -- Doesn't handle edefs
          "test/ksc/edef.ks"
          -- Doesn't handle dummy variables
        , "test/ksc/fold.ks"
        , "test/ksc/logsumexp.ks"
        , "test/ksc/vprod.ks"
          -- Doesn't handle recursion
        , "test/ksc/power.ks"
        , "test/ksc/sum.ks"
          -- $trace not supported
        , "test/ksc/test0.ks"
        ]

  errors <- flip mapM ksFiles $ \ksFile -> do
    let ksTest = System.FilePath.dropExtension ksFile
    putStrLn ""
    putStrLn $ ">>>>> TEST: " ++ ksFile
    (if ksFile `elem` testsThatDon'tWorkWithFuthark
      then putStrLn ("Skipping " ++ ksFile
                      ++ " because it is known not to work with Futhark")
      else do
        genFuthark ksTest
        Cgen.readProcessPrintStderr
          "futhark-0.11.2-linux-x86_64/bin/futhark"
          ["check", "obj/" ++ ksTest ++ ".fut"]
        return ())
      `orThrowJust` ksFile

  case gatherErrors errors of
    Right r -> return r
    Left e  -> error e

demoFOnTestPrograms :: [String] -> IO ()
demoFOnTestPrograms ksTests = do
  putStrLn ("Testing " ++ show ksTests)

  errors <- flip mapM ksTests $ \ksTest -> do
    flip mapM [BasicAD, TupleAD] $ \adp -> do
      putStrLn ""
      putStrLn $ ">>>>> TEST: " ++ ksTest
      demoFFilter Nothing (snd . moveMain) adp ksTest
        `orThrowJust` (ksTest, adp)

  case gatherErrors (concat errors) of
    Right r -> return r
    Left e  -> error e

testRunKS :: String -> String -> IO ()
testRunKS compiler ksFile = do
  let ksTest = System.FilePath.dropExtension ksFile
  output <- displayCppGenCompileAndRun compiler Nothing ksTest

  let "TESTS FOLLOW":testResults = dropWhile (/= "TESTS FOLLOW") (lines output)

      groupedTestResults = group testResults
        where group = \case
                "----":testName:"----":testResult:rest ->
                  (testName, boolOfIntString testResult):group rest
                [] -> []
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

testC :: String -> [String] -> IO ()
testC compiler fsTestKs = do
  runSpec Main.hspec defaultConfig
  demoFOnTestPrograms =<< ksTestFiles "test/ksc/"
  compileKscPrograms compiler =<< ksTestFiles "test/ksc/"
  testRunKS compiler "test/ksc/gmm.ks"
  testRunKS compiler "test/ksc/fold.ks"
  compileKscPrograms compiler fsTestKs

profileArgs :: String -> FilePath -> FilePath -> FilePath -> IO ()
profileArgs source proffile proffunctions proflines = do
  let compiler = "g++-7"

  exe <- displayCppGenAndCompile (Cgen.compileWithProfiling compiler) ".exe" Nothing source
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

-------------------------------------
-- The Futhark driver
-------------------------------------
futharkPipeline :: FilePath -> KM [TDef]
futharkPipeline file
  = do
  { let display _ _ _ = return ()

  ; decls0 <- liftIO (parseF (file ++ ".ks"))

  ; let (_main, decls)    = moveMain decls0

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
