-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
module Main where

import GHC.Stack
import qualified System.Exit

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
import Data.List( partition )
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
demoF = demoFFilter id

demoFFilter :: ([Decl] -> [Decl]) -> ADPlan -> String -> IO ()
demoFFilter theFilter adp file = do
  defs <- parseF file
  runKM (demoN adp (theFilter defs))

demo :: Decl -> IO ()
demo d = runKM (demoN BasicAD [d])

demoN :: ADPlan -> [Decl] -> KM ()
demoN adp decls
  = let disp = displayPass 999 in
    do { banner "Original declarations"
       ; displayN decls

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

displayCppGenAndCompile :: HasCallStack => (String -> String -> IO String) -> Maybe Int -> String -> IO String
displayCppGenAndCompile compile verbosity file =
  let dd defs = mapM_ (liftIO . putStrLn . ("...\n" ++) . pps . flip take defs) verbosity
  in
  runKM $
  do { decls0 <- liftIO (parseF (file ++ ".ks"))
  ; liftIO $ putStrLn "read decls"

  ; let (main, decls)    = moveMain decls0

  ; (env, ann_decls) <- annotDecls emptyGblST decls
  ; let (rules, defs) = partitionDecls ann_decls
  ; let rulebase      = mkRuleBase rules
  ; displayPassM verbosity "Typechecked defs" env defs

  ; dd main

  ; let grad_defs = gradDefs BasicAD defs
        env1 = env `extendGblST` grad_defs
  ; displayPassM verbosity "Grad" env1 grad_defs

  -- We generate grad_defs_tupled even though we do not use it yet.
  -- We do not use it because
  --
  -- 1. lmApplys are not completely removed in forward mode, and
  --
  -- 2. lmApplys are deliberately not removed in reverse mode but we
  -- don't yet implement the necessary LM constructors in C++.
  --
  -- Nonetheless, it's good to generated optgrad_tupled here so that
  -- the tests will run on it and we can be sure it typechecks.
  ; let grad_defs_tupled = gradDefs TupleAD defs
        env15 = env1 `extendGblST` grad_defs_tupled
  ; displayPassM verbosity "Grad tupled" env15 grad_defs_tupled

  ; (env2, optgrad) <- optDefs rulebase env15 grad_defs
  ; displayPassM verbosity "Optgrad" env2 optgrad

  ; (env25, optgrad_tupled) <- optDefs rulebase env2 grad_defs_tupled
  ; displayPassM verbosity "Optgrad tupled" env25 optgrad_tupled

  ; let diffs = applyDefs Fwd optgrad ++ applyDefs Rev optgrad
  ; displayPassM verbosity "Diffs" env25 diffs

  ; (env3, optdiffs) <- optDefs rulebase env25 diffs
  ; displayPassM verbosity "OptDiffs" env3 optdiffs

  ; (env4, ann_main) <- annotDecls env3 main

  ; let (_rules, main_tdef) = partitionDecls ann_main

  -- Note optgrad removed from below as we can not currently
  -- codegen the optgrad for recursive functions
  -- [see https://github.com/awf/knossos/issues/281]
  ; let alldefs = defs ++ optdiffs ++ main_tdef

  -- We use ANF to expose optimisation opportunities and use optDefs
  -- to take them.  See Note [Inline tuples] for the motiviation for
  -- doing ANF-then-optDefs.
  ; anf_alldefs <- anfDefs alldefs
  ; (env45, opt_alldefs) <- optDefs rulebase env4 anf_alldefs

  ; (env5, cse) <- cseDefs rulebase env45 opt_alldefs
  ; displayPassM verbosity "CSE" env5 cse

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
test = testC "g++-7"

testWindows :: IO ()
testWindows = testC "g++"

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
compileKscTestPrograms compiler = do
  ksFiles <- ksTestFiles "test/ksc/"

  let ksTests = map System.FilePath.dropExtension ksFiles

  putStrLn ("Testing " ++ show ksTests)

  errors <- flip mapM ksTests $ \ksTest -> do
    putStrLn ""
    putStrLn $ ">>>>> TEST: " ++ ksTest ++ ".ks"
    fmap (const Nothing) (displayCppGenAndCompile (Cgen.compileWithOpts ["-c"] compiler) Nothing ksTest)
      `Control.Exception.catch` \e -> do
        print (e :: Control.Exception.ErrorCall)
        return (Just ksTest)

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
      fmap (const Nothing) (demoFFilter (snd . moveMain) adp ksTest)
        `Control.Exception.catch` \e -> do
          print (e :: Control.Exception.ErrorCall)
          return (Just (ksTest, adp))

  case Data.Maybe.catMaybes (concat errors) of
    []     -> return ()
    errors -> error ("Had errors in:\n" ++ unlines (map show errors))

testGMM :: String -> IO ()
testGMM compiler = do
  output <- displayCppGenCompileAndRun compiler Nothing "test/ksc/gmm"

  let success = case reverse (lines output) of
        notImpossiblyGoodS:_:everythingWorksAsExpectedS:_:everythingWorksAsExpectedReverseS:_:goldenGMMS:_ ->
          let boolOfIntString s = case s of
                "0" -> False
                "1" -> True
                _   -> error ("boolOfIntString: Unexpected " ++ s)

          in all boolOfIntString [ goldenGMMS
                                 , everythingWorksAsExpectedReverseS
                                 , everythingWorksAsExpectedS
                                 , notImpossiblyGoodS ]
        _ -> False

  if success
    then do
    putStrLn "Success"
    System.Exit.exitWith System.Exit.ExitSuccess
    else do
    putStrLn ("FAILURE!" ++ unlines (reverse (take 5 (reverse (lines output)))))
    System.Exit.exitWith (System.Exit.ExitFailure 1)

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
