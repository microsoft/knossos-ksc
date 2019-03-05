module Main where

import GHC.Stack
import qualified System.Exit

import Lang
import LangUtils (hspec)
import Parse (runParser, pDecls, parseF )
import Rules
import Annotate
import AD
import Opt
import CSE

import ANF
import qualified Cgen
import KMonad
import Data.List( partition )
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath
import qualified System.Process
import System.Process (createProcess, proc, std_out)
import qualified System.IO
import Test.Hspec (hspec, Spec)


-------------------------------------
--  The demo driver
-------------------------------------

demoF :: String -> IO ()
-- Read source code from specified input file, optimise,
-- differentiate, optimise, and display results of each step
demoF file
  = do { cts <- readFile file
       ; case runParser pDecls cts of
            Left err   -> putStrLn ("Failed parse: " ++ show err)
            Right defs -> runKM (demoN defs) }

demo :: Decl -> IO ()
demo d = runKM (demoN [d])

demoN :: [Decl] -> KM ()
demoN decls
  = let disp = displayPass 999 in
    do { banner "Original declarations"
       ; displayN decls

       ; (env, tc_decls) <- annotDecls emptyGblST decls
       ; let (rules, defs)   = partitionDecls tc_decls
             rulebase        = mkRuleBase rules

       ; disp "Typechecked declarations" env defs

       ; let (env1, opt_defs) = optDefs rulebase env defs
       ; disp "Optimized original definition" env1 opt_defs

       ; anf_defs <- anfDefs opt_defs
       ; disp "Anf-ised original definition" env1 anf_defs

       ; let grad_defs = gradDefs anf_defs
             env2      = env1 `extendGblST` grad_defs
       ; disp "The full Jacobian (unoptimised)" env2 grad_defs

       ; let (env3, opt_grad_defs) = optDefs rulebase env2 grad_defs
       ; disp "The full Jacobian (optimised)" env3 opt_grad_defs

       ; let der_fwd = applyDefs opt_grad_defs
       ; disp "Forward derivative (unoptimised)" env3 der_fwd

       ; let (env4, opt_der_fwd) = optDefs rulebase env3 der_fwd
       ; disp "Forward-mode derivative (optimised)" env4 opt_der_fwd

       ; (env5, cse_fwd) <- cseDefs rulebase env4 opt_der_fwd
       ; disp "Forward-mode derivative (CSE'd)" env5 cse_fwd

       ; let trans_grad_defs = map transposeD opt_grad_defs
       ; disp "Transposed Jacobian" env5 trans_grad_defs

       ; let (env6, opt_trans_grad_defs) = optDefs rulebase env5 trans_grad_defs
       ; disp "Optimised transposed Jacobian" env6 opt_trans_grad_defs

       ; let der_rev = applyDefs opt_trans_grad_defs
       ; disp "Reverse-mode derivative (unoptimised)" env6 der_rev

       ; let (env7, opt_der_rev) = optDefs rulebase env6 der_rev
       ; disp "Reverse-mode derivative (optimised)" env7 opt_der_rev

       ; (env8, cse_rev) <- cseDefs rulebase env7 opt_der_rev
       ; disp "Reverse-mode derivative (CSE'd)" env8 cse_rev
       }

displayPass :: Int -> String -> GblSymTab -> [TDef] -> KM ()
displayPass verbosity what env decls
  = do { banner what
       ; displayN (take verbosity decls)
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
    isMain (DefDecl (DefX (TFun _ (Fun (UserFun "main"))) _ _)) = True
    isMain _ = False

displayCppGenAndCompile :: HasCallStack => (String -> String -> IO String) -> Int -> String -> IO String
displayCppGenAndCompile compile verbosity file =
  let dd defs = liftIO $ putStrLn ("...\n" ++ (pps $ take verbosity defs))
  in
  runKM $
  do { decls0 <- liftIO (parseF (file ++ ".ks"))
  ; liftIO $ putStrLn "read decls"

  ; let (main, decls)    = moveMain decls0

  ; (env, ann_decls) <- annotDecls emptyGblST decls
  ; let (rules, defs) = partitionDecls ann_decls
  ; let rulebase      = mkRuleBase rules
  ; displayPass verbosity "Typechecked defs" env defs

  ; banner "main"
  ; dd main

  ; let grad_defs = gradDefs defs
        env1 = env `extendGblST` grad_defs
  ; displayPass verbosity "Grad" env1 grad_defs

  ; let trans_grad_defs = map transposeD grad_defs

  ; let (env2, optgrad) = optDefs rulebase env1 (grad_defs ++ trans_grad_defs)
  ; displayPass verbosity "Optgrad" env2 optgrad

  ; let fwd = applyDefs optgrad
  ; displayPass verbosity "Fwd" env2 fwd

  ; let (env3, optfwd) = optDefs rulebase env2 fwd
  ; displayPass verbosity "OptFwd" env3 optfwd

  ; (env4, ann_main) <- annotDecls env3 main

  ; let (_rules, main_tdef) = partitionDecls ann_main

  -- Note optgrad removed from below as we can not currently 
  -- codegen the optgrad for recursive functions 
  -- [see https://github.com/awf/knossos/issues/281]
  ; let alldefs = defs ++ optfwd ++ main_tdef     

  ; (env5, cse) <- cseDefs rulebase env4 alldefs
  ; displayPass verbosity "CSE" env5 cse

  ; let ann2 =  cse
  ; liftIO (Cgen.cppGenAndCompile compile ("obj/" ++ file) ann2)
  }

displayCppGenCompileAndRun :: HasCallStack => String -> Int -> String -> IO String
displayCppGenCompileAndRun compiler verbosity file = do
  { exefile <- displayCppGenAndCompile (Cgen.compile compiler) verbosity file
  ; Cgen.runExe exefile
  }

displayCppGenCompileAndRunWithOutput :: HasCallStack => String -> Int -> String -> IO ()
displayCppGenCompileAndRunWithOutput compiler verbosity file = do
  { output <- displayCppGenCompileAndRun compiler verbosity file
  ; putStrLn "Done"
  ; putStr output
  }

displayCppGenCompileAndRunWithOutputGpp7 :: HasCallStack => Int -> String -> IO ()
displayCppGenCompileAndRunWithOutputGpp7 = displayCppGenCompileAndRunWithOutput "g++-7"

doall :: HasCallStack => Int -> String -> IO ()
doall = displayCppGenCompileAndRunWithOutputGpp7

doallE :: HasCallStack => (String -> String -> IO String) -> Int -> String -> IO String
doallE = displayCppGenAndCompile

doallC :: HasCallStack => String -> Int -> String -> IO ()
doallC = displayCppGenCompileAndRunWithOutput

doallG :: HasCallStack => String -> Int -> String -> IO String
doallG = displayCppGenCompileAndRun

gmm :: IO ()
gmm = displayCppGenCompileAndRunWithOutputGpp7 400 "test/ksc/gmm"

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

compileKscTestPrograms :: String -> IO ()
compileKscTestPrograms compiler = do
  let last n xs = drop (length xs - n) xs
      testDir = "test/ksc/"

      naughtyTestsThatDon'tWorkButShouldBeFixedAndRemovedFromThisList
        = ["ex2.ks", "CA-subst.ks", "inline.ks", "logsumexp.ks", "test_stopgrad.ks"]

  ksFiles <- fmap (filter (not . (`elem` naughtyTestsThatDon'tWorkButShouldBeFixedAndRemovedFromThisList))
                    . filter ((== ".ks") . last 3))
             (System.Directory.listDirectory testDir)

  let ksTests = map ((testDir ++) . System.FilePath.dropExtension) ksFiles

  putStrLn ("Testing " ++ show ksTests)

  flip mapM_ ksTests $ \ksTest -> do
    putStrLn ksTest
    (displayCppGenAndCompile (Cgen.compileWithOpts ["-c"] compiler) 0) ksTest

testC :: String -> IO ()
testC compiler = do
  Test.Hspec.hspec Main.hspec
  compileKscTestPrograms compiler

  output <- displayCppGenCompileAndRun compiler 0 "test/ksc/gmm"

  let success = case reverse (lines output) of
        impossiblyGoodS:_:everythingWorksAsExpectedS:_:everythingWorksAsExpectedReverseS:_ ->
          let boolOfIntString s = case s of
                "0" -> False
                "1" -> True
                _   -> error ("boolOfIntString: Unexpected " ++ s)

              everythingWorksAsExpectedReverse = boolOfIntString everythingWorksAsExpectedReverseS
              everythingWorksAsExpected = boolOfIntString everythingWorksAsExpectedS
              impossiblyGood = boolOfIntString impossiblyGoodS
          in everythingWorksAsExpectedReverse && everythingWorksAsExpected && not impossiblyGood
        _ -> False

  if success
    then do
    putStrLn "Success"
    System.Exit.exitWith System.Exit.ExitSuccess
    else do
    putStrLn ("FAILURE!" ++ unlines (reverse (take 5 (reverse (lines output)))))
    System.Exit.exitWith (System.Exit.ExitFailure 1)

profileArgs :: String -> FilePath -> FilePath -> FilePath -> IO ()
profileArgs source proffile proffunctions proflines = do
  let compiler = "g++-7"

  exe <- displayCppGenAndCompile (Cgen.compileWithProfiling compiler) 0 source
  Cgen.readProcessEnvPrintStderr exe [] (Just [("CPUPROFILE", proffile)])
  withOutputFileStream proffunctions $ \std_out -> createProcess
    (proc "google-pprof" ["--text", "--lines", exe, proffile]) { std_out = std_out
                                                               }
  withOutputFileStream proflines $ \std_out ->
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
