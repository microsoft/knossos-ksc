-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE LambdaCase #-}

module Main where

import Lang
import LangUtils
import Parse (parseE)
import Opt
import Ksc.Pipeline (demoFFilter,
                     displayCppGenCompileAndRun,
                     displayCppGenAndCompile,
                     displayCppGenNoDiffs,
                     genFuthark, ignoreMain)
import qualified Ksc.Pipeline
import qualified Cgen
import qualified Control.Exception
import qualified Data.Maybe
import Data.List( intercalate )
import qualified System.Directory
import qualified System.Environment
import qualified System.FilePath
import qualified System.Process
import System.Process (createProcess, proc, std_out)
import qualified System.IO
import Test.Hspec (Spec)
import Test.Hspec.Runner (isSuccess, runSpec, defaultConfig)

import Control.Monad (when)
import Text.Parsec hiding (option)

hspec :: Spec
hspec = do
    Opt.hspec
    Lang.hspec
    LangUtils.hspec

main :: IO ()
main = do
  System.Environment.getArgs >>= \case
    ["--test", "--fs-test", fsTest]
      -> testWithfsTest fsTest
    [ "--profile",
      "--ks-file-without-extension", source,
      "--proffile", proffile,
      "--proffunctions", proffunctions,
      "--proflines", proflines ]
      -> profileArgs source proffile proffunctions proflines
    "--generate-cpp-without-diffs":rest
      -> generateCppWithoutDiffs rest
    "--compile-and-run":rest
      -> compileAndRun rest

    _ -> fail "Unknown arguments"

parseErr :: Parsec [String] () a -> [String] -> a
parseErr p s = either (error . show) id (parse p "" s)

generateCppWithoutDiffs :: [String] -> IO ()
generateCppWithoutDiffs = parseErr p
  where p = do
          input  <- many (option "ks-source-file")
          ksout  <- option "ks-output-file"
          cppout <- option "cpp-output-file"

          return (Ksc.Pipeline.displayCppGenNoDiffs
                   Nothing input ksout cppout)

compileAndRun :: [String] -> IO ()
compileAndRun = parseErr p
  where p = do
          inputs   <- many (option "ks-source-file")
          ksout    <- option "ks-output-file"
          cppout   <- option "cpp-output-file"
          compiler <- option "c++"
          exeout   <- option "exe-output-file"

          return $ do
            Ksc.Pipeline.displayCppGenDiffs
               Ksc.Pipeline.theDiffs Nothing inputs ksout cppout
            Cgen.compile compiler cppout exeout
            output <- Cgen.runExe exeout
            putStrLn output

satisfyS :: Monad m => (String -> Bool) -> ParsecT [String] u m String
satisfyS f = tokenPrim id
                       (\pos _ _ -> pos)
                       (\s -> if f s then Just s else Nothing)

switch :: String -> Parsec [String] u ()
switch s = satisfyS (== ("--" ++ s)) >> return ()

option :: String -> Parsec [String] u String
option s = do
  switch s
  anyToken

testWithfsTest :: String -> IO ()
testWithfsTest fsTestKs = do
  --futharkCompileKscPrograms =<< ksTestFiles "test/ksc/"
  let compiler = "g++-7"
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

testOn :: Show t => [t] -> (t -> IO z) -> IO ()
testOn cases f = do
  putStrLn ("Testing " ++ show cases)

  errors <- flip mapM cases $ \case_ -> do
    putStrLn ""
    putStrLn $ ">>>>> TEST: " ++ show case_
    f case_ `orThrowJust` case_

  case gatherErrors errors of
    Right r -> return r
    Left e  -> error e

orThrowJust :: IO z -> a -> IO (Maybe a)
orThrowJust body message = fmap (const Nothing) body `Control.Exception.catch` \e -> do
  print (e :: Control.Exception.ErrorCall)
  return (Just message)

gatherErrors :: Show a => [Maybe a] -> Either String ()
gatherErrors errors =
  case Data.Maybe.catMaybes errors of
    []     -> return ()
    errors -> Left ("Had errors in:\n" ++ unlines (map show errors))

dropExtensionOrFail :: String -> FilePath -> IO String
dropExtensionOrFail ext path =
  if pathWithoutExt ++ "." ++ ext == path
  then return pathWithoutExt
  else fail (path ++ " did not end with ." ++ ext)
  where pathWithoutExt = System.FilePath.dropExtension path

compileKscPrograms :: String -> [String] -> IO ()
compileKscPrograms compilername ksFiles = do
  testOn ksFiles $ \ksFile -> do
        ksTest <- dropExtensionOrFail "ks" ksFile
        displayCppGenAndCompile (Cgen.compileWithOpts ["-c"] compilername) ".obj" Nothing ["src/runtime/prelude"] ksTest

testRoundTrip :: [String] -> IO ()
testRoundTrip ksFiles = do
  testOn ksFiles $ \ksFile -> do
    original <- readFile ksFile

    let render :: InPhase p => [DeclX p] -> String
        render = unlines . map (renderSexp . ppr)

        parseIgnoringMain :: String -> Either String [Decl]
        parseIgnoringMain = fmap ignoreMain . parseE

        parsedE = parseIgnoringMain original

        parsed = case parsedE of
          Left e  -> error ("Original failed to parse:\n"
                            ++ original ++ "\n"
                            ++ e)
          Right p -> p

        rendered_parsed = render parsed

        parsed_rendered_parsedE = parseIgnoringMain rendered_parsed

        parsed_rendered_parsed = case parsed_rendered_parsedE of
          Left e  -> error ("Round-tripped failed to parse:\n"
                            ++ rendered_parsed ++ "\n"
                            ++ e)
          Right p -> p

    -- It's unlikely that
    --
    --     original == render parsed
    --
    -- because the rendered version will have different whitespace to
    -- the original so instead we test
    --
    --     parsed = parsed_rendered_parsed
    if parsed /= parsed_rendered_parsed
      then do
        print parsed
        print parsed_rendered_parsed
        error "Round trip failure"
      else return ()

futharkCompileKscPrograms :: [String] -> IO ()
futharkCompileKscPrograms ksFiles = do
  let testsThatDon'tWorkWithFuthark =
        [ -- Doesn't handle edefs
          "test/ksc/edef.ks"
          -- Doesn't handle dummy variables
        , "test/ksc/adbench-lstm.ks"
        , "test/ksc/fold.ks"
        , "test/ksc/logsumexp.ks"
        , "test/ksc/vprod.ks"
        , "test/ksc/syntax-primer.ks"
          -- Doesn't handle recursion
        , "test/ksc/power.ks"
        , "test/ksc/sum.ks"
          -- $trace not supported
        , "test/ksc/test0.ks"
          -- Seems not to handle negative float literals
        , "test/ksc/negative-float-literals.ks"
        ]

  testOn ksFiles $ \ksFile -> do
        ksTest <- dropExtensionOrFail "ks" ksFile
        (if ksFile `elem` testsThatDon'tWorkWithFuthark
         then putStrLn ("Skipping " ++ ksFile
                        ++ " because it is known not to work with Futhark")
         else do
            genFuthark ["src/runtime/prelude"] ksTest
            Cgen.readProcessPrintStderr
              "futhark-0.11.2-linux-x86_64/bin/futhark"
              ["check", "obj/" ++ ksTest ++ ".fut"]
            return ())

demoFOnTestPrograms :: [String] -> IO ()
demoFOnTestPrograms ksTests = do
  let ksTestsInModes :: [(String, ADPlan)]
      ksTestsInModes = (,) <$> ksTests <*> [BasicAD, TupleAD]

  testOn ksTestsInModes $ \(ksTest, adp) -> do
        demoFFilter Nothing ignoreMain adp ["src/runtime/prelude.ks", ksTest]

-- Drop items from the list while the condition is satisfied, and also
-- drop the first element satisfying the condition, if any.
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 f = tail . dropWhile f

testRunKS :: String -> String -> IO ()
testRunKS compiler ksFile = do
  let ksTest = System.FilePath.dropExtension ksFile
  output <- displayCppGenCompileAndRun compiler Nothing ["src/runtime/prelude"] ksTest

  let testResults = dropWhile1 (/= "TESTS FOLLOW") (lines output)

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

testHspec :: IO ()
testHspec = do
  summary <- runSpec Main.hspec defaultConfig
  when (not (isSuccess summary)) (fail "Hspec tests failed.  See message above")

testC :: String -> [String] -> IO ()
testC compiler fsTestKs = do
  testHspec
  ksTestFiles_ <- ksTestFiles "test/ksc/"
  testRoundTrip ksTestFiles_
  demoFOnTestPrograms ksTestFiles_
  compileKscPrograms compiler ksTestFiles_
  testRunKS compiler "test/ksc/gmm.ks"
  testRunKS compiler "test/ksc/fold.ks"
  compileKscPrograms compiler fsTestKs

profileArgs :: String -> FilePath -> FilePath -> FilePath -> IO ()
profileArgs source proffile proffunctions proflines = do
  let compiler = "g++-7"

  exe <- displayCppGenAndCompile (Cgen.compileWithProfiling compiler) ".exe" Nothing ["src/runtime/prelude"] source
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
