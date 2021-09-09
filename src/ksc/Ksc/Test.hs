module Ksc.Test where

import Ksc.Lang
import Ksc.LangUtils
import Ksc.Parse (parseE)
import Ksc.Opt
import Ksc.Pipeline (genFuthark)
import qualified Ksc.Pipeline
import qualified Ksc.Cgen
import qualified Control.Exception
import qualified Data.Maybe
import Data.List( intercalate )
import qualified System.Directory
import qualified System.FilePath
import Test.Hspec (Spec)
import Test.Hspec.Runner (isSuccess, runSpec, defaultConfig)
import Text.PrettyPrint (render)

import Control.Monad (when)

hspec :: Spec
hspec = do
    Ksc.Opt.hspec
    Ksc.Lang.hspec
    Ksc.LangUtils.hspec

testWithfsTest :: String -> IO ()
testWithfsTest fsTestKs = do
  -- futharkCompileKscPrograms =<< ksTestFiles "test/ksc/"
  let compiler = "g++-7"
  testC compiler [fsTestKs]

testWindowsWithfsTest :: String -> IO ()
testWindowsWithfsTest fsTestKs = do
  let compiler = "g++"
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
    errors -> Left ("\n\n\n--- The errors were as follows ---\n\n" ++ unlines (map show errors))

dropExtensionOrFail :: String -> FilePath -> IO String
dropExtensionOrFail ext path =
  if pathWithoutExt ++ "." ++ ext == path
  then return pathWithoutExt
  else fail (path ++ " did not end with ." ++ ext)
  where pathWithoutExt = System.FilePath.dropExtension path

testRoundTrip :: [String] -> IO ()
testRoundTrip ksFiles = do
  testOn ksFiles $ \ksFile -> do
    original <- readFile ksFile

    let render :: InPhase p => [DeclX p] -> String
        render = unlines . map (Text.PrettyPrint.render . ppr)

        parsedE = parseE original

        parsed = case parsedE of
          Left e  -> error ("Original failed to parse:\n"
                            ++ original ++ "\n"
                            ++ e)
          Right p -> p

        rendered_parsed = render parsed

        parsed_rendered_parsedE = parseE rendered_parsed

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
        if ksFile `elem` testsThatDon'tWorkWithFuthark
         then putStrLn ("ksc: Skipping " ++ ksFile
                        ++ " because it is known not to work with Futhark")
         else do
            genFuthark ["src/runtime/prelude"] ksTest
            Ksc.Cgen.readProcessPrintStderrOnFail
              "futhark-0.11.2-linux-x86_64/bin/futhark"
              ["check", "obj/" ++ ksTest ++ ".fut"]
            return ()

-- Drop items from the list while the condition is satisfied, and also
-- drop the first element satisfying the condition, if any.
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 pred xs = case dropWhile pred xs of
  []  -> []
  _:t -> t

testRunKS :: String -> [Char] -> IO ()
testRunKS compiler ksFile = do
  let ksTest = System.FilePath.dropExtension ksFile
  (output, (_, ksoContents)) <-
      Ksc.Pipeline.displayCppGenCompileAndRun
      Nothing compiler Nothing ["prelude.h"] ["src/runtime/prelude"] ksTest

  _ <- case parseE ksoContents of
          Left e -> error ("Generated .kso failed to parse:\n"
                           ++ ksoContents ++ "\n"
                           ++ e)
          Right _ -> pure ()

  let testResults = dropWhile1 (/= "TESTS FOLLOW") (lines output)

      groupedTestResults = group testResults
        where group ls = case ls of
                "----":testName:testResult:rest ->
                  (testName, boolOfIntString testResult):group rest
                [] -> []
                _ -> error ("Unexpected test result structure" ++ unlines ls)

              boolOfIntString context = case context of
                "0" -> False
                "1" -> True
                s   -> error ("boolOfIntString: Unexpected " ++ s ++ " at " ++ context)

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
  summary <- runSpec Ksc.Test.hspec defaultConfig
  when (not (isSuccess summary)) (fail "Hspec tests failed.  See message above")

testsThatDoNoCodegen :: IO [String]
testsThatDoNoCodegen = do
  testHspec
  ksTestFiles_ <- ksTestFiles "test/ksc/"
  testRoundTrip ksTestFiles_
  return ksTestFiles_

testC :: String -> [String] -> IO ()
testC compiler fsTestKs = do
  ksTestFiles_ <- testsThatDoNoCodegen
  testOn ksTestFiles_ (testRunKS compiler)
  testOn fsTestKs (testRunKS compiler)
