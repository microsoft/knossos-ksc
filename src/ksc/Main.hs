-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# LANGUAGE LambdaCase #-}

module Main where

import Ksc.Parse (pUserFunTyped, runParser)
import Ksc.Pipeline (displayCppGenAndCompile)
import Ksc.Test (testWithfsTest, testWindowsWithfsTest, testsThatDoNoCodegen, testRunKS)
import qualified Ksc.Pipeline
import qualified Ksc.Cgen
import Data.List( intercalate )
import qualified System.Environment
import qualified System.Process
import System.Process (createProcess, proc, std_out)
import qualified System.IO

import Text.Parsec hiding (option)

-- | 'main' is the entry point of this module. It allows compiling
-- @.ks@ files, running tests and profiling.
main :: IO ()
main = do
  System.Environment.getArgs >>= \args -> case args of
    ["--test", "--fs-test", fsTest]
      -> testWithfsTest fsTest
    ["--test-windows", "--fs-test", fsTest]
      -> testWindowsWithfsTest fsTest
    ["--test-except-codegen"]
      -> do { _ <- testsThatDoNoCodegen; return () }
    [ "--profile",
      "--ks-file-without-extension", source,
      "--proffile", proffile,
      "--proffunctions", proffunctions,
      "--proflines", proflines ]
      -> profileArgs source proffile proffunctions proflines
    "--generate-cpp":rest
      -> generateCpp rest
    "--compile-and-run":rest
      -> compileAndRun rest
    ["--test-run-ks"]
      -> do
          testRunKS "g++-7" "test/ksc/gmm.ks"
          testRunKS "g++-7" "test/ksc/fold.ks"

    _ -> fail ("Unknown arguments: " ++ intercalate " " args)

parseErr :: Parsec [String] () a -> [String] -> a
parseErr p s = either (error . show) id (parse p "" s)

generateCpp :: [String] -> IO ()
generateCpp = parseErr p
  where p = do
          input  <- many (option "ks-source-file")
          ksout  <- option "ks-output-file"
          cppincludefiles <- many (option "cpp-include")
          cppout <- option "cpp-output-file"
          roots  <- pRoots

          return (Ksc.Pipeline.displayCppGen roots Nothing cppincludefiles input ksout cppout
                 >> pure ())

pRoots :: Parsec [String] u Ksc.Pipeline.Roots
pRoots = do
          switch "all-defs"
          pure Nothing
          <|>
          do
            switch "remove-unused"
            Just <$> (many $ do
              { switch "used"
              ; rootName <- satisfyS (const True)
              ; case Ksc.Parse.runParser pUserFunTyped rootName of
                      Left _ -> unexpected "Couldn't parse UserFun"
                      Right u -> pure u
              })

compileAndRun :: [String] -> IO ()
compileAndRun = parseErr p
  where p = do
          inputs   <- many (option "ks-source-file")
          ksout    <- option "ks-output-file"
          cppincludefiles <- many (option "cpp-include")
          cppout   <- option "cpp-output-file"
          roots    <- pRoots
          compiler <- option "c++"
          exeout   <- option "exe-output-file"

          return $ do
            Ksc.Pipeline.displayCppGen roots Nothing cppincludefiles inputs ksout cppout
            Ksc.Cgen.compile compiler cppout exeout
            output <- Ksc.Cgen.runExe exeout
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

profileArgs :: String -> FilePath -> FilePath -> FilePath -> IO ()
profileArgs source proffile proffunctions proflines = do
  let compiler = "g++-7"

  (exe, _) <- displayCppGenAndCompile Nothing (Ksc.Cgen.compileWithProfiling compiler) ".exe" Nothing ["prelude.h"] ["src/runtime/prelude"] source
  Ksc.Cgen.readProcessEnvPrintStderr exe [] (Just [("CPUPROFILE", proffile)])
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
