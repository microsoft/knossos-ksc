module Main where

import GHC.Stack;
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
import Cgen (cppFG)
import KMonad
import Data.List( partition )
import qualified System.Directory
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
  = do { banner "Original declarations"
       ; displayN decls

       ; (env, tc_decls) <- annotDecls emptyGblST decls
       ; let (rules, defs)   = partitionDecls tc_decls
             rulebase        = mkRuleBase rules

       ; displayPass "Typechecked declarations" env defs

       ; let (env1, opt_defs) = optDefs rulebase env defs
       ; displayPass "Optimised original definition" env1 opt_defs

       ; anf_defs <- anfDefs opt_defs
       ; displayPass "Anf-ised original definition" env1 anf_defs

       ; let grad_defs = gradDefs anf_defs
             env2      = env1 `extendGblST` grad_defs
       ; displayPass "The full Jacobian (unoptimised)" env2 grad_defs

       ; let (env3, opt_grad_defs) = optDefs rulebase env2 grad_defs
       ; displayPass "The full Jacobian (optimised)" env3 opt_grad_defs

       ; let der_fwd = applyDefs opt_grad_defs
       ; displayPass "Forward derivative (unoptimised)" env3 der_fwd

       ; let (env4, opt_der_fwd) = optDefs rulebase env3 der_fwd
       ; displayPass "Forward-mode derivative (optimised)" env4 opt_der_fwd

       ; (env5, cse_fwd) <- cseDefs rulebase env4 opt_der_fwd
       ; displayPass "Forward-mode derivative (CSE'd)" env5 cse_fwd

       ; let trans_grad_defs = map transposeD opt_grad_defs
       ; displayPass "Transposed Jacobian" env5 trans_grad_defs

       ; let (env6, opt_trans_grad_defs) = optDefs rulebase env5 trans_grad_defs
       ; displayPass "Optimised transposed Jacobian" env6 opt_trans_grad_defs

       ; let der_rev = applyDefs opt_trans_grad_defs
       ; displayPass "Reverse-mode derivative (unoptimised)" env6 der_rev

       ; let (env7, opt_der_rev) = optDefs rulebase env6 der_rev
       ; displayPass "Reverse-mode derivative (optimised)" env7 opt_der_rev

       ; (env8, cse_rev) <- cseDefs rulebase env7 opt_der_rev
       ; displayPass "Reverse-mode derivative (CSE'd)" env8 cse_rev
       }

displayPass :: String -> GblSymTab -> [TDef] -> KM ()
displayPass what env decls
  = do { banner what
       ; displayN (take 4 $! decls)
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
    isMain (DefDecl (DefX (Fun (UserFun "main")) _ _)) = True
    isMain _ = False

doallC :: HasCallStack => String -> Int -> String -> IO ()
doallC compiler verbosity file = do
  { output <- doallG compiler verbosity file
  ; putStrLn "Done"
  ; putStr output
  }

doall :: HasCallStack => Int -> String -> IO ()
doall = doallC "g++-7"

doallG :: HasCallStack => String -> Int -> String -> IO String
doallG compiler verbosity file =
  let dd defs = liftIO $ putStrLn ("...\n" ++ (pps $ take verbosity $! defs))
  in
  runKM $
  do { decls0 <- liftIO (parseF (file ++ ".ks"))
  ;  liftIO $ putStrLn "read decls"

  ;  let (main, decls)    = moveMain decls0

  ;  (env, ann_decls) <- annotDecls emptyGblST decls
  ;  let (rules, defs) = partitionDecls ann_decls
         rulebase      = mkRuleBase rules
  ; displayPass "Typechecked defs" env defs

  ;  banner "main"
  ;  dd main

  ;  banner "defs"
  ;  dd defs

  ;  let grad = gradDefs defs
         env1 = env `extendGblST` grad
  ;  displayPass "Grad" env1 grad

  ;  let (env2, optgrad) = optDefs rulebase env1 grad
  ;  displayPass "Optgrad" env2 optgrad

  ;  let fwd = applyDefs optgrad
  ;  displayPass "Fwd" env2 fwd

  ;  let (env3, optfwd) = optDefs rulebase env2 fwd
  ;  displayPass "OptFwd" env2 optfwd

  ; (env4, ann_main) <- annotDecls env3 main

  ;  let (_rules, main_tdef) = partitionDecls ann_main
         alldefs = defs ++ optgrad ++ optfwd ++ main_tdef
  ;  (env5, cse) <- cseDefs rulebase env4 alldefs
  ;  displayPass "CSE" env3 cse

  ;  let ann2 =  cse
  ;  liftIO (cppFG compiler ("obj/" ++ file) ann2)
  }

gmm :: IO ()
gmm = doall 400 "test/ksc/gmm"

main :: IO ()
main = gmm

hspec :: Spec
hspec = do
    Opt.hspec
    Lang.hspec
    LangUtils.hspec

test :: IO ()
test = do
  Test.Hspec.hspec Main.hspec

  System.Directory.createDirectoryIfMissing True "obj/test/ksc"
  output <- doallG "g++-7" 0 "test/ksc/gmm"

  let success = case reverse (lines output) of
        impossiblyGoodS:_:everythingWorksAsExpectedS:_ ->
          let boolOfIntString s = case s of
                "0" -> False
                "1" -> True
                _   -> error ("boolOfIntString: Unexpected " ++ s)

              impossiblyGood = boolOfIntString impossiblyGoodS
              everythingWorksAsExpected = boolOfIntString everythingWorksAsExpectedS
          in everythingWorksAsExpected && not impossiblyGood
        _ -> False

  if success
    then do
    putStrLn "Success"
    System.Exit.exitWith System.Exit.ExitSuccess
    else do
    putStrLn "FAILURE!"
    System.Exit.exitWith (System.Exit.ExitFailure 1)
