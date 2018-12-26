module Main where

import GHC.Stack;
import Data.Hashable

import Lang
import Parse (runParser, pDecls, parseF )
import Rules
import Annotate
import AD
import Opt
import CSE

import ANF
import Cgen (cppF, runM, cgenDef, cgenDefs)
import KMonad
import Text.PrettyPrint as PP
import Data.List( partition )
import Control.Monad( unless )


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
       ; disp "Optimised original definition" env1 opt_defs

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
       ; displayN (take verbosity $! decls)
       ; lintDefs what (env `extendGblST` decls) decls
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

doall :: HasCallStack => Int -> String -> IO ()
doall verbosity file =
  let dd defs = liftIO $ putStrLn ("...\n" ++ (pps $ take verbosity $! defs))
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
  ; displayPass verbosity "OptFwd" env2 optfwd

  ; (env4, ann_main) <- annotDecls env3 main

  ; let (_rules, main_tdef) = partitionDecls ann_main
  
  ; let alldefs = defs ++ optgrad ++ optfwd ++ main_tdef
  
  ; (env5, cse) <- cseDefs rulebase env4 alldefs
  ; displayPass verbosity "CSE" env3 cse

  ; let ann2 =  cse
  ; liftIO (cppF ("obj/" ++ file) ann2)
  }

gmm :: IO ()
gmm = doall 400 "test/ksc/gmm"

main :: IO ()
main = gmm
